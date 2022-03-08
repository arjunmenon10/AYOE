#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(nflfastR)
library(ggplot2)
library(ggimage)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(gt)
library(paletteer)
library(webshot)
library(ggthemes)
library(readr)
library(ggtext)
library(ggforce)
library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(na.tools)
library(stringr)
library(magick)
library(ggbeeswarm)
library(vip)
library(gtExtras)


ayoe_github <- read_csv(url("https://raw.githubusercontent.com/arjunmenon10/AYOE/main/ayoe_projs.csv"))

passing_pbp <- read_csv(url("https://raw.githubusercontent.com/arjunmenon10/AYOE/main/passing_pbp_all.csv"))

ayoe_github <- ayoe_github %>%
  group_by(passer_name) %>%
  mutate(count = n()) %>%
  filter(count >= 50) %>%
  ungroup()

passing_pbp <- passing_pbp %>% 
  group_by(passer_name) %>%
  mutate(count = n()) %>%
  filter(count >= 50) %>%
  ungroup()

teams <- nflfastR::teams_colors_logos %>%
  filter(!team_abbr %in% c("LAR", "SD", "STL", "OAK"))

ids <- teams %>%
  pull(team_abbr)

passers <- unique(ayoe_github$passer_name)
seasons <- unique(ayoe_github$season)

options(shiny.usecairo = T)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Air Yards over Expected"),
    
    mainPanel(
      navbarPage("By Arjun Menon at PFF",
                 tabPanel("By Season",
                          fluidRow(
                            column(4, align = "center",
                                   sliderInput("year_range", "Year Range", value = c(2020:2021), min = 2013, max = 2021,
                                               sep = ""),
                                   ),
                            column(7, align = "center",
                                   sliderInput("min_attempts", "Minimum Attempts", value = 250, min = 100, max = 6000),
                                   ),
                            mainPanel(
                              plotOutput(outputId = "passer_graph",
                                      width = "100%",
                                      height = "50%"),
                            br(),
                            tableOutput("passer_table"),
                            br()
                          )
                          )
                 ),
      tabPanel('Quarterback Comparison',
               fluidRow(
                 column(7, align = "center",
                        selectInput("player_1", "Player 1", c(sort(unique(as.character(passers)))), selected = "Tom Brady"),
                        selectInput("player_2", "Player 2", c(sort(unique(as.character(passers)))), selected = "Patrick Mahomes"),
                        selectInput("player_3", "Player 3", c(sort(unique(as.character(passers)))), selected = "Aaron Rodgers"),),
                 column(4, align = "center",
                        selectInput(
                          inputId =  "Season",
                          label = "Season:",
                          choices = 2013:2021,
                          selected = 2021
                        ),
                                     sliderInput("week_range", "Weeks Range", value = c(1, 18), min = 1, max = 18),),
                        mainPanel(
                          plotOutput(outputId = "beeswarm_graph",
                                     width = "750px", height = "500px"),
                          br(),
                          br(),
                          br(),
                          tableOutput(outputId = "player_comp_tab"),
                          br()
                        ),
                        )
                 ),
      tabPanel('Yearly AYOE',
               fluidRow(
                 column(6, align = 'center',
                        selectInput("player", "Player", c(sort(unique(as.character(passers)))), selected = "Tom Brady"),),
                mainPanel(
                  plotOutput(outputId = "YearoverYear",
                             width = "750px", height = "500px"),
                  br(),
                  br(),
                  br(),
                  tableOutput(outputId = "YearoverYear_tab"),
                  br()
                )
               )
               )
              
      )
    )
)

server <- function(input, output){
  
  output$passer_graph <- renderPlot({
    
    passes_season <- ayoe_github %>% 
      filter(season >= input$year_range[1] & season <= input$year_range[2])
    
    EPA_season <- passing_pbp %>% 
      filter(season >= input$year_range[1] & season <= input$year_range[2])
    
    qb_colors <- passes_season %>% 
      group_by(passer_name, offense) %>% 
      summarise(plays = n()) %>% 
      arrange(-plays) %>% 
      group_by(passer_name) %>% 
      top_n(n = 1) %>% 
      left_join(teams_colors_logos, by = c('offense' = 'team_abbr'))
    
    qb_ayoe <- passes_season %>%
      filter(!is.na(passer_name)) %>%
      group_by(passer_name) %>%
      summarize(attempts = n(),
                actual_airyards = mean(pass_depth, na.rm = T),
                expec_airyards = mean(exp_airyards, na.rm = T),
                avg_ayoe = mean(ayoe, na.rm = T))
    
    qbEPA_season <- EPA_season %>% 
      filter(!is.na(EPA), !is.na(passer_name)) %>% 
      group_by(passer_name) %>% 
      summarise(EPA_play = mean(EPA, na.rm = T))
    
    qb_season <- left_join(qb_ayoe, qbEPA_season, by = 'passer_name') 
  
  qb_season <- qb_season %>%
    filter(attempts > as.numeric(input$min_attempts))
  
  qb_season <- qb_season %>%
    left_join(qb_colors, by = c("passer_name"))
  
  qb_season %>% 
    ggplot()+
    ggrepel::geom_text_repel(aes(x = EPA_play, y = avg_ayoe, label = passer_name),
                             box.padding = 0.3, size = 5)+
    geom_point(aes(x = EPA_play, y = avg_ayoe, size = attempts, 
                   fill = team_color, color = team_color2), shape = 21)+
    geom_hline(yintercept = mean(qb_season$avg_ayoe), color = "black", linetype = "dashed", alpha=0.7)+
    geom_vline(xintercept =  mean(qb_season$EPA_play), color = "black", linetype = "dashed", alpha=0.7)+
    scale_color_identity(aesthetics =  c("fill", "color")) +
    scale_size(name = "Attempts") +
    theme_fivethirtyeight()+
    labs( title = paste0("Air Yards Over Expected and EPA/Pass, ", input$year_range[1], "-", input$year_range[2]),
          subtitle = paste0("Minimum of ", input$min_attempts," attempts in the time period"),
          caption = "By Arjun Menon | @arjunmenon100 | PFF")+
    theme(axis.title = element_text(size = 18)) + ylab('Air Yards Over Expected Per Pass') + xlab("EPA/Pass")+
    theme(panel.grid.minor=element_blank(),
          legend.position = 'none')+
    theme(axis.text = element_text(size = 17))+
    theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
          plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
  },  height = 600, width = 850)
  
  output$passer_table <- render_gt({
    
    passes_season <- ayoe_github %>% 
      filter(season >= input$year_range[1] & season <= input$year_range[2])
    
    EPA_season <- passing_pbp %>% 
      filter(season >= input$year_range[1] & season <= input$year_range[2])
    
    qb_colors <- passes_season %>% 
      group_by(passer_name, offense) %>% 
      summarise(plays = n()) %>% 
      arrange(-plays) %>% 
      group_by(passer_name) %>% 
      top_n(n = 1) %>% 
      left_join(teams_colors_logos, by = c('offense' = 'team_abbr'))
    
    qb_ayoe <- passes_season %>%
      filter(!is.na(passer_name)) %>%
      group_by(passer_name) %>%
      summarize(attempts = n(),
                actual_airyards = mean(pass_depth, na.rm = T),
                expec_airyards = mean(exp_airyards, na.rm = T),
                avg_ayoe = mean(ayoe, na.rm = T))
    
    qbEPA_season <- EPA_season %>% 
      filter(!is.na(EPA), !is.na(passer_name)) %>% 
      group_by(passer_name) %>% 
      summarise(EPA_play = mean(EPA, na.rm = T))
    
    qb_season <- left_join(qb_ayoe, qbEPA_season, by = 'passer_name')
    
    qb_season <- qb_season %>%
      filter(attempts > as.numeric(input$min_attempts))
    
    qb_season <- qb_season %>%
      left_join(qb_colors, by = c("passer_name"))
    
    qb_gt <- qb_season %>% 
      select(passer_name, team_logo_espn, attempts, EPA_play, actual_airyards,
             expec_airyards, avg_ayoe) %>% 
      mutate_if(is.numeric, ~round(., 2)) %>%
      arrange(-avg_ayoe) %>%
      ungroup() %>%
      mutate(rank = row_number()) %>%
      dplyr::select(rank, everything())
    
    qb_gt %>% 
      gt() %>%
      text_transform(
        locations = cells_body(c(team_logo_espn)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>%
      cols_label(
        rank = "Rank",
        passer_name = "Player",
        team_logo_espn = "Team",
        attempts = "Attempts",
        EPA_play = "EPA/Pass",
        actual_airyards = "Air Yards",
        expec_airyards = "Expected Air Yards",
        avg_ayoe = "Air Yards Over Expected") %>%
      data_color(
        columns = c(avg_ayoe),
        colors = scales::col_numeric(
          palette = c("#cf3e53", "#bfb202", "#00a2b3"),
          domain = NULL
        )
      ) %>%
      opt_align_table_header(align = "center") %>%
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538()
  }, width = 850)
  
  output$beeswarm_graph <- renderPlot({
    
    passers_needed <- c(input$player_1, input$player_2, input$player_3)
    
    filtered_pbp <- ayoe_github %>% 
      filter(!is.na(ayoe)) %>% 
      filter(passer_name %in% passers_needed) %>%
      filter(season == input$Season) %>%
      filter(week >= input$week_range[1] & week <= input$week_range[2])
    
    filtered_pbp %>% 
      ggplot(aes(x = passer_name, y = ayoe, fill = ayoe)) + 
      geom_quasirandom(pch = 21, size = 5) + 
      scale_fill_viridis_b() +
      theme_fivethirtyeight() +
      geom_hline(yintercept = 0, color = "black", alpha=1.0) +
      theme(axis.title = element_text(size = 18)) + ylab('AYOE') + xlab("Quarterback")+
      theme(panel.grid.minor=element_blank(),
            legend.position = 'none')+
      theme(axis.text = element_text(size = 17))+
      theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
            plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
      labs(
        title = paste0("Every QB's single Air Yards Over Expected"),
        subtitle = paste0(input$Season, ", weeks ", input$week_range[1], "-", input$week_range[2]),
        caption = "By Arjun Menon | @arjunmenon | PFF"
      ) 
  }, height = 550, width = 800)
  
  output$player_comp_tab <- render_gt({
    
    filtered_pbp <- ayoe_github %>% 
      filter(season == input$Season)
    
    EPA_season <- passing_pbp %>% 
      filter(season >= input$year_range[1] & season <= input$year_range[2])
    
    passers_needed <- c(input$player_1, input$player_2, input$player_3)
    
    filtered_pbp2 <- filtered_pbp %>% 
      filter(!is.na(ayoe)) %>% 
      filter(passer_name %in% passers_needed) %>%
      filter(season == input$Season) %>%
      filter(week >= input$week_range[1] & week <= input$week_range[2])
    
    qb_logos <- filtered_pbp2 %>%
      group_by(passer_name, offense) %>%
      summarize(count = n()) %>%
      arrange(passer_name, count) %>%
      top_n(1) %>%
      left_join(teams_colors_logos, by = c("offense" = "team_abbr"))
    
    qb_ayoe <- filtered_pbp2 %>%
      filter(!is.na(passer_name)) %>%
      group_by(passer_name) %>%
      summarize(attempts = n(),
                actual_airyards = mean(pass_depth, na.rm = T),
                expec_airyards = mean(exp_airyards, na.rm = T),
                avg_ayoe = mean(ayoe, na.rm = T))
    
    qbEPA_season <- EPA_season %>% 
      filter(!is.na(EPA), !is.na(passer_name)) %>% 
      group_by(passer_name) %>% 
      summarise(EPA_play = mean(EPA, na.rm = T))
    
    qb_tab <- left_join(qb_ayoe, qbEPA_season, by = 'passer_name')
    
    qb_tab <- qb_tab %>%
      left_join(qb_logos, by = "passer_name")
    
    qb_tab <- qb_tab %>%
      arrange(desc(avg_ayoe)) %>%
      mutate(rank = row_number()) %>%
      select(rank, passer_name, team_logo_espn, attempts, EPA_play, actual_airyards, expec_airyards, avg_ayoe)
    
    qb_tab <- qb_tab %>%
      arrange(rank) %>%
      mutate_if(is.numeric, ~round(., 2))
    
    qb_tab %>% 
      gt() %>%
      text_transform(
        locations = cells_body(c(team_logo_espn)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>%
      cols_label(
        rank = "Rank",
        passer_name = "Player",
        team_logo_espn = "Team",
        attempts = "Attempts",
        EPA_play = "EPA/Pass",
        actual_airyards = "Air Yards",
        expec_airyards = "Expected Air Yards",
        avg_ayoe = "Air Yards Over Expected") %>%
      data_color(
        columns = c(avg_ayoe),
        colors = scales::col_numeric(
          palette = c("#cf3e53", "#bfb202", "#00a2b3"),
          domain = NULL
        )
      ) %>%
      opt_align_table_header(align = "center") %>%
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538()
    
  }, width = 800)
  
  output$YearoverYear <- renderPlot({
    
    passers_needed <- input$player
    
    passes_season <- ayoe_github %>% 
      filter(passer_name == passers_needed)
    
    passes_season2 <- ayoe_github
    
    
    EPA_season <- passing_pbp %>% 
      filter(passer_name == passers_needed)
    
    qb_colors <- passes_season %>% 
      group_by(passer_name, offense, season) %>% 
      summarise(plays = n()) %>% 
      arrange(-plays) %>% 
      group_by(passer_name, season) %>% 
      top_n(n = 1) %>% 
      left_join(teams_colors_logos, by = c('offense' = 'team_abbr'))
    
    qb_ayoe <- passes_season %>%
      filter(!is.na(passer_name)) %>%
      group_by(passer_name, season) %>%
      summarize(attempts = n(),
                actual_airyards = mean(pass_depth, na.rm = T),
                expec_airyards = mean(exp_airyards, na.rm = T),
                avg_ayoe = mean(ayoe, na.rm = T)) %>% 
      filter(attempts > 50)
    
    qb_ayoe2 <- passes_season2 %>%
      filter(!is.na(passer_name)) %>%
      group_by(passer_name, season) %>%
      summarize(attempts = n(),
                actual_airyards = mean(pass_depth, na.rm = T),
                expec_airyards = mean(exp_airyards, na.rm = T),
                avg_ayoe = mean(ayoe, na.rm = T)) %>% 
      filter(attempts > 50)
    
    qbEPA_season <- EPA_season %>% 
      filter(!is.na(EPA), !is.na(passer_name)) %>% 
      group_by(passer_name, season) %>% 
      summarise(EPA_play = mean(EPA, na.rm = T))
    
    qb_season <- left_join(qb_ayoe, qbEPA_season, by = c('passer_name', 'season'))
    
    qb_season <- qb_season %>%
      left_join(qb_colors, by = c("passer_name", 'season'))
    
    qb_ayoe2 <- qb_ayoe2 %>% 
      filter(season %in% qb_season$season)
    
    test <- passes_season %>%
      group_by(passer_name, offense, season) %>% 
      summarise(plays = n()) %>% 
      filter(plays > 50) %>% 
      mutate(seasons_played = n())
    
    qb_ayoe2 %>%
      ggplot(aes(x = season, y = avg_ayoe))+
      geom_jitter(color = "black", width = 0.05, alpha = 0.25, size = 2.5)+
      geom_line(data = qb_season, mapping = aes(x = season, y = avg_ayoe), color = qb_season$team_color2, size = 2)+
      geom_image(data = qb_season, mapping = aes(x = season, y = avg_ayoe), image = qb_season$team_logo_espn, asp = 16/9, size = 0.0625)+
      theme_fivethirtyeight()+
      labs(title = paste0(input$player, " AYOE Year over Year"),
           caption = 'By: Arjun Menon | @arjunmenon100 | Data: PFF',
           subtitle = glue::glue(""))+
      theme(axis.title = element_text(size = 18)) + xlab("Season") + ylab("Air Yards over Expected (AYOE)")+
      theme(axis.text = element_text(size = 17))+
      theme(legend.position = "none")+
      theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
            plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 9))+
      scale_x_continuous(breaks = scales::pretty_breaks(n = test$seasons_played))
  }, height = 550, width = 800)
  
  output$YearoverYear_tab <- render_gt({
    
    passers_needed <- input$player
    
    passes_season <- ayoe_github %>% 
      filter(passer_name == passers_needed)
    
    EPA_season <- passing_pbp %>% 
      filter(passer_name == passers_needed)
    
    qb_colors <- passes_season %>% 
      group_by(passer_name, offense, season) %>% 
      summarise(plays = n()) %>% 
      arrange(-plays) %>% 
      group_by(passer_name, season) %>% 
      top_n(n = 1) %>% 
      left_join(teams_colors_logos, by = c('offense' = 'team_abbr'))
    
    qb_ayoe <- passes_season %>%
      filter(!is.na(passer_name)) %>%
      group_by(passer_name, season) %>%
      summarize(attempts = n(),
                actual_airyards = mean(pass_depth, na.rm = T),
                expec_airyards = mean(exp_airyards, na.rm = T),
                avg_ayoe = mean(ayoe, na.rm = T)) %>% 
      filter(attempts > 50)
    
    qbEPA_season <- EPA_season %>% 
      filter(!is.na(EPA), !is.na(passer_name)) %>% 
      group_by(passer_name, season) %>% 
      summarise(EPA_play = mean(EPA, na.rm = T))
    
    qb_season <- left_join(qb_ayoe, qbEPA_season, by = c('passer_name', 'season')) 
    
    qb_season <- qb_season %>%
      left_join(qb_colors, by = c("passer_name", 'season'))
    
    qb_tab <- qb_season %>%
      arrange(season) %>%
      select(season, passer_name, team_logo_espn, EPA_play, actual_airyards, expec_airyards, avg_ayoe)
    
    qb_tab <- qb_tab %>%
      arrange(season) %>%
      mutate(season = as.character(season), 
             passer_name = as.character(passer_name)) %>% 
      mutate_if(is.numeric, ~round(., 2)) %>% 
      ungroup()
    
    qb_tab %>% 
      select(season, passer_name, team_logo_espn, EPA_play, actual_airyards, expec_airyards, avg_ayoe) %>% 
      gt() %>%
      text_transform(
        locations = cells_body(c(team_logo_espn)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>%
      cols_label(
        passer_name = "Player",
        team_logo_espn = "Team",
        EPA_play = "EPA/Pass",
        actual_airyards = "Air Yards",
        expec_airyards = "Expected Air Yards",
        avg_ayoe = "Air Yards Over Expected") %>%
      data_color(
        columns = c(avg_ayoe),
        colors = scales::col_numeric(
          palette = c("white", "#32a251"),
          domain = NULL
        )
      ) %>%
      opt_align_table_header(align = "center") %>%
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538()
  }, width = 800)
  
}

shinyApp(ui = ui, server = server)