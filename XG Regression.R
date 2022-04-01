smp_size <- floor(0.60 * nrow(passes_select))
set.seed(123)
ind <- sample(seq_len(nrow(passes_select)), size = smp_size)
train <- as.matrix(passes_select[ind, ])
test <- as.matrix(passes_select[-ind, ])

ayoe_model <-
  xgboost(
    data = train[, 2:17],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3,
    eta = .22
  )   

vip(ayoe_model)

xgb.plot.tree(model = ayoe_model, trees = 1)

pred_xgb <- predict(ayoe_model, test[, 2:17])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

hyper_grid <- expand.grid(max_depth = seq(3, 5, 1),
                          eta = seq(.2, .3, .01))
xgb_train_rmse <- NULL
xgb_test_rmse <- NULL

for (j in 1:nrow(hyper_grid)) {
  set.seed(123)
  m_xgb_untuned <- xgb.cv(
    data = train[, 2:15],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    nfold = 5,
    max_depth = hyper_grid$max_depth[j],
    eta = hyper_grid$eta[j]
  )
  
  xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  
  cat(j, "\n")
}

hyper_grid[which.min(xgb_test_rmse), ]

airyards_model <-
  xgboost(
    data = train[, 2:17],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 4, #ideal max depth
    eta = 0.2 #ideal eta
  )   

vip(airyards_model)

xgb.plot.tree(model = airyards_model, trees = 1)

pred_xgb <- predict(airyards_model, test[, 2:17])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

airyards_preds <- as.data.frame(
  matrix(predict(airyards_model, as.matrix(passes_select %>% select(-label))))
) %>%
  dplyr::rename(exp_airyards = V1)


ayoe_projs <- cbind(passing_pbp2, airyards_preds)

x = mean(ayoe_projs$pass_depth) - mean(ayoe_projs$exp_airyards)
x

ayoe_projs <- ayoe_projs %>%
  mutate(ayoe = pass_depth - exp_airyards - x)

passing_EPA <- passing_pbp_all %>%
  group_by(passer_name, season, offense) %>%
  summarize(
            EPA_play = mean(EPA, na.rm = T))

passer_seasons <- ayoe_projs %>%
  group_by(passer_name, season, offense) %>%
  summarize(passes = n(),
                   exp_airyards = mean(exp_airyards),
                   actual_airyards = mean(pass_depth),
                   avg_ayoe = mean(ayoe)) %>%
  dplyr::filter(passes >= 250) %>% 
  dplyr::arrange(desc(avg_ayoe)) %>%
  dplyr::group_by(passer_name) %>%
  arrange(season) %>% 
  dplyr::mutate(next_passes = lead(passes),
                next_avg_ayoe = lead(avg_ayoe),
                next_airyards = lead(actual_airyards)) %>%
  dplyr::arrange(desc(avg_ayoe)) %>%
  ungroup() %>% 
  left_join(teams_colors_logos, by = c("offense" = "team_abbr"))

passer_seasons <- left_join(passer_seasons, passing_EPA, by = c('passer_name', 'season', 'offense'))

colSums(is.na(passer_seasons))

ayoe_github <- ayoe_projs %>% 
  select(game_id, play_id, season, week, quarter, down, distance, passer_name,
         offense, defense, step_drop, yards, pressure, target_name, coverage_name,
         blitz, shotgun, pistol, play_action, mofo_coverage_played, mofo_coverage_shown,
         coverage_scheme, EP, EPA, exp_airyards, pass_depth, ayoe)

passing_pbpgithub <- passing_pbp_all %>% 
  select(game_id, play_id, season, week, quarter, down, distance, passer_name,
         offense, defense, step_drop, yards, pressure, target_name, coverage_name,
         blitz, shotgun, pistol, play_action, mofo_coverage_played, mofo_coverage_shown,
         coverage_scheme, EP, EPA)


write_csv(ayoe_github, "ayoe_projs.csv")
write_csv(passing_pbpgithub, "passing_pbp_all.csv")

passer_seasons %>% 
  ggplot()+
  ggrepel::geom_text_repel(aes(x = EPA_play, y = avg_ayoe, label = passer_name),
                            box.padding = 0.3, size = 5)+
  geom_point(aes(x = EPA_play, y = avg_ayoe, size = passes, 
                 fill = team_color, color = team_color2), shape = 21)+
  geom_hline(yintercept = mean(passer_seasons$avg_ayoe), color = "black", linetype = "dashed", alpha=0.7)+
  geom_vline(xintercept =  mean(passer_seasons$EPA_play), color = "black", linetype = "dashed", alpha=0.7)+
  scale_color_identity(aesthetics =  c("fill", "color")) +
  scale_size(name = "Attempts") +
  theme_fivethirtyeight()+
  labs(title = paste0("Air Yards Over Expected and EPA"),
        subtitle = paste0("Minimum of attempts in the time period"),
        caption = "By Arjun Menon | @arjunmenon100 | PFF")+
  theme(axis.title = element_text(size = 18)) + ylab('Air Yards Over Expected Per Pass') + xlab("EPA/Pass")+
  theme(panel.grid.minor=element_blank(),
        legend.position = 'none')+
  theme(axis.text = element_text(size = 17))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))

ayoe_projs %>% 
  filter(passer_name %in% c("Patrick Mahomes", 'Aaron Rodgers', 'Tom Brady')) %>% 
  filter(season == 2021) %>% 
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
    subtitle = paste0(input$range_years[1], "-", input$range_years[2], ", weeks ", input$week_range[1], "-", input$week_range[2]),
    caption = "By Arjun Menon | @arjunmenon | PFF"
  ) 
  

passer_seasons %>% 
  select(avg_ayoe, EPA_play) %>% 
  cor(use = "complete.obs") %>% 
  round(2)

table(passer_seasons$passer_name, passer_seasons$exp_airyards, passer_seasons$actual_airyards)

brady <- passer_seasons %>% filter(passer_name == 'Tom Brady')
passer_seasons %>%
ggplot(aes(x = season, y = avg_ayoe))+
  geom_jitter(color = "black", width = 0.05, alpha = 0.25, size = 2.5)+
  geom_line(data = brady, mapping = aes(x = season, y = avg_ayoe), color = brady$team_color2)+
  geom_point(data = brady, mapping = aes(x = season, y = avg_ayoe), fill = brady$team_color2, color = brady$team_color, shape = 21, size = 5.5)+
  theme_fivethirtyeight()+
  labs(title = "Brady AYOE since 2013",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: PFF',
       subtitle = glue::glue(""))+
  theme(axis.title = element_text(size = 16)) + xlab("Season") + ylab("Air Yards over Expected (AYOE)")+
  theme(axis.text = element_text(size = 14))+
  theme(legend.position = "none")+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=6))
ggsave('specialteams.png', width = 14, height = 10, dpi = "retina")

