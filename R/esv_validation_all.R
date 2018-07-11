library(ggplot2)
library(dplyr)
library(DT)

source('static_variables.R')
source('plotting_utilities.R')
source('esv_prep_utilities.R')
source('esv_calc_utilities.R')
source('all_data_prep.R')

match_summaries <- shot_df_all %>%
  select(match_code) %>%
  unique() %>%
  left_join(
    shot_df_all %>%
      group_by(match_code) %>%
      summarise(p1 = unique(striker_id)[1], p2 = unique(striker_id)[2]),
    by = 'match_code') %>%
  left_join(
    shot_df_all %>%
      group_by(striker_id) %>%
      summarise(n1 = length(unique(match_code))),
    by = c('p1' = 'striker_id')) %>%
  left_join(
    shot_df_all %>%
      group_by(striker_id) %>%
      summarise(n2 = length(unique(match_code))),
    by = c('p2' = 'striker_id')) %>%
  filter(n1 >= 3, n2 >= 3) %>%
  arrange(desc(n1), desc(n2))
  
# Data Collection  ----

matches_in_question <- match_summaries$match_code

points <- shot_df_all %>%
  filter(!(plc %in% c('1S') & strike_significance %in% c('out_of_bounds', 'net'))) %>%
  filter(match_code %in% matches_in_question) %>%
  pull(internal_point_id) %>%
  unique()

esv_df <- matrix(NA, nrow = 0, ncol = 4)
colnames(esv_df) <- c('esv_striker', 'striker_won_point', 'esv_returner', 'returner_won_point')
for(p in points){
  
  print(
    sprintf("Starting fitting for point %s, %d/%d at %s", p, which(points == p), length(points), Sys.time()))

  df <- esv_validation(shot_df = shot_df_all, match_to_filter_out = p)
  df <- df[,colnames(esv_df)]
  
  esv_df <- rbind(esv_df, df)
  
}

# ----

# Plotting Results ----

seq_by <- .1
seq_min <- 0.05
seq_max <- .95
bins <- c(0, seq(seq_min, seq_max, by = seq_by), 1)

tot_esv_df <- data.frame(
  esv = c(esv_df[,'esv_striker'], esv_df[,'esv_returner']),
  wins = c(esv_df[,'striker_won_point'], esv_df[,'returner_won_point']))

tot_esv_df$bin_number <- cut(tot_esv_df$esv, bins, include.lowest = T)
plot_df <- tot_esv_df %>%
  group_by(bin_number) %>%
  summarise(win_pct = sum(wins)/n(), n = n(), wins = sum(wins)) %>%
  mutate(mid_pt = zoo::rollapply(bins, 2, mean, align = 'left', fill = c(mean(c(bins[1], bins[2]))))[1:n()])

plot_df <- plot_df[complete.cases(plot_df),]

linear_model_fit <- lm(win_pct ~ mid_pt, data = plot_df)
coeffs <- linear_model_fit$coefficients
r_squared <- summary(linear_model_fit)$r.squared

ggplot() +
  
  geom_path(
    data = data.frame(x = plot_df$mid_pt, y = plot_df$mid_pt*coeffs[2] + coeffs[1])
    ,mapping =  aes(x = x, y = y)) +
  
  geom_point(
    data = plot_df
    ,mapping = aes(x = mid_pt, y = win_pct, size = n)
    ,color = '#0047AB') +
  
  geom_text(
    data = data.frame(
      x = 0.25
      ,y = 0.75
      ,txt = paste0(
        'R-Squared = ', round(r_squared,4), '\n',
        'Win Rate = ', round(coeffs[2], 3), '*ESV  + ', round(coeffs[1],3)))
    ,mapping = aes(x = x, y = y, label = txt)) +
  
  geom_point(
    data = data.frame(x = c(.75, .8, .85, .9), y = rep(.1, 4), size = c(1000, 3000, 5000, 7000))
    ,mapping = aes(x, y, size = size)
    ,color = '#0047AB') +
  
  geom_text(
    data = data.frame(x = c(.75, .9), y = rep(.1, 2), label = c('LESS ESV VALUES', 'MORE ESV VALUES'))
    ,mapping = aes(x, y, label = label)
    ,size = 2.5, nudge_y = 0.05) +
  
  scale_x_continuous(limits = c(0, 1), breaks = bins) +#, expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  xlab('ESV') + ylab('Win Rate') + 
  guides(size = F) +
  theme_minimal() +
  theme(
    legend.justification = c(1, 0)
    ,legend.position = c(1, 0)
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank())

# ----


