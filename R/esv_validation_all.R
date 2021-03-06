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
  filter(n1 >= 4, n2 >= 4) %>%
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
# colnames(esv_df) <- c('esv', 'wins')
for(p in points){
  
  print(
    sprintf("Starting fitting for point %s, %d/%d at %s", p, which(points == p), length(points), Sys.time()))

  df <- esv_validation(shot_df = shot_df_all, match_to_filter_out = p)
  # df <- data.frame(
  #   esv = c(df[,'esv_striker'], df[,'esv_returner']),
  #   wins = c(df[,'striker_won_point'], df[,'returner_won_point']))
  # df <- df[df$esv == max(df$esv) | df$esv == min(df$esv),]
  df <- df[,colnames(esv_df)]
  esv_df <- rbind(esv_df, df)
  
}

# ----

# Plotting Results ----

seq_by <- .1
seq_min <- 0.05
seq_max <- .95
bins <- c(0, seq(seq_min, seq_max, by = seq_by), 1.01)

# tot_esv_df <- esv_df
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

g <- ggplot() +
  
  geom_hline(yintercept = 0.5, alpha = 0.5, color = 'grey') +
  
  geom_path(
    data = data.frame(x = plot_df$mid_pt, y = plot_df$mid_pt*coeffs[2] + coeffs[1])
    ,mapping =  aes(x = x, y = y)
    ,color = 'gray40') +
  
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
    data = data.frame(
      x = c(.75, .8, .85, .9)
      ,y = rep(.1, 4)
      ,size = c(max(plot_df$n)*0.1, max(plot_df$n)*0.4, max(plot_df$n)*0.7, max(plot_df$n)))
    ,mapping = aes(x, y, size = size)
    ,color = '#0047AB') +
  
  geom_text(
    data = data.frame(x = c(.75, .9), y = rep(.1, 2), label = c('LESS ESV VALUES', 'MORE ESV VALUES'))
    ,mapping = aes(x, y, label = label)
    ,size = 2.5, nudge_y = 0.05) +
  
  scale_x_continuous(limits = c(0, 1), breaks = c(0, seq(0.05, 0.95, .1), 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .25)) +
  xlab('ESV') + ylab('Win Rate') + 
  guides(size = F) +
  theme_minimal() +
  theme(
    legend.justification = c(1, 0)
    ,legend.position = c(1, 0)
    ,plot.margin = unit(c(0, 0, 0, 0), "mm")
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank())

ggsave('../plots/win_percentage_plot_original_weights_all.jpg', g, height = 150, width = 200, unit = 'mm')

# ----


