library(ggplot2)
library(dplyr)
library(DT)

source('static_variables.R')
source('plotting_utilities.R')
source('esv_prep_utilities.R')
source('esv_calc_utilities.R')
source('all_data_prep.R')

# Data Collection  ----

points <- shot_df_all %>%
  filter(!(plc %in% c('1S') & strike_significance %in% c('out_of_bounds', 'net'))) %>%
  pull(internal_point_id) %>%
  unique()

esv_df <- matrix(NA, nrow = 0, ncol = 4)
colnames(esv_df) <- c('esv_striker', 'striker_won_point', 'esv_returner', 'returner_won_point')
for(p in points){
  
  print(
    sprintf("Starting fitting for point %s, %d/%d at %s", p, which(points == p), length(points), Sys.time()))

  df <- esv_validation(shot_df = shot_df_all, match_to_filter_out = p)
  df <- df[,c('esv_striker', 'striker_won_point', 'esv_returner', 'returner_won_point')]
  
  esv_df <- rbind(esv_df, df)
  
}

# ----

# Plotting Results ----

seq_by = .1
seq_min = 0
seq_max = 1
bins <- seq(seq_min, seq_max, by = seq_by)
mid_pt <- seq(seq_min + seq_by/2, by = seq_by, length = length(bins)-1)
bins[1] <- -0.1

tot_esv_df <- data.frame(
  esv = c(esv_df[,'esv_striker'], esv_df[,'esv_returner']),
  wins = c(esv_df[,'striker_won_point'], esv_df[,'returner_won_point'])
)
# tot_esv_df <- tot_esv_df %>% filter(esv < 1, esv > 0)
plot_df <- data.frame(esv = mid_pt, win_pct = 0, n = 0)

for(i in 1:nrow(plot_df)){
  
  min_esv <- bins[i]
  max_esv <- bins[i+1]
  plot_df[i, c('win_pct', 'n')] <- tot_esv_df %>%
    filter(esv >= min_esv, esv < max_esv) %>%
    summarise(win_pct = sum(wins)/n(),
              n = n())
}

plot_df <- plot_df[complete.cases(plot_df),]

linear_model_fit <- lm(win_pct ~ esv, data = plot_df)
coeffs <- linear_model_fit$coefficients
r_squared <- summary(linear_model_fit)$r.squared

g <- ggplot() +
  geom_path(data = data.frame(x = plot_df$esv,
                              y = {plot_df$esv*coeffs[2] + coeffs[1]}), 
            mapping =  aes(x = x, y = y)) +
  geom_point(data = plot_df, mapping = aes(esv, win_pct, size = n), color = '#0047AB') +
  geom_text(data = data.frame(x = 0.25,
                              y = 0.75,
                              txt = paste0('R-Squared = ', round(r_squared,4), '\n',
                                           'Win Pct = ',
                                           round(coeffs[2], 3), '*ESV  + ', round(coeffs[1],3))),
            mapping = aes(x = x, y = y, label = txt)) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = bins,
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1),
                     expand = c(0,0)) +
  xlab('ESV') + ylab('Winning Percentage') + labs(size = 'Number of Shots') +
  theme(legend.justification=c(1,0), legend.position=c(1,0))




# ----


