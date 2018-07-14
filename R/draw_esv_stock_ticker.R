
library(ggplot2)
library(ggrepel)
library(dplyr)
library(DT)

options(stringsAsFactors = F)

source('static_variables.R')
source('plotting_utilities.R')
source('esv_prep_utilities.R')
source('esv_calc_utilities.R')
source('sample_data_prep.R')
source('../../esv_hidden_variables.R')

pt <- match_code_hidden
esv_pt_df <- esv_validation(shot_df = shot_df_sample, match_to_filter_out = pt) 

player_1_id <- unique(esv_pt_df$striker_id)[1]
player_2_id <- unique(esv_pt_df$striker_id)[2]

p1_esv <- esv_pt_df %>%
  mutate(
    esv = ifelse(striker_id == player_1_id, as.numeric(esv_striker), as.numeric(esv_returner)),
    s_or_r = ifelse(striker_id == player_1_id, 'Strike', 'Return'),
    time = as.numeric(time)) %>%
  select(esv, s_or_r, time, plc)

p1_esv_plot <- data.frame(
  x = c(rbind(p1_esv$time, p1_esv$time %>% lead(1))),
  y = rep(p1_esv$esv, each = 2))

p1_esv_plot <- p1_esv_plot %>% filter(!is.na(x))
p1_esv_plot$plyr <- 'Player 1'

p2_esv <- esv_pt_df %>%
  mutate(
    esv = ifelse(striker_id == player_2_id, as.numeric(esv_striker), as.numeric(esv_returner)),
    s_or_r = ifelse(striker_id == player_2_id, 'Strike', 'Return'),
    time = as.numeric(time)) %>%
  select(esv, s_or_r, time, plc)

p2_esv_plot <- data.frame(
  x = c(rbind(p2_esv$time, p2_esv$time %>% lead(1))),
  y = rep(p2_esv$esv, each = 2))

p2_esv_plot <- p2_esv_plot %>% filter(!is.na(x))
p2_esv_plot$plyr <- 'Player 2'

p1p2_esv <- rbind(
  mutate(p1_esv, plyr = 'Player 1')
  ,mutate(p2_esv, plyr = 'Player 2'))

point_legend_df <- data.frame(
  x = c(0.1, 0.1),
  y = c(.975, 0.925),
  label = c('Strike', 'Return'))

color_legend_df <- data.frame(
  x = c(0.05, 0.05),
  xend = c(0.3, 0.3),
  y = c(.075, .025),
  yend = c(.075, .025),
  plyr = c('Player 1','Player 2'))

g <- ggplot() +
  
  geom_hline(yintercept = 0.5, linetype = 1, color = 'gray70', size = 0.5, alpha = 0.25) +
  
  geom_line(data = p1_esv_plot,
            mapping = aes(x = x, y = y),
            colour = 'blue', linetype = 1, alpha = 0.5) +
  
  geom_point(data = p1_esv,
             mapping = aes(x = time, y = esv, shape = s_or_r),
             colour = 'blue', size = 3, alpha = 0.75) +
  
  geom_line(data = p2_esv_plot,
            mapping = aes(x = x, y = y),
            colour = 'red', linetype = 2, alpha = 0.5) +
  
  geom_point(data = p2_esv,
             mapping = aes(x = time, y = esv, shape = s_or_r),
             colour = 'red', size = 3, alpha = 0.75) +
  
  geom_text_repel(data = p1p2_esv,
            mapping = aes(x = time, y = esv, label = plc, group = plyr, color = plyr),
            size = 3, force = 10) +
  
  geom_point(data = point_legend_df,
             mapping = aes(x = x, y = y, shape = label), 
             color = 'gray40', size = 3) +
  
  geom_text(data = point_legend_df,
            mapping = aes(x = x, y = y, label = label), 
            color = 'gray40', size = 4, hjust = -0.25, vjust = 0.55) +
  
  geom_segment(data = color_legend_df,
               mapping = aes(x = x, y = y,
                             xend = xend, yend = yend,
                             group = plyr, colour = plyr, linetype = plyr)) +
  
  geom_text(data = color_legend_df,
            mapping = aes(x = xend, y = yend, label = plyr), 
            color = 'gray40', size = 4, hjust = -0.1, vjust = 0.55) +
  
  scale_colour_manual(values = c('Player 1' = 'blue', 'Player 2' = 'red')) +
  scale_shape_manual(values = c('Strike' = 19, 'Return' = 1)) +
  scale_linetype_manual(values = c('Player 1' = 1, 'Player 2' = 2)) +
  xlim(c(0, max(p1_esv$time)+1)) + ylim(0,1) +
  guides(fill = F, size = F, alpha = F, colour = F, shape = F, linetype = F) +
  xlab('Time (s)') + ylab('ESV') +
  theme_minimal() +
  theme(
    legend.justification = c(1, 0)
    ,legend.position = c(1, 0)
    ,plot.margin = unit(c(0, 0, 0, 0), "mm")
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank())

ggsave('../plots/esv_stock_ticker.jpg', g, height = 150, width = 200, unit = 'mm')

# Analysis of Player Positioning Decisions ----

avg_swr <- shot_df_sample %>%
  filter(!(plc == '1S' & strike_significance == 'out_of_bounds')) %>%
  count(strike_significance) %>%
  mutate(weight = weights_df$weight[match(strike_significance, weights_df$significance)]) %>%
  summarise(x = sum(n*weight)/sum(n)) %>%
  pull()

avg_rwr <- shot_df_sample %>%
  filter(
    !(plc == '1S' & strike_significance == 'out_of_bounds'), return_significance != 'insignificant') %>%
  count(return_significance) %>%
  mutate(weight = weights_df$weight[match(return_significance, weights_df$significance)]) %>%
  summarise(x = sum(n*weight)/sum(n)) %>%
  pull()

# Strike Win Rate Calculation ----
swr_df <- swr_calc(shot_df = shot_df_sample, matches_to_filter_out = pt, avg = F)
avg_swr_df <- swr_calc(shot_df = shot_df_sample, matches_to_filter_out = pt, avg = T)
avg_swr_df <- cbind('AVG', avg_swr_df, stringsAsFactors = F)
colnames(avg_swr_df) <- c('striker_id', 'plc', 'swr')
swr_df <- rbind(swr_df, avg_swr_df)

# Return Win Rate Calculation ----
rwr_df <- rwr_calc(shot_df = shot_df_sample, matches_to_filter_out = pt, avg = F)
avg_rwr_df <- rwr_calc(shot_df = shot_df_sample, matches_to_filter_out = pt, avg = T)
avg_rwr_df <- cbind('AVG', avg_rwr_df, stringsAsFactors = F)
colnames(avg_rwr_df) <- c('returner_id', 'plc', 'rwr')
rwr_df <- rbind(rwr_df, avg_rwr_df)

# Transition Probability Matrices for each player ----

player_tpm <- player_tpm_calc(shot_df_sample, matches_to_filter_out = pt)

shot_df_filtered <- shot_df_sample %>% filter(!internal_point_id %in% pt)

potential_plcs <- c('14-6', '14-7', '14-10', '14-11', '14-14')

p1_potential_esvs <- mapply(
  esv_calc,
  rep(player_1_id_hidden, length(potential_plcs)),
  rep(player_2_id_hidden, length(potential_plcs)),
  potential_plcs,
  MoreArgs = list(
    p_tpm = player_tpm
    ,rwr_df = rwr_df
    ,swr_df = swr_df
    ,avg_rwr = avg_rwr
    ,avg_swr = avg_swr
    ,shot_df = shot_df_filtered
    ,is_striker = T))

p2_potential_esvs <- mapply(
  esv_calc,
  rep(player_1_id_hidden, length(potential_plcs)),
  rep(player_2_id_hidden, length(potential_plcs)),
  potential_plcs,
  MoreArgs = list(
    p_tpm = player_tpm
    ,rwr_df = rwr_df
    ,swr_df = swr_df
    ,avg_rwr = avg_rwr
    ,avg_swr = avg_swr
    ,shot_df = shot_df_filtered
    ,is_striker = F))
