
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
# source('../../esv_hidden_variables.R')

an <- function(x) as.numeric(x)
region_oi <- 14

pt <- match_code_hidden

avg_swr <- shot_df_all %>%
  filter(!(plc == '1S' & strike_significance == 'out_of_bounds')) %>%
  count(strike_significance) %>%
  mutate(weight = weights_df$weight[match(strike_significance, weights_df$significance)]) %>%
  summarise(x = sum(n*weight)/sum(n)) %>%
  pull()

avg_rwr <- shot_df_all %>%
  filter(
    !(plc == '1S' & strike_significance == 'out_of_bounds'), return_significance != 'insignificant') %>%
  count(return_significance) %>%
  mutate(weight = weights_df$weight[match(return_significance, weights_df$significance)]) %>%
  summarise(x = sum(n*weight)/sum(n)) %>%
  pull()

# Strike Win Rate Calculation ----
swr_df <- swr_calc(shot_df = shot_df_all, matches_to_filter_out = pt, avg = F)
avg_swr_df <- swr_calc(shot_df = shot_df_all, matches_to_filter_out = pt, avg = T)
avg_swr_df <- cbind('AVG', avg_swr_df, stringsAsFactors = F)
colnames(avg_swr_df) <- c('striker_id', 'plc', 'swr')
swr_df <- rbind(swr_df, avg_swr_df)

# Return Win Rate Calculation ----
rwr_df <- rwr_calc(shot_df = shot_df_all, matches_to_filter_out = pt, avg = F)
avg_rwr_df <- rwr_calc(shot_df = shot_df_all, matches_to_filter_out = pt, avg = T)
avg_rwr_df <- cbind('AVG', avg_rwr_df, stringsAsFactors = F)
colnames(avg_rwr_df) <- c('returner_id', 'plc', 'rwr')
rwr_df <- rbind(rwr_df, avg_rwr_df)

# Transition Probability Matrices for each player ----

player_tpm <- player_tpm_calc(shot_df_all, matches_to_filter_out = pt)

shot_df_filtered <- shot_df_all %>% filter(!internal_point_id %in% pt)

other_player_ids = unique(shot_df_all$striker_id)
other_player_ids = other_player_ids[!other_player_ids %in% c(player_2_id_hidden)]
average_list = list()
regions = paste0(region_oi, '-', 1:15)

for (p in other_player_ids) {
  
  # esv_calc(
  #   str_id = player_1_id_hidden,
  #   ret_id = p,
  #   plc = regions[3],
  #   p_tpm = player_tpm,
  #   rwr_df = rwr_df,
  #   swr_df = swr_df,
  #   avg_rwr = avg_rwr,
  #   avg_swr = avg_swr,
  #   shot_df = shot_df_all,
  #   is_striker = FALSE
  # )
  
  df = mapply(
    esv_calc,
    rep(player_1_id_hidden, 15),
    rep(p, 15),
    regions,
    MoreArgs = list(
      p_tpm = player_tpm
      ,rwr_df = rwr_df
      ,swr_df = swr_df
      ,avg_rwr = avg_rwr
      ,avg_swr = avg_swr
      ,shot_df = shot_df_all
      ,is_striker = F
      )) %>%
    matrix(nrow = 1) %>%
    data.frame(stringsAsFactors = F)
  colnames(df) = regions
    
  average_list[[length(average_list) + 1]] = df
}

avg_esv_df = bind_rows(average_list) %>%
  tidyr::gather(key = plc, value = esv) %>%
  group_by(plc) %>%
  summarise(esv = mean(esv)) %>%
  mutate(region_number = an(stringr::str_extract(plc, '(?<=\\-)\\d{1,2}')))
  
gdf <- unname(cbind(
  draw_region_numbers_ggplot(),
  mapply(
    esv_calc,
    rep(player_1_id_hidden, 15),
    rep(player_2_id_hidden, 15),
    paste0(region_oi, '-', 1:15),
    MoreArgs = list(
      p_tpm = player_tpm
      ,rwr_df = rwr_df
      ,swr_df = swr_df
      ,avg_rwr = avg_rwr
      ,avg_swr = avg_swr
      ,shot_df = shot_df_filtered
      ,is_striker = F
      ))
))
colnames(gdf) <- c('x', 'y', 'region_number', 'esv')

esv_diff_df = gdf %>%
  left_join(avg_esv_df %>% select(region_number, avg_esv = esv), by = 'region_number') %>%
  mutate(
    esv_from_avg = round(esv - avg_esv, 3),
    esv_from_avg = ifelse(region_number %in% c(1, 4, 5), 0, esv_from_avg),
    esv_from_avg_sign = case_when(
      esv_from_avg == 0 ~ 'zero',
      esv_from_avg > 0 ~ 'pos',
      esv_from_avg < 0 ~ 'neg'
      ),
    esv_from_avg_label = case_when(
      region_number %in% c(1, 4, 5) ~ 'N/A',
      esv_from_avg >= 0 ~ paste0('+', esv_from_avg),
      TRUE ~ as.character(esv_from_avg))
    ) %>%
  left_join(
    draw_region_rect_ggplot() %>% mutate(region_number = an(region_number)),
    by = 'region_number')


gdf = gdf %>%
  mutate(esv = round(esv,3)) %>%
  mutate(esv = case_when(
    an(esv) == 0.5 ~ '0.500',
    an(esv) == round(an(esv), 1) & an(esv) == round(an(esv), 2) & an(esv) == round(an(esv), 3) ~ paste0(esv, '.000'),
    an(esv) == round(an(esv), 1) & an(esv) == round(an(esv), 2) ~ paste0(esv, '.00'),
    an(esv) == round(an(esv), 1) ~ paste0(esv, '.0'),
    an(esv) == round(an(esv), 2) & an(esv) == round(an(esv), 3) ~ paste0(esv, '0'),
    an(esv) == round(an(esv), 2) ~ paste0(esv, '0'),
    T ~ as.character(esv)
  )) %>%
  rowwise() %>%
  mutate(
    percentile = round(an(esv)*100),
    percentile = ifelse(percentile == 55, 50, percentile),
    color = kingsutils::get_percentile_color(percentile),
    percentile = as.character(percentile)
    ) %>%
  ungroup() %>%
  mutate(esv = ifelse(is.na(esv) | esv == '0.545', 'N/A', esv)) %>%
  left_join(
    draw_region_rect_ggplot() %>% mutate(region_number = an(region_number)),
    by = 'region_number')

color_mapping <- unique(gdf$color)
names(color_mapping) = unique(gdf$percentile)

player_location <- draw_region_numbers_ggplot() %>% filter(region_number == as.character(region_oi))
player_location$label <- 'P1'

# Raw ESV plot
g <- ggplot() +
  
  geom_rect(data = draw_tennis_court_surface_ggplot(),
            mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ID)) +
  
  geom_rect(data = gdf,
            mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = percentile)) +
  
  geom_path(data = draw_tennis_court_lines_ggplot(),
            mapping = aes(x = x, y = y, group = ID),
            color = 'white') +
  
  geom_path(data = draw_tennis_net_ggplot(),
            mapping = aes(x = x, y = y),
            color = 'white', linetype = 'dashed') +
  
  geom_path(data = draw_region_lines_ggplot(),
            mapping = aes(x = x, y = y, group = ID),
            color = 'white', alpha = 0.5, linetype = 'dotted') +
  
  geom_label(data = gdf,
             mapping = aes(x = x, y = y, label = esv),
             color = 'black', size = 4, alpha = 0.75) +
  
  geom_text(data = player_location,
            mapping = aes(x = -x, y = -y, label = label),
            color = 'white', fontface = 2, size = 10) +
  
  scale_fill_manual(values = c('ob_surface' = '#658B4E', 'ib_surface' = '#2A5387', color_mapping)) +
  scale_y_continuous(limits = c(-max_y, max_y),
                     breaks = c(round(-baseline_y - doubles_alley_width, 3),
                                round(-baseline_y, 3),
                                0,
                                round(baseline_y, 3),
                                round(baseline_y + doubles_alley_width,3)),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(-max_x, max_x),
                     breaks = c(round(-baseline_x, 3),
                                round(-service_line, 3),
                                0,
                                round(service_line, 3),
                                round(baseline_x, 3)),
                     expand = c(0,0)) +
  guides(fill = FALSE, size = FALSE, alpha = FALSE, colour = FALSE) +
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0),"mm"),
        legend.title = element_blank(),
        panel.background = element_rect(colour = NULL, fill = NULL))

ggsave('../plots/esv_visualized.jpg', g, height = 100, width = 200, unit = 'mm')

# ESV Difference Plot

g_diff <- ggplot() +
  
  geom_rect(data = draw_tennis_court_surface_ggplot(),
            mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ID)) +
  
  geom_rect(data = esv_diff_df,
            mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = esv_from_avg_sign)) +
  
  geom_path(data = draw_tennis_court_lines_ggplot(),
            mapping = aes(x = x, y = y, group = ID),
            color = 'white') +
  
  geom_path(data = draw_tennis_net_ggplot(),
            mapping = aes(x = x, y = y),
            color = 'white', linetype = 'dashed') +
  
  geom_path(data = draw_region_lines_ggplot(),
            mapping = aes(x = x, y = y, group = ID),
            color = 'white', alpha = 0.5, linetype = 'dotted') +
  
  geom_label(data = esv_diff_df,
             mapping = aes(x = x, y = y, label = esv_from_avg_label),
             color = 'black', size = 4, alpha = 0.75) +
  
  geom_text(data = player_location,
            mapping = aes(x = -x, y = -y, label = label),
            color = 'white', fontface = 2, size = 10) +
  
  scale_fill_manual(
    values = c(
      'ob_surface' = '#658B4E',
      'ib_surface' = '#2A5387',
      'neg' = kingsutils::get_percentile_color(10),
      'zero' = kingsutils::get_percentile_color(50),
      'pos' = kingsutils::get_percentile_color(90)
      )
    ) +
  scale_y_continuous(limits = c(-max_y, max_y),
                     breaks = c(round(-baseline_y - doubles_alley_width, 3),
                                round(-baseline_y, 3),
                                0,
                                round(baseline_y, 3),
                                round(baseline_y + doubles_alley_width,3)),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(-max_x, max_x),
                     breaks = c(round(-baseline_x, 3),
                                round(-service_line, 3),
                                0,
                                round(service_line, 3),
                                round(baseline_x, 3)),
                     expand = c(0,0)) +
  guides(fill = FALSE, size = FALSE, alpha = FALSE, colour = FALSE) +
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0),"mm"),
        legend.title = element_blank(),
        panel.background = element_rect(colour = NULL, fill = NULL))

ggsave('../plots/esv_from_avg.jpg', g_diff, height = 100, width = 200, unit = 'mm')


