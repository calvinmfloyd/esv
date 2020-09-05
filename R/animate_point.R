
library(ggplot2)
library(ggrepel)
library(gganimate)
library(dplyr)
library(DT)

setwd("~/esv/R")

source('static_variables.R')
source('plotting_utilities.R')
source('esv_prep_utilities.R')
source('esv_calc_utilities.R')
source('sample_data_prep.R')
source('all_data_prep.R')
source('../../local/esv/hidden_variables.R')

shots = shot_df_sample %>%
  filter(internal_point_id == internal_point_id_hidden) %>%
  mutate(time = round(time, 2))

player_1_shots = shots %>%
  mutate(
    x = ifelse(returner_id == player_1_name_hidden, returner_x_coordinate, striker_x_coordinate),
    y = ifelse(returner_id == player_1_name_hidden, returner_y_coordinate, striker_y_coordinate),
    player_id = 1
    ) %>%
  select(time, x, y, player_id)

player_2_shots = shots %>%
  mutate(
    x = ifelse(returner_id == player_2_name_hidden, returner_x_coordinate, striker_x_coordinate),
    y = ifelse(returner_id == player_2_name_hidden, returner_y_coordinate, striker_y_coordinate),
    player_id = 2
    ) %>%
  select(time, x, y, player_id)

gg_animation = ggplot() +
  
  geom_rect(data = draw_tennis_court_surface_ggplot(),
            mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ID)) +
  
  geom_path(data = draw_tennis_court_lines_ggplot(),
            mapping = aes(x = x, y = y, group = ID),
            color = 'white') +
  
  geom_path(data = draw_tennis_net_ggplot(),
            mapping = aes(x = x, y = y),
            color = 'white', linetype = 'dashed') +
  
  geom_label(
    data = shots,
    mapping = aes(
      label = paste("Strike State:", plc)
      ),
    x = 0,
    y = 7,
    size = 5
  ) + 

  geom_label(
    data = player_1_shots,
    mapping = aes(
      x = x,
      y = y,
      label = player_id
      ),
    size = 5
    ) +
  
  geom_label(
    data = player_2_shots,
    mapping = aes(
      x = x,
      y = y,
      label = player_id
      ),
    size = 5
    ) +
  
  geom_point(
    data = shots,
    mapping = aes(
      x = striker_x_coordinate,
      y = striker_y_coordinate
      ),
    color = "#CCFF00",
    size = 3
    ) +
  
    transition_states(
      time,
      transition_length = 2,
      state_length = 1
      ) +
  
    geom_path(data = draw_region_lines_ggplot(),
              mapping = aes(x = x, y = y, group = ID),
              color = 'white', alpha = 0.5, linetype = 'dotted') +

    geom_text(data = draw_region_numbers_ggplot(),
              mapping = aes(x = x, y = y, label = region_number),
              color = 'white', size = 5, alpha = 0.5, angle = 270) +

    geom_text(data = draw_region_numbers_ggplot(),
              mapping = aes(x = -x, y = -y, label = region_number),
              color = 'white', size = 5, alpha = 0.5, angle = 90) +
  
  ggtitle(
    label = "Time: {closest_state} seconds"
    ) +
  
  scale_fill_manual(values = c('ob_surface' = '#658B4E', 'ib_surface' = '#2A5387')) +
  scale_y_continuous(
    limits = c(-8.5, 8.5),
    breaks = c(
      round(-baseline_y - doubles_alley_width, 3),
      round(-baseline_y, 3),
      0,
      round(baseline_y, 3),
      round(baseline_y + doubles_alley_width, 3)
    ),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(-max_x, max_x),
    breaks = c(
      round(-baseline_x, 3),
      round(-service_line, 3),
      0,
      round(service_line, 3),
      round(baseline_x, 3)
    ),
    expand = c(0, 0)
  ) +
  guides(
    fill = FALSE,
    size = FALSE,
    alpha = FALSE,
    colour = FALSE
    ) +
  
  ylab(NULL) + 
  xlab(NULL) +
  
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) +
  
  theme(
    line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    legend.title = element_blank(),
    panel.background = element_rect(colour = NULL, fill = NULL)
    )

anim_save("../animations/individual_point.gif", gg_animation, height = 400, width = 600)
