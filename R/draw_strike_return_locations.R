library(ggplot2)
library(dplyr)
library(DT)

source('esv_prep_utilities.R')
source('static_variables.R')
source('plotting_utilities.R')
source('sample_data_prep.R')

g <- ggplot() +
  
  geom_rect(data = draw_tennis_court_surface_ggplot(),
            mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ID)) +
  
  geom_path(data = draw_tennis_court_lines_ggplot(),
            mapping = aes(x = x, y = y, group = ID),
            color = 'white') +
  
  geom_path(data = draw_tennis_net_ggplot(),
            mapping = aes(x = x, y = y),
            color = 'white', linetype = 'dashed') +
  
  geom_point(data = shot_df_sample %>%
                    filter(!plc %in% c('1S', '2S')) %>%
                    mutate(x = ifelse(striker_x_coordinate < 0,
                                      -striker_x_coordinate,
                                      striker_x_coordinate),
                           y = ifelse(striker_x_coordinate < 0,
                                      -striker_y_coordinate,
                                      striker_y_coordinate)),
             mapping = aes(x = x, y = y),
             color = 'white', alpha = 0.25) +

  geom_point(data = shot_df_sample %>%
                    filter(!plc %in% c('1S', '2S')) %>%
                    mutate(x = ifelse(returner_x_coordinate < 0,
                                      returner_x_coordinate,
                                      -returner_x_coordinate),
                           y = ifelse(returner_x_coordinate < 0,
                                      returner_y_coordinate,
                                      -returner_y_coordinate)),
             mapping = aes(x = x, y = y),
             color = 'cyan', alpha = 0.25) +

  scale_fill_manual(values = c('ob_surface' = '#658B4E', 'ib_surface' = '#2A5387')) +
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
  ylab('y (m)') + xlab('x (m)') +
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) +
  theme(line = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "mm"),
        legend.title = element_blank(),
        panel.background = element_rect(colour = NULL, fill = NULL))

ggsave('../plots/strike_return_locations.jpg', g, height = 150, width = 200, unit = 'mm')

