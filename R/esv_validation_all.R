library(ggplot2)
library(dplyr)
library(DT)

source('static_variables.R')
source('plotting_utilities.R')
source('esv_prep_utilities.R')
source('esv_calc_utilities.R')
source('all_data_prep.R')

points <- shot_df_all %>%
  filter(!(plc %in% c('1S') & strike_significance %in% c('out_of_bounds', 'net'))) %>%
  pull(internal_point_id) %>%
  unique()

esv_df <- matrix(NA, nrow = 0, ncol = 4)
colnames(esv_df) <- c('esv_striker', 'striker_won_point', 'esv_returner', 'returner_won_point')
for(p in points){
  
  print(
    sprintf("Starting fitting for point %s, %d/%d at %s", p, which(points == p), length(points), Sys.time()))
  
  df <- esv_validation(shot_df = shot_df_all, match_to_filter_out = p) %>% 
    select(esv_striker, striker_won_point, esv_returner, returner_won_point)
  esv_df <- rbind(esv_df, df)
  
}





