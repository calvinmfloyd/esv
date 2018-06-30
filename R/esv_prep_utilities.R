library(dplyr)

# Function to classify shots for a player in a point based on the final bounce ----

point_shot_classifier <- function(final_bounce){
  
  n <- length(final_bounce)
  last_bounce_val <- unique(final_bounce)
  
  if(last_bounce_val == 'net') shot_class_vec <- c(
    rep('non_impactful', ifelse(n > 2, n-2, 0)),
    c('forced_net', 'net') %>% tail(ifelse(n < 2, n, 2)))
  
  if(last_bounce_val == 'inbounds') shot_class_vec <- c(
    rep('non_impactful', ifelse(n > 3, n-3, 0)),
    c('set_up_pure_winner', 'set_up_opp_pure_winner', 'pure_winner') %>% tail(ifelse(n < 3, n, 3)))
  
  if(last_bounce_val == 'out_of_bounds') shot_class_vec <- c(
    rep('non_impactful', ifelse(n > 2, n-2, 0)),
    c('forced_out_of_bounds', 'out_of_bounds') %>% tail(ifelse(n < 2, n, 2)))
  
  return(shot_class_vec)
}

# Function to classify an (x,y) coordinate in grid system ----

region_locator <- function(x, y){
  
  pos_ver_lines <- ver_lines[ver_lines >= 0]
  df <- data.frame(x1 = x, y1 = y) %>%
    mutate(x = ifelse(x1 < 0, -x1, x1),
           y = ifelse(x1 < 0, -y1, y1)) %>%
    data.frame()
  
  df$region <- NA
  baseline_filter <- {df$x <= pos_ver_lines[4] & df$x >= pos_ver_lines[3]}
  
  df$region[baseline_filter] <- 8 + sapply(df$y[baseline_filter], function(x) sum(x <= hor_lines_baseline))
  
  df$region[!baseline_filter] <- mapply(
    function(x, y)region_grid[sum(x >= pos_ver_lines),sum(y <= hor_lines_general)],
    df$x[!baseline_filter],
    df$y[!baseline_filter])
  
  return(df$region)
}

# Rounding numbers to the nearest base ----

round_to_nearest <- function(x, base){ 
  base*round(x/base) 
} 

