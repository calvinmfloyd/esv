
library(dplyr)

upid_creation <- function(df){
  upid <- paste0(df$matchCode, '-', df$setId, '-', df$gameId, '-', df$pointId, '-', df$serveId)
  return(upid)
}

round_to_nearest <- function(x, base) base*round(x/base)

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

tMovement_raw <- read.csv(
  '../../local/esv/tMovement.csv',
  colClasses = c('character', 'numeric', 'character','character',
                  'numeric', 'numeric', 'numeric', 'character', 
                  'character', 'numeric', 'numeric', 'numeric',
                  'character', 'numeric', 'character', 'numeric',
                  'numeric', 'numeric')
  )

tBall_raw <- read.csv(
  '../../local/esv/tBall.csv',
  colClasses = c('numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))

tPoint_raw <- read.csv('../../local/esv/tPoint.csv', colClasses = 'character')
tPoint_raw$rallyLength <- as.numeric(tPoint_raw$rallyLength)
tPoint_raw$id <- as.numeric(tPoint_raw$id)

tShot_raw <- read.csv(
  '../../local/esv/tShot.csv',
  na.strings = 'NULL',
  colClasses = c('numeric', 'character', 'character', 'numeric','character', 'character', 'numeric', 'numeric', 'numeric')
  )

tTrajectory_raw <- read.csv('../../local/esv/tTrajectory.csv', colClasses = 'character')

# prepping tTrajectory ----

tTrajectory <- tTrajectory_raw
colnames(tTrajectory)[which(colnames(tTrajectory) == 'point')] <- 'pointId'
tTrajectory$internal_point_id <- upid_creation(tTrajectory)

# prepping tPoint ----

tPoint <- tPoint_raw
tPoint$internal_point_id <- upid_creation(tPoint)

# prepping tShot  ----

tShot <- tShot_raw
tShot <- tShot %>%
  arrange(pointId) %>% 
  select(id, playerName, playerNumber, speed, shotType, spin, spinRpm, pointId, isServe) %>%
  left_join(tPoint %>% select(id, internal_point_id), by = c('pointId' = 'id'))

# prepping tBall ----

tBall <- tBall_raw
tBall <- tBall %>%
  arrange(shotId, time) %>%
  left_join(
    tShot %>% select(id, internal_point_id, playerName, isServe, speed, spin, spinRpm),
    by = c('shotId' = 'id')) %>%
  left_join(
    tPoint %>% select(internal_point_id, serveClass),
    by = 'internal_point_id')

# prepping tMovement ----

tMovement <- tMovement_raw
colnames(tMovement)[which(colnames(tMovement) == 'point')] <- 'pointId'
tMovement$internal_point_id <- upid_creation(tMovement)

tMovement <- tMovement %>%
  select(id, time, x1, y1, x2, y2, feedId, pointId, matchCode, setId, gameId, serveId, internal_point_id) %>%
  arrange(internal_point_id, feedId, time)

df_keep_feed_ids <- tMovement %>%
  group_by(internal_point_id) %>%
  summarise(feed_id = max(feedId)) %>%
  ungroup() %>%
  pull(feed_id)

tMovement <- tMovement %>% filter(feedId %in% df_keep_feed_ids)
tMovement$time_string <- tMovement$time %>% as.character()

tMovement <- tMovement %>%
  left_join(tTrajectory %>% select(internal_point_id, playerAtXName), by = 'internal_point_id')

colnames(tMovement)[which(colnames(tMovement) == 'playerAtXName')] <- 'PName2'

tMovement <- tMovement %>%
  left_join(
    tPoint %>%
      select(internal_point_id, serverName, receiverName),
    by = 'internal_point_id')

na_counts_df <- tMovement %>%
  group_by(internal_point_id) %>%
  summarise(na_counts = sum(is.na(serverName)) + sum(is.na(receiverName)))

tMovement <- tMovement %>% left_join(na_counts_df, by = 'internal_point_id') %>% distinct()
tMovement <- tMovement[tMovement$na_counts == 0,]
tMovement$PName1[tMovement$PName2 == tMovement$serverName] <- tMovement$receiverName[tMovement$PName2 == tMovement$serverName]
tMovement$PName1[tMovement$PName2 != tMovement$serverName] <- tMovement$serverName[tMovement$PName2 != tMovement$serverName]

# Final Preparations

bounce_df <- tBall %>%
  group_by(shotId) %>%
  filter(bounce == 1) %>%
  mutate(
    x = ifelse(x < 0, -x, x),
    y = ifelse(x < 0, -y, y)) %>%
  ungroup() %>%
  select(shotId, x, y) 

valid_point_ids <- tPoint %>% filter(valid == 1, tournamentCode == 'MS') %>% pull(internal_point_id) %>% unique()

shot_df_sample <- tBall %>%
  arrange(internal_point_id) %>%
  filter(internal_point_id %in% valid_point_ids,
         net == 0) %>%
  group_by(shotId) %>%
  mutate(no_bounce = ifelse(sum(bounce) == 0, TRUE, FALSE)) %>%
  ungroup() %>%
  group_by(internal_point_id) %>%
  mutate(
    final_bounce = ifelse(
      no_bounce %>% tail(1) == TRUE,
      'net',
      ifelse(abs(x %>% tail(1)) > inbounds_baseline_x |
               abs(y %>% tail(1)) > inbounds_sideline_y |
               any(grepl('FAULT', serveClass)),
            'out_of_bounds',
            'inbounds'))) %>%
  ungroup() %>%
  filter(bounce == 0) %>%
  mutate(rounded_time = ifelse(time > time %>% round_to_nearest(0.04),
                               {time %>% round_to_nearest(0.04) + 0.04} %>% as.character(),
                               {time %>% round_to_nearest(0.04)}  %>% as.character())) %>%
  left_join(tMovement %>% select(internal_point_id, time_string,
                                 x1, y1, PName1, x2, y2, PName2),
            by = c('internal_point_id' = 'internal_point_id',
                   'rounded_time' = 'time_string')) %>%
  mutate(striker_id = playerName,
         returner_id = ifelse(playerName == PName1, PName2, PName1)) %>%
  mutate(striker_x_coordinate = ifelse(striker_id == PName1, x1, x2),
         striker_y_coordinate = ifelse(striker_id == PName1, y1, y2),
         returner_x_coordinate = ifelse(returner_id == PName1, x1, x2),
         returner_y_coordinate = ifelse(returner_id == PName1, y1, y2)) %>%
  group_by(internal_point_id) %>%
  mutate(strike_significance = point_shot_classifier(final_bounce),
         is_serve_return = ifelse(row_number() == 2, 1, 0),
         serve_number = strsplit(internal_point_id,'-') %>% unlist() %>% tail(1)) %>%
  ungroup() %>%
  filter(!is.na(striker_x_coordinate)) %>%
  mutate(striker_region = region_locator(striker_x_coordinate, striker_y_coordinate),
         returner_region = region_locator(returner_x_coordinate, returner_y_coordinate)) %>%
  mutate(plc = ifelse(
    isServe == 1, 
    paste0(serve_number, 'S'),
    ifelse(is_serve_return == 1, paste0(serve_number, 'SR'), paste0(striker_region, '-', returner_region)))) %>%
  left_join(tPoint %>% select(internal_point_id, scorerName),
            by = c('internal_point_id')) %>%
  mutate(striker_won_point = ifelse(striker_id == scorerName, 1, 0),
         returner_won_point = ifelse(returner_id == scorerName, 1, 0)) %>%
  select(internal_point_id,
         shotId,
         time,
         striker_region,
         returner_region,
         plc,
         strike_significance,
         striker_id,
         striker_x_coordinate,
         striker_y_coordinate,
         striker_won_point,
         returner_id,
         returner_x_coordinate,
         returner_y_coordinate,
         returner_won_point)

ids_with_repeated_times <- shot_df_sample %>%
  group_by(internal_point_id, time) %>%
  mutate(rn = n()) %>%
  ungroup() %>%
  filter(rn > 1) %>% 
  pull(internal_point_id) %>% 
  unique()
shot_df_sample <- shot_df_sample %>% filter(!(internal_point_id %in% ids_with_repeated_times))

shot_df_sample <- shot_df_sample %>%
  group_by(internal_point_id) %>%
  mutate(
    next_plc = ifelse(
      row_number() != n(),
      ifelse(strike_significance %in% c('1S', '2S'), paste0(strike_significance, 'R'), plc %>% lead(1)),
      ifelse(strike_significance %in% c('pure_winner'), 'W', ifelse(plc %in% c('1S'), '2S', 'L'))),
    return_significance = ifelse(
      row_number() != n(),
      strike_significance %>% lead(1),
      ifelse(strike_significance %in% c('1S', 'out_of_bounds', 'net'), 'insignificant', 'no_return'))) %>%
  ungroup()

shot_df_sample <- shot_df_sample %>%
  left_join(
    bounce_df %>% rename(bounce_x = x, bounce_y = y),
    by = 'shotId')
