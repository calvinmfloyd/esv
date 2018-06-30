
library(dplyr)

upid_creation <- function(df){
  upid <- paste0(df$matchCode, '-', df$setId, '-', df$gameId, '-', df$pointId, '-', df$serveId)
  return(upid)
}

tMovement_raw <- read.csv(
  '../esv_data/tMovement.csv',
  colClasses = c('character', 'numeric', 'character','character',
                  'numeric', 'numeric', 'numeric', 'character', 
                  'character', 'numeric', 'numeric', 'numeric',
                  'character', 'numeric', 'character', 'numeric',
                  'numeric', 'numeric')
  )

tBall_raw <- read.csv(
  '../esv_data/tBall.csv',
  colClasses = c('numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))

tPoint_raw <- read.csv('tPoint.csv', colClasses = 'character')
tPoint_raw$rallyLength <- as.numeric(tPoint_raw$rallyLength)
tPoint_raw$id <- as.numeric(tPoint_raw$id)

tShot_raw <- read.csv(
  '../esv_data/tShot.csv',
  na.strings = 'NULL',
  colClasses = c('numeric', 'character', 'character', 'numeric','character', 'character', 'numeric', 'numeric', 'numeric')
  )

tTrajectory_raw <- read.csv('tTrajectory.csv', colClasses = 'character')

# prepping tTrajectory ----

tTrajectory <- tTrajectory_raw
colnames(tTrajectory)[which(colnames(tTrajectory) == 'point')] <- 'pointId'
tTrajectory$internal_point_id <- upid_creation(tTrajectory)

# prepping tPoint ----

tPoint <- tPoint_raw
tPoint$internal_point_id <- upid_creation(tPoint)

# prepping tShot  ----

tShot <- tShot_raw
tShot <- tShot %>% arrange(pointId)
tShot <- tShot %>% select(id, playerName, playerNumber, speed, shotType, spin, spinRpm, pointId, isServe)
df <- tPoint %>% select(id, internal_point_id)
tShot <- tShot %>% left_join(df, by = c('pointId' = 'id'))

# prepping tBall ----

tBall <- tBall_raw
tBall <- tBall %>% arrange(shotId, time)

df <- tShot %>% select(id, internal_point_id, playerName, isServe, speed, spin, spinRpm)
tBall <- tBall %>% left_join(df, by = c('shotId' = 'id'))
df_1 <- tPoint %>% select(internal_point_id, serveClass)
tBall <- tBall %>% left_join(df_1, by = 'internal_point_id')

# prepping tMovement ----

tMovement <- tMovement_raw
colnames(tMovement)[which(colnames(tMovement) == 'point')] <- 'pointId'
tMovement$internal_point_id <- upid_creation(tMovement)

tMovement <- tMovement %>%
  select(id, time, x1, y1, x2, y2, feedId, pointId, matchCode, setId, gameId, serveId, internal_point_id)

tMovement <- tMovement %>% arrange(internal_point_id, feedId, time)
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
