
library(dplyr)
options(stringsAsFactors = F)

shot_df_all_raw <- read.csv('Xdf_AllData.csv', stringsAsFactors = F)
shot_df_all <- shot_df_all_raw %>% select(-PlyrLocCombo, -NextShotLoc)
colnames(shot_df_all) <- c(
  'internal_point_id'
  ,'match_code'
  ,'time'
  ,'returner_id'
  ,'returner_region'
  ,'striker_id'
  ,'striker_region'
  ,'strike_significance'
  ,'serve_class'
  ,'serve_significance'
  ,'return_significance'
)

shot_df_all <- shot_df_all %>%
  mutate(sr1 = as.numeric(striker_region), rr1 = as.numeric(returner_region)) %>%
  mutate(
    sr1 = case_when(
      sr1 <= 8 ~ sr1
      ,between(sr1, 9, 10) ~ 9.0
      ,between(sr1, 11, 12) ~ 10.0
      ,between(sr1, 13, 14) ~ 11.0
      ,sr1 >= 15 ~ sr1 - 3
      ),
    rr1 = case_when(
      rr1 <= 8 ~ rr1,
      rr1 %in% c(9, 10) ~ 9,
      rr1 %in% c(11, 12) ~ 10,
      rr1 %in% c(13, 14) ~ 11,
      rr1 >= 15 ~ rr1 - 3
      )
    ) %>%
  mutate(striker_region = sr1, returner_region = rr1) %>%
  select(-sr1, -rr1) %>%
  mutate(plc = paste0(striker_region, '-', returner_region)) %>%
  # mutate(plc = paste0(striker_region, '-', returner_region)) %>%
  mutate(plc = if_else(serve_significance == 'NoServeSignif', plc, serve_significance)) %>%
  mutate(
    strike_significance = case_when(
      strike_significance == 'Net' ~ 'net'
      ,strike_significance == 'OutofBounds' ~ 'out_of_bounds'
      ,strike_significance == 'NonImpactful' ~ 'non_impactful'
      ,strike_significance == 'ForcedNet' ~ 'forced_net'
      ,strike_significance == 'ForcedOutofBounds' ~ 'forced_out_of_bounds'
      ,strike_significance == 'SetUpofPureWinner' ~ 'set_up_pure_winner'
      ,strike_significance == 'SetUpOppPureWinner' ~ 'set_up_opp_pure_winner'
      ,strike_significance == 'PureWinner' ~ 'pure_winner')
    ) %>%
  group_by(internal_point_id) %>%
  mutate(
    return_significance = case_when(
      row_number() == n() ~ ifelse(strike_significance %in% c('pure_winner'), 'no_return', 'insignificant')
      ,TRUE ~ lead(strike_significance, 1)
      ),
    next_plc = case_when(
      row_number() == n() ~ ifelse(strike_significance %in% c('net', 'out_of_bounds'), 'L', 'W')
      ,TRUE ~ lead(plc, 1)
      )
    ) %>%
  ungroup()

shot_df_all <- shot_df_all %>%
  left_join(
    shot_df_all %>%
      group_by(internal_point_id) %>%
      filter(time == max(time)) %>%
      summarise(scorer = if_else(strike_significance == 'pure_winner', striker_id, returner_id)) %>%
      ungroup(),
    by = 'internal_point_id'
    ) %>%
  mutate(
    striker_won_point = ifelse(striker_id == scorer, 1, 0),
    returner_won_point = ifelse(returner_id == scorer, 1, 0)
    )

