

all_df_raw <- read.csv('../../esv_data/Xdf_AllData.csv', stringsAsFactors = F)
all_df <- all_df_raw %>% select(-PlyrLocCombo, -NextShotLoc)
colnames(all_df) <- c(
  'internal_point_id'
  ,'match_code'
  ,'time'
  ,'striker_name'
  ,'striker_region'
  ,'returner_name'
  ,'returner_region'
  ,'strike_significance'
  ,'serve_class'
  ,'serve_significance'
  ,'return_significance'
)

all_df %>%
  mutate(sr1 = as.numeric(striker_region), rr1 = as.numeric(returner_region)) %>%
  mutate(
    sr1 = case_when(
      sr1 <= 8 ~ sr1
      ,between(sr1, 9, 10) ~ 9.0
      ,between(sr1, 11, 12) ~ 10.0
      ,between(sr1, 13, 14) ~ 11.0
      ,sr1 >= 15 ~ sr1 - 3),
    rr1 = case_when(
      rr1 <= 8 ~ rr1,
      rr1 %in% c(9, 10) ~ 9,
      rr1 %in% c(11, 12) ~ 10,
      rr1 %in% c(13, 14) ~ 11,
      rr1 >= 15 ~ rr1 - 3)) %>%
  mutate(striker_region = sr1, returner_region = rr1) %>%
  select(-sr1, -rr1) %>%
  mutate(plc = paste0(striker_region, '-', returner_region)) %>%
  mutate(next_plc = lead(plc, 1))










