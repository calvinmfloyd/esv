
# Function to create SWR dataframe ----

swr_calc <- function(shot_df, matches_to_filter_out = c(), avg = F){
  
  df <- shot_df[!(shot_df$internal_point_id %in% matches_to_filter_out),]
  df <- df %>% count(striker_id, plc, strike_significance)
  df$weight_per_strike <- weights_df$weight[match(df$strike_significance, weights_df$significance)]
  df <- df[!(df$plc == '1S' & df$strike_significance %in% c('out_of_bounds', 'net')),]
  df$weight <- df$n*df$weight_per_strike
  
  if(avg){ df <- df %>% group_by(plc) } else { df <- df %>% group_by(striker_id, plc) }
    
  df <- df %>% summarise(swr = sum(weight)/sum(n)) %>% ungroup()
  
  if(avg){ df <- df %>% arrange(plc) } else { df <- df %>% arrange(striker_id, plc) }
  
  df
}

# ----

# Function to create RWR dataframe ----

rwr_calc <- function(shot_df, matches_to_filter_out = c(), avg = F){

  df <- shot_df[!(shot_df$internal_point_id %in% matches_to_filter_out),]
  df <- df[df$return_significance != 'insignificant',]
  df <- df %>% count(returner_id, plc, return_significance)
  df$weight_per_return <- weights_df$weight[match(df$return_significance, weights_df$significance)]
  df$weight <- df$n*df$weight_per_return
  
  if(avg){ df <- df %>% group_by(plc) } else { df <- df %>% group_by(returner_id, plc) }
  
  df <- df %>% summarise(rwr = sum(weight)/sum(n)) %>% ungroup()
  
  if(avg){ df <- df %>% arrange(plc) } else { df <- df %>% arrange(returner_id, plc) }
  
  df
}

# ----

# General function to create a TPM ----

trans_prob_matrix <- function(X, prob = T){
  tt <- table(c(X[,-ncol(X)]), c(X[,-1]))
  if(prob) tt <- tt / rowSums(tt)
  tt
}

# ----

# Player TPM function ----

player_tpm_calc <- function(shot_df, matches_to_filter_out){
  
  players <- shot_df$striker_id %>% unique()
  player_tpm <- list()
  X <- shot_df[!(shot_df$internal_point_id %in% matches_to_filter_out),]
  for(p in players){
    
    X1 <- as.matrix(X[X$striker_id == p, c('plc', 'next_plc')])
    
    tpm <- matrix(0, nrow = max_reg^2 + 6, ncol = max_reg^2 + 6, dimnames = list(states, states))
    
    p_tpm <- trans_prob_matrix(X1)
    reordered_rows <- match(rownames(tpm), rownames(p_tpm))
    reordered_rows <- reordered_rows[!is.na(reordered_rows)]
    reordered_cols <- match(colnames(tpm), colnames(p_tpm))
    reordered_cols <- reordered_cols[!is.na(reordered_cols)]
    p_tpm <- p_tpm[reordered_rows, reordered_cols]
    
    tpm[match(rownames(p_tpm), rownames(tpm)),
        match(colnames(p_tpm), colnames(tpm))] <- p_tpm
    
    player_tpm[[p]] <- tpm
  } 
  
  # Average transition probability matrix
  X1 <- as.matrix(X[,c('plc', 'next_plc')])
  
  tpm <- matrix(0, nrow = max_reg^2 + 6, ncol = max_reg^2 + 6, dimnames = list(states, states))
  
  avg_tpm <- trans_prob_matrix(X1)
  reordered_rows <- match(rownames(tpm), rownames(avg_tpm))
  reordered_rows <- reordered_rows[!is.na(reordered_rows)]
  reordered_cols <- match(colnames(tpm), colnames(avg_tpm))
  reordered_cols <- reordered_cols[!is.na(reordered_cols)]
  avg_tpm <- avg_tpm[reordered_rows, reordered_cols]
  
  tpm[match(rownames(avg_tpm), rownames(tpm)),
      match(colnames(avg_tpm), colnames(tpm))] <- avg_tpm
  
  player_tpm[['AVG']] <- tpm
  
  player_tpm

}

# ----

# esv Calculation Function ----

esv_calc <- function(str_id, ret_id, plc, p_tpm, rwr_df, swr_df, avg_swr, avg_rwr, shot_df, is_striker = T){
  
  if(sum(p_tpm[[str_id]][plc,]) == 0){
    tpm <- p_tpm[['AVG']][plc,]
    if(sum(tpm) == 0){
      x <- shot_df %>% count(next_plc) %>% mutate(n = n/sum(n))
      tpm <- x$n
      names(tpm) <- x$next_plc
    }
  } else {
    tpm <- p_tpm[[str_id]][plc,]
  }
  
  w_prob <- tpm['W']
  l_prob <- tpm['L']
  
  w_weight <- ifelse(is_striker, 1, 0)
  l_weight <- ifelse(is_striker, 0, 1)
  
  non_wl_prob <- tpm[tpm > 0 & !(names(tpm) %in% c('W', 'L'))]
  non_wl_prob <- cbind(names(non_wl_prob), non_wl_prob) %>% data.frame(stringsAsFactors = F)
  colnames(non_wl_prob) <- c('plc', 'prob')
  non_wl_prob$prob <- non_wl_prob$prob %>% as.numeric()

  if(is_striker){
    wr_df <- rwr_df %>%
      filter(returner_id == ret_id, plc %in% rownames(non_wl_prob)) %>%
      select(plc, rwr) %>%
      rename(wr = rwr)
    avg_wr_df <- rwr_df %>% filter(returner_id == 'AVG') %>% rename(wr = rwr)
    avg_wr <- avg_rwr
  } else {
    wr_df <- swr_df %>%
      filter(striker_id == str_id, plc %in% rownames(non_wl_prob)) %>%
      select(plc, swr) %>%
      rename(wr = swr)
    avg_wr_df <- swr_df %>% filter(striker_id == 'AVG') %>% rename(wr = swr)
    avg_wr <- avg_swr
  }
  
  non_wl_portion <- non_wl_prob %>%
    left_join(wr_df, by = c('plc')) %>%
    mutate(
      wr = ifelse(is.na(wr), avg_wr_df$wr[match(plc, avg_wr_df$plc)], wr)
    ) %>%
    # mutate(
    #   wr = ifelse(is.na(wr), avg_wr, wr)
    # ) %>%
    summarise(x = sum(prob*wr)) %>%
    pull()
  
  w_prob*w_weight + l_prob*l_weight + non_wl_portion
}

# ----

# esv Validation Function ----

esv_validation <- function(shot_df, match_to_filter_out){

  avg_swr <- shot_df %>%
    filter(!(plc == '1S' & strike_significance == 'out_of_bounds')) %>%
    count(strike_significance) %>%
    mutate(weight = weights_df$weight[match(strike_significance, weights_df$significance)]) %>%
    summarise(x = sum(n*weight)/sum(n)) %>%
    pull()
  
  avg_rwr <- shot_df %>%
    filter(
      !(plc == '1S' & strike_significance == 'out_of_bounds'), return_significance != 'insignificant') %>%
    count(return_significance) %>%
    mutate(weight = weights_df$weight[match(return_significance, weights_df$significance)]) %>%
    summarise(x = sum(n*weight)/sum(n)) %>%
    pull()
  
  # Strike Win Rate Calculation ----
  swr_df <- swr_calc(shot_df = shot_df, matches_to_filter_out = match_to_filter_out, avg = F)
  avg_swr_df <- swr_calc(shot_df = shot_df, matches_to_filter_out = match_to_filter_out, avg = T)
  avg_swr_df <- cbind('AVG', avg_swr_df, stringsAsFactors = F)
  colnames(avg_swr_df) <- c('striker_id', 'plc', 'swr')
  swr_df <- rbind(swr_df, avg_swr_df)
  
  # Return Win Rate Calculation ----
  rwr_df <- rwr_calc(shot_df = shot_df, matches_to_filter_out = match_to_filter_out, avg = F)
  avg_rwr_df <- rwr_calc(shot_df = shot_df, matches_to_filter_out = match_to_filter_out, avg = T)
  avg_rwr_df <- cbind('AVG', avg_rwr_df, stringsAsFactors = F)
  colnames(avg_rwr_df) <- c('returner_id', 'plc', 'rwr')
  rwr_df <- rbind(rwr_df, avg_rwr_df)
  
  # Transition Probability Matrices for each player ----
  
  player_tpm <- player_tpm_calc(shot_df, matches_to_filter_out = match_to_filter_out)
  
  # Calculating ESV's for each match not filtered out ----
  
  shot_df_subset <- shot_df %>% filter(internal_point_id %in% match_to_filter_out)
  
  shot_df_subset$esv_striker <- mapply(
    esv_calc,
    shot_df_subset$striker_id,
    shot_df_subset$returner_id,
    shot_df_subset$plc,
    MoreArgs = list(
      p_tpm = player_tpm
      ,rwr_df = rwr_df
      ,swr_df = swr_df
      ,avg_rwr = avg_rwr
      ,avg_swr = avg_swr
      ,shot_df = shot_df
      ,is_striker = T))
  
  shot_df_subset$esv_returner <- mapply(
    esv_calc,
    shot_df_subset$striker_id,
    shot_df_subset$returner_id,
    shot_df_subset$plc,
    MoreArgs = list(
      p_tpm = player_tpm
      ,rwr_df = rwr_df
      ,swr_df = swr_df
      ,avg_rwr = avg_rwr
      ,avg_swr = avg_swr
      ,shot_df = shot_df
      ,is_striker = F))
  
  df <- shot_df_subset %>%
    select(
      internal_point_id,
      plc,
      time,
      striker_id,
      esv_striker,
      striker_won_point,
      returner_id,
      esv_returner,
      returner_won_point) %>%
    data.frame(stringsAsFactors = F)
  
  # if(return_all_info){
  #   
  #   return(list('sv_df' = swr_df, 'rv_df' = rwr_df, esv_pt_df))
  #   
  # }
  return(df)
  
}

# ----