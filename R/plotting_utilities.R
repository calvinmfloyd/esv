
options(stringsAsFactors = F)

# Plotting the lines on the actual tennis court ----

draw_tennis_court_lines_ggplot <- function(){
  
  mid_line <- data.frame(
    x = c(-service_line, service_line),
    y = c(0, 0),
    ID = rep('mid_line', 2))
  
  top_sideline <- data.frame(
    x = c(-baseline_x, baseline_x),
    y = rep(baseline_y, 2),
    ID = rep('top_sideline', 2))
  
  bottom_sideline <- data.frame(
    x = c(-baseline_x, baseline_x),
    y = rep(-baseline_y, 2),
    ID = rep('bottom_sideline', 2))
  
  far_top_sideline <- data.frame(
    x = c(-baseline_x, baseline_x),
    y = rep(baseline_y + doubles_alley_width, 2),
    ID = rep('far_top_sideline', 2))
  
  far_bottom_sideline <- data.frame(
    x = c(-baseline_x, baseline_x),
    y = rep(-baseline_y - doubles_alley_width, 2),
    ID = rep('far_bottom_sideline', 2))
  
  pos_baseline <- data.frame(
    x = rep(baseline_x, 2),
    y = c(-baseline_y - doubles_alley_width, baseline_y + doubles_alley_width),
    ID = rep('pos_baseline', 2))
  
  neg_baseline <- data.frame(
    x = rep(-baseline_x, 2),
    y = c(-baseline_y - doubles_alley_width, baseline_y + doubles_alley_width),
    ID = rep('neg_baseline', 2))
  
  pos_service_line <- data.frame(
    x = rep(service_line, 2),
    y = c(-baseline_y, baseline_y),
    ID = rep('pos_service_line', 2))
  
  neg_service_line <- data.frame(
    x = rep(-service_line, 2),
    y = c(-baseline_y, baseline_y),
    ID = rep('neg_service_line', 2))
  
  df <- rbind(
    mid_line
    ,top_sideline
    ,bottom_sideline
    ,far_top_sideline
    ,far_bottom_sideline
    ,pos_baseline
    ,neg_baseline
    ,pos_service_line
    ,neg_service_line)
  
  return(df)
  
}

# ----

# Plotting the court surface of the actual tennis court ----

draw_tennis_court_surface_ggplot <- function(){
  
  ob_surface <- data.frame(
    xmin = -max_x
    ,xmax = max_x
    ,ymin = -max_y
    ,ymax = max_y
    ,ID = 'ob_surface')
  
  ib_surface <- data.frame(
    xmin = -baseline_x,
    xmax = baseline_x,
    ymin = -baseline_y - doubles_alley_width,
    ymax = baseline_y + doubles_alley_width,
    ID = 'ib_surface')
  
  df <- rbind(ob_surface, ib_surface)
  
}

# ----

# Plotting the tennis net on the court ----

draw_tennis_net_ggplot <- function(){
  
  df <- data.frame(
    x = c(0, 0)
    ,y = c(-baseline_y - doubles_alley_width - .5, baseline_y + doubles_alley_width + .5))  
  
  return(df)
  
}

# ----

# Drawing the lines of the grid system used in ESV ----

draw_region_lines_ggplot <- function(){
  
  short_hor_lines <- hor_lines_baseline[!(hor_lines_baseline %in% hor_lines_general)]
  df <- data.frame(
    x = c(
      rep(ver_lines, each = 2)
      ,rep(ver_lines, each = 2)
      ,rep(ver_lines, each = 2)
      ,rep(c(-max_x, max_x), length = 2*length(hor_lines_general))
      # ,rep(c(-13.887, -9.887), length = 2*length(short_hor_lines))
      # ,rep(c(9.887, 13.887), length = 2*length(short_hor_lines))
    )
    ,y = c(
      rep(c(-max_y, -13.887), length = 2*length(ver_lines))
      ,rep(c(-9.887, 9.887), length = 2*length(ver_lines))
      ,rep(c(13.887, max_y), length = 2*length(ver_lines))
      ,rep(hor_lines_general, each = 2)
      # ,rep(short_hor_lines, each = 2)
      # ,rep(short_hor_lines, each = 2)
      )
    ) %>%
    mutate(ID = rep(1:(length(x)/2), each = 2))
  
  return(df)
  
}

# ----

# Labeling the regions with numbers ----

draw_region_numbers_ggplot <- function(){
  
  net_to_baseline_regions <- 1:8
  behind_baseline_regions <- 15:18
  
  pos_ver_lines <- ver_lines[ver_lines >= 0]
  hor_lines_general_rev <- rev(hor_lines_general)
  hor_lines_baseline_rev <- rev(hor_lines_baseline)
  
  # FINDING THE x,y LOCATIONS OF THE REGIONS BEFORE THE BASELINE
  net_to_baseline_inds <- sapply(
    net_to_baseline_regions, function(x) which(region_grid == x, arr.ind = TRUE)) %>%
    t()
  
  net_to_baseline_locs <- apply(
    net_to_baseline_inds
    ,1
    ,function(x) c(
      mean(c(pos_ver_lines[x[1]], pos_ver_lines[x[1]+1])),
      mean(c(hor_lines_general_rev[x[2]], hor_lines_general_rev[x[2] + 1])))) %>%
    t()
  
  net_to_baseline_locs <- cbind(net_to_baseline_locs, net_to_baseline_regions)
  
  # FINDING THE x,y LOCATIONS OF THE REGIONS BEHIND THE BASELINE
  behind_baseline_inds <- sapply(
    behind_baseline_regions, function(x) which(region_grid == x, arr.ind = TRUE)) %>%
    t()
  
  behind_baseline_locs <- apply(
    behind_baseline_inds
    ,1
    ,function(x) c(
      mean(c(pos_ver_lines[x[1]], pos_ver_lines[x[1]+1])),
      mean(c(hor_lines_general_rev[x[2]], hor_lines_general_rev[x[2] + 1])))) %>%
    t()
  
  behind_baseline_locs <- cbind(behind_baseline_locs, behind_baseline_regions)
  
  # FINDING THE x,y LOCATIONS OF THE REGIONS AT THE BASELINE
  baseline_inds <- sapply(
    baseline_regions, function(x) which(baseline_regions == x)) %>%
    t()
  
  baseline_locs <- sapply(
    baseline_inds
    ,function(x) c(
      mean(c(pos_ver_lines[3], pos_ver_lines[4])) + 0.75,
      mean(c(hor_lines_baseline_rev[x], hor_lines_baseline_rev[x+1])))) %>%
    t()
  
  baseline_locs <- cbind(baseline_locs, baseline_regions)
  
  df <- rbind(
    net_to_baseline_locs,
    baseline_locs,
    behind_baseline_locs) %>%
    data.frame()
  
  colnames(df) <- c('x', 'y', 'region_number')
  
  return(df)
}

# ----

# Outputting rectangles in a dataframe, to help illustrate player strengths/weakness ----

draw_region_rect_ggplot <- function(){
  
  pos_ver_lines_before_baseline <- ver_lines[ver_lines >= 0 & ver_lines < 13.887]
  pos_ver_lines_after_baseline <- ver_lines[ver_lines >= 13.887]
  pos_ver_lines_baseline <- c(9.887, 13.887)
  
  pre_baseline_rects <- cbind(
    data.frame(matrix(
      c(rep(pos_ver_lines_before_baseline[1:2], 4), rep(pos_ver_lines_before_baseline[2:3], 4)),
      ncol = 2,
      byrow = T)),
    data.frame(matrix(
      c(rbind(hor_lines_general[1:4], lead(hor_lines_general,1)[1:4])),
      ncol = 2,
      byrow = T))
  )
  
  post_baseline_rects <- cbind(
    data.frame(matrix(
      rep(pos_ver_lines_after_baseline[1:2], 4),
      ncol = 2,
      byrow = T)),
    data.frame(matrix(
      c(rbind(hor_lines_general[1:4], lead(hor_lines_general,1)[1:4])),
      ncol = 2,
      byrow = T))
  )
  
  baseline_rects <- cbind(
    data.frame(matrix(
      rep(pos_ver_lines_baseline, 6),
      ncol = 2,
      byrow = T)),
    data.frame(matrix(
      c(rbind(hor_lines_baseline[1:6], lead(hor_lines_baseline,1)[1:6])),
      ncol = 2,
      byrow = T))
  )
  
  rects_df <- rbind(
    pre_baseline_rects,
    baseline_rects,
    post_baseline_rects)
  
  colnames(rects_df) <- c('xmin', 'xmax', 'ymin', 'ymax') 
  rects_df$region_number <- as.character(c(4:1, 8:5, 14:9, 18:15))
  
  rects_df
  
}

# ----