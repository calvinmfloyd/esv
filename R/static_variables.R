
baseline_x <- 11.887
baseline_y <- 4.115
inbounds_baseline_x <- 11.887 + .0265
inbounds_sideline_y <- 4.115 + .0265
max_x <- 18.5
max_y <- 8.5
service_line <- 6.401
far_line <- 1.3716
x_incr <- max_x - baseline_x
y_incr <- max_y - baseline_y
doubles_alley_width <- 1.3716

ver_lines <- c(-max_x, -13.887, -9.887, -4.9435, 0, 4.9435, 9.887, 13.887, max_x)
hor_lines_general <- c(-max_y, -4.115, 0, 4.115, max_y)
hor_lines_baseline <- c(-max_y, -4.115, -2.0575, 0, 2.0575, 4.115, max_y)

n <- (length(ver_lines)-1)/2
p_general <- length(hor_lines_general) - 1
p_baseline <- length(hor_lines_baseline) - 1

region_grid <- matrix(data = c(1:12, 15:18), nrow = 4, byrow = TRUE)
baseline_regions <- c(9, 10, 11, 12, 13, 14)

# Weights used in the ESV estimator ----

weights_df <- data.frame(
  significance = c('pure_winner',
                   'out_of_bounds',
                   'net',
                   'non_impactful',
                   'forced_out_of_bounds',
                   'forced_net',
                   'set_up_pure_winner',
                   'set_up_opp_pure_winner',
                   'no_return'),
  weight = c(1.0, 0.0, 0.0, 0.5, 0.75, 0.75, 0.75, 0.25, 0.0)
)

# States used in TPM's ----

max_reg <- 18
states <- paste0(rep(c(1:max_reg), each = max_reg), '-', rep(c(1:max_reg), length = max_reg^2)) 
states <- c(states, '1S', '1SR', '2S', '2SR', 'W', 'L')

