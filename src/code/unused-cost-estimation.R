##### START UNUSED CODE ######

# these do not have 5 percent rule
# cost_small <- sb_bids_sub - 1 /
#   ( ((n_S - 1) * g_s_s) / (1 - G_s_s) + (n_L * g_l_s) / (1 - G_l_s) )
#
# cost_large <- lb_bids_sub - 1 /
#   ( (n_S * g_s_l) / (1 - G_s_l) + ((n_L - 1) * g_l_l) / (1 - G_l_l) )

# repeat the median appropriate number of times
sb_med_est_vec <- rep(median_estimate, length(sb_bids_sub))
lb_med_est_vec <- rep(median_estimate, length(lb_bids_sub))

library(ks) # density does not support higher dimensional kde
# step 3 conditional bids evaluated at bids (not b)
# also returns a vector of probabilities
conditional_bids <- function(bids, estimates, to_pred) {
  # step 1 joint PDF of bid and estimates evaluated at BID and median
  # trains on some bids/estimates, returns a vector of estimates for other bids (and median est)
  kde_joint_pdf <- ks::kde(x = cbind(bids, estimates),
                           eval.points = to_pred)
  joint_bids_est <- kde_joint_pdf$estimate
  
  # step 2 marginal of estimates evaluated at its median
  # returns a scalar
  marg_kde_pdf <- density(estimates, bw = 0.01) # look into bw issue
  myspline <- approxfun(marg_kde_pdf)
  marginal_bids <- myspline(median(estimates))
  
  return(joint_bids_est / marginal_bids)
}

# get functions for our g's so we can get G's by numerical integration
vgrid <- seq(1e3, 5.9e6, by = 500)
vgrid_medians <- rep(median_estimate, length(vgrid))
# create a function for g_l and for g_s
# I think splinefun is better than approxfun?
gl_grid <- cbind(vgrid, conditional_bids(lb_bids_sub, lb_ests_sub, cbind(vgrid, vgrid_medians)))
gl_spline <- splinefun(gl_grid)
gs_grid <- cbind(vgrid, conditional_bids(sb_bids_sub, sb_ests_sub, cbind(vgrid, vgrid_medians)))
gs_spline <- splinefun(gs_grid)

G_s_f <- function(evalpoint) {
  out <- integrate(gs_spline, 1e3, evalpoint)$value
  return(out)
}

G_l_f <- function(evalpoint) {
  out <- integrate(gl_spline, 1e3, evalpoint)$value
  return(out)
}

# lines - sort and then rownames <- NULL
# plot(sb_bids_sub, g_s_s)
plot(gs_grid)
plot(lb_bids_sub, g_l_l)
# TODO make smooth lines
plot(sb_bids_sub, G_s_s)
plot(lb_bids_sub, G_l_l)

# evaluate the conditional densities for bids
# g_s_l is g_s(large bids)
# evaluate at the bids because these are for plugging into the cost function
g_s_s <- conditional_bids(sb_bids_sub, sb_ests_sub, cbind(sb_bids_sub, sb_med_est_vec))
g_l_l <- conditional_bids(lb_bids_sub, lb_ests_sub, cbind(lb_bids_sub, lb_med_est_vec))
g_l_s_105 <- conditional_bids(lb_bids_sub, lb_ests_sub, cbind(1.05 * sb_bids_sub, sb_med_est_vec))
g_s_l_105 <- conditional_bids(sb_bids_sub, sb_ests_sub, cbind(lb_bids_sub / 1.05, lb_med_est_vec))

G_s_s <- sapply(sb_bids_sub, G_s_f)
G_l_l <- sapply(lb_bids_sub, G_l_f)
# same thing - CDFs with 1.05 * bids
G_l_s_105 <- sapply(1.05 * sb_bids_sub, G_l_f)
G_s_l_105 <- sapply(lb_bids_sub / 1.05, G_s_f)

##### END UNUSED CODE #####
