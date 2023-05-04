load("./src/code/Caltrans_Data/caltransdata.RData")

# FIXME: take out auctions with only one bidder

# summary stats on the data from 705 auctions
# here, getting the number of types of bidders (either 1 or 2)
types_bidders <- vector(length = length(unique(caltransdata$ProjectID)))
auction_rows <- match(unique(caltransdata$ProjectID), caltransdata$ProjectID)
auction_indexer <- 1
for (auction in auction_rows) {
  types_bidders[auction_indexer] <-
    (caltransdata$NumberofSmallBusinessBidders[auction] != 0) +
    (caltransdata$NumberofLargeBusinessBidders[auction] != 0)
  auction_indexer <- auction_indexer + 1
}

sb_bids <- caltransdata[caltransdata$SmallBusinessPreference == 1, ]$Bid
num_bidders <- caltransdata$NumberofSmallBusinessBidders +
  caltransdata$NumberofLargeBusinessBidders

my_smry <- function(x) {
  return(c(mean(x), sd(x), min(x), max(x)))
}
all_bids_smry <- my_smry(caltransdata$Bid)
sb_bids_smry <- my_smry(sb_bids)
num_bidders_smry <- my_smry(num_bidders)
num_types_bidders_smry <- my_smry(types_bidders)
eng_est_smry <- my_smry(caltransdata$Estimate)
workdays_smry <- my_smry(caltransdata$WorkDays)
summary_stats <- matrix(Reduce(c, list(all_bids_smry, sb_bids_smry,
                                       num_bidders_smry,
                                       num_types_bidders_smry,
                                       eng_est_smry, workdays_smry)),
                        nrow = 6, byrow = TRUE)
colnames(summary_stats) <- c("Mean", "Standard Deviation", "Minimum", "Maximum")
rownames(summary_stats) <- c("Bids", "Small Business Bids", "Number of Bidders",
                             "Business types present",
                             "Engineer's Estimates", "Workdays")
library(xtable)
print(xtable(summary_stats), latex.environments = NULL, booktabs = TRUE,
      file = "./src/sections/data-summary.tex")

# how close are the (winning) bids to the engineer's estimate?
find_winning_bid <- function(x) {
  low_bidder <- which.min(x$Bid)
  low_bid <- min(x$Bid)
  if (x$SmallBusinessPreference[low_bidder] == 1 ||
      sum(x$NumberofSmallBusinessBidders) == 0) {
    return(low_bid)
  }
  sb <- x[x$SmallBusinessPreference == 1, ]$Bid
  low_sb_bid <- min(sb)
  if (low_sb_bid / low_bid < 1.05) {
    return(low_sb_bid)
  } else {
    return(low_bid)
  }
}
winning_bids <- by(caltransdata, factor(caltransdata$ProjectID),
                   find_winning_bid)
mean(winning_bids - caltransdata[auction_rows, ]$Estimate)

# pdfs and cdfs

pdf_sb <- density(sb_bids, bw = "UCV")
png("src/imgs/sb-pdf.png")
hist(sb_bids, breaks = 100, freq = FALSE, main = "Small Business Bids PDF",
     xlab = "Density", ylab = "Bids", xlim = c(0, 5e6), ylim = c(0, 2e-6))
lines(pdf_sb, col = "firebrick")
dev.off()

lb_bids <- caltransdata[caltransdata$SmallBusinessPreference == 0, ]$Bid
lb_bw <- bw.ucv(lb_bids, lower = 1e-6, upper = 1e6)
pdf_lb <- density(lb_bids, bw = lb_bw)
png("./src/imgs/lb-pdf.png")
hist(lb_bids, breaks = 800, freq = FALSE, main = "Large Business Bids PDF",
     xlab = "Density", ylab = "Bids", xlim = c(0, 6e6), ylim = c(0, 1.8e-6))
lines(pdf_sb, col = "firebrick")
dev.off()

log_lb_bids <- log(lb_bids)
png("./src/imgs/log-lb-pdf.png")
hist(log_lb_bids, breaks = 30, freq = FALSE,
     main = "Log of Large Business Bids PDF", xlab = "Density", ylab = "Bids",
     xlim = c(10, 20))
pdf_log_lb <- density(log_lb_bids)
lines(pdf_log_lb)
dev.off()

log_sb_bids <- log(sb_bids)
png("./src/imgs/log-sb-pdf.png")
hist(log_sb_bids, breaks = 30, freq = FALSE,
     main = "Log of Small Business Bids PDF", xlab = "ln(Bids)",
     ylab = "Density", xlim = c(10, 18))
pdf_log_sb <- density(log_sb_bids)
lines(pdf_log_sb)
dev.off()

cdf_lb_bids <- cumsum(lb_bids) / sum(lb_bids)
cdf_sb_bids <- cumsum(sb_bids) / sum(sb_bids)

png("./src/imgs/cdf.png")
# CDFs of the bids for small and large businesses
plot(ecdf(lb_bids), xlim = c(0, 8e6), col = "red", lty = 4,
     main = "CDF of Large and Small Business Bids", xlab = "bid")
lines(ecdf(sb_bids), col = "blue", lty = 3)
legend("bottomright", legend = c("Large", "Small"),
       col = c("red", "blue"), lty = c(4, 3))
dev.off()

# regressions
calt_full <- cbind(num_bidders, caltransdata)

calt_full <- calt_full[calt_full$NumberofSmallBusinessBidders +
                       calt_full$NumberofLargeBusinessBidders != 1, ]

full_reg <- lm(Bid ~ num_bidders + Estimate + WorkDays, data = calt_full)
print(xtable(summary(full_reg)), latex.environments = NULL, booktabs = TRUE,
      file = "./src/sections/data-regressions.tex")

sb_reg <- lm(Bid ~ NumberofSmallBusinessBidders + Estimate + WorkDays,
             data = caltransdata[caltransdata$SmallBusinessPreference == 1, ])
print(xtable(summary(full_reg)), latex.environments = NULL, booktabs = TRUE,
      file = "./src/sections/data-regressions.tex", append = TRUE)


lb_reg <- lm(Bid ~ NumberofLargeBusinessBidders + Estimate + WorkDays,
             data = caltransdata[caltransdata$SmallBusinessPreference == 0, ])
print(xtable(summary(full_reg)), latex.environments = NULL, booktabs = TRUE,
      file = "./src/sections/data-regressions.tex", append = TRUE)



# Estimating section: want picture of denominator terms in slide 10, lecture ?
# density/cdf for type 1/type 2: 8 total
# small large: pdf/cdf cost, pdf/cdf bid (f, g, F, G)
# we are estimating the whole term from that slide: the COST
# evaluate density of type 1 bids at the type 2 bids value - ksdensity in matlab
# if you get a negative/infinity, check if g_1/2 is small - likely numerical
# calculation error
# all these pictures are for the median engineer's estimate
# the c we get is automatically conditional on that because right side is

# x -> (Cost) F_(L, S)(dot | x) -> (Bids) G_(L, S)(dot | X, n_L, n_S)

# we are assuming n does not affect f. in real world, does: decision to enter.
# larger n means bid more aggressively, # so affects g.
# select subset of auctions with the representative combination of n_L or n_S
# eg how many had 2 n_L and 5 n_S is largest percent, take those
# alternatively, not doing this requires a bit more work. x/bids are continuous,
# so we have to mix integers (n_S, n_L) in

# then, conditioning on x, bayes's rule
# so how do we get a joint probability density of bids and x?
# estimate marginal of X, divide first by second, integrate

# n by 2 matrix of n_S and n_L for each i in n
dt_bids <- cbind(calt_full$NumberofSmallBusinessBidders,
                 calt_full$NumberofLargeBusinessBidders)
rows <- c()
for (row_i in seq_len(nrow(dt_bids))) {
  current_row <- dt_bids[row_i, ]
  row_as_char <- paste(current_row, collapse = " ")
  rows <- c(rows, row_as_char)
}
sort(table(rows), decreasing = TRUE)
calt_subset <- calt_full[calt_full$NumberofSmallBusinessBidders == 1 &
                         calt_full$NumberofLargeBusinessBidders == 3, ]

# vectors of large/small bids from our subset
sb_bids_sub <- calt_subset[calt_subset$SmallBusinessPreference == 1, ]$Bid
lb_bids_sub <- calt_subset[calt_subset$SmallBusinessPreference == 0, ]$Bid
# change these further down
ests_subset <- calt_subset$Estimate
# there is just one estimate per auction, so median estimate is same for large/small
median_estimate <- median(ests_subset)
med_est_vec <- rep(median_estimate, length(sb_bids_sub)) # and lb bids too

sb_ests_sub <- calt_subset[calt_subset$SmallBusinessPreference == 1, ]$Estimate
# this is just sb_ests_sub with each entry repeated three times
lb_ests_sub <- calt_subset[calt_subset$SmallBusinessPreference == 0, ]$Estimate

# repeat the median appropriate number of times
sb_med_est_vec <- rep(median_estimate, length(sb_bids_sub))
lb_med_est_vec <- rep(median_estimate, length(lb_bids_sub))


library(hdrcde)
library(devtools)
# FIXME: there's no point using nix if I do this :(
devtools::install_github("https://github.com/sethmcg/climod")
library(climod)

# cde(x,y) gives p(y|x)
ests_grid <- seq(from = min(sb_ests_sub), to = max(sb_ests_sub), length = 21)
g_l <- cde(lb_ests_sub, lb_bids_sub, deg = 1, link = "log", nxmargin = 21,
           x.name = "Estimates", y.name = "Large Business Bids")
png("./src/imgs/g_l_cond.png")
plot(g_l)
dev.off()
png("./src/imgs/g_l_median.png")
plot(g_l$y, g_l$z[3, ], main = "Large Bid PDF",
     xlab = "Bid", ylab = "Estimated Density") # at the median estimate
dev.off()
g_s <- cde(sb_ests_sub, sb_bids_sub, deg = 1, link = "log", nxmargin = 21,
           x.name = "Estimates", y.name = "Small Business Bids")
png("./src/imgs/g_s_cond.png")
plot(g_s)
dev.off()
png("./src/imgs/g_s_median.png")
plot(g_s$y, g_s$z[3, ], main = "Small Bid PDF",
     xlab = "Bid", ylab = "Estimated Density")
dev.off()

# I think the issue I'm running into is that we are getting a joint PDF
# which integrates to one over the entire space of estimates. but we are just taking
# a subset of the median estimate. so it doesn't integrate to one.
# but, pdf2cdf can normalize to 1, so it's ok.

# y is a grid of length 100 plugged in for bids
# z has 100 columns: the third row is those evaluated at the median estimate
G_l <- pdf2cdf(g_l$z[3, ], g_l$y)
png("./src/imgs/G_l.png")
plot(G_l, main = "Large Bid CDF",
     xlab = "Bid", ylab = "Estimated Cumulative Density")
dev.off()
G_s <- pdf2cdf(g_s$z[3, ], g_s$y)
png("./src/imgs/G_s.png")
plot(G_s, main = "Small Bid CDF",
     xlab = "Bid", ylab = "Estimated Cumulative Density")
dev.off()

g_s_spline <- splinefun(g_s$y, g_s$z[3, ])
# integrate(g_s_spline, 100, 6e6) gives 0.996
g_l_spline <- splinefun(g_l$y, g_l$z[3, ])
g_s_s <- g_s_spline(sb_bids_sub)
g_l_l <- g_l_spline(lb_bids_sub)
g_l_s_105 <- g_l_spline(1.05 * sb_bids_sub)
g_s_l_105 <- g_s_spline(lb_bids_sub / 1.05)

G_s_spline <- splinefun(G_s)
G_l_spline <- splinefun(G_l)
G_s_s <- G_s_spline(sb_bids_sub)
G_l_l <- G_l_spline(lb_bids_sub)
G_l_s_105 <- G_l_spline(1.05 * sb_bids_sub)
G_s_l_105 <- G_s_spline(lb_bids_sub / 1.05)

n_S <- 1
n_L <- 3
cost_small <- sb_bids_sub - 1 / (((n_S - 1) * g_s_s) / (1 - G_s_s) + (n_L * g_l_s_105) / (1 - G_l_s_105))
cost_large <- lb_bids_sub - 1 / ((n_S * g_s_l_105) / (1 - G_s_l_105) + ((n_L- 1) * g_l_l) / (1 - G_l_l))

png("./src/imgs/f_s.png")
plot(density(cost_small[cost_small > 0 & is.na(cost_small) == FALSE]),
     main = "Estimated Distribution of Small Business Costs", xlim = c(0, 5.5e6))
dev.off()
png("./src/imgs/f_l.png")
plot(density(cost_large[cost_large > 0 & is.na(cost_large) == FALSE]),
     main = "Estimated Prob Distribution of Large Business Costs", xlim = c(0, 5.5e6))
dev.off()

png("./src/imgs/F_l.png")
plot(ecdf(cost_large[cost_large > 0 & is.na(cost_large) == FALSE]),
     main = "Estimated CDF of Large Business Costs", xlim = c(0, 5.5e6))
dev.off()
png("./src/imgs/F_s.png")
plot(ecdf(cost_small[cost_small > 0 & is.na(cost_small) == FALSE]),
     main = "Estimated CDF of Small Business Costs", xlim = c(0, 5.5e6))
dev.off()

# these do not have 5 percent rule
# cost_small <- sb_bids_sub - 1 / 
#   ( ((n_S - 1) * g_s_s) / (1 - G_s_s) + (n_L * g_l_s) / (1 - G_l_s) )
#     
# cost_large <- lb_bids_sub - 1 /
#   ( (n_S * g_s_l) / (1 - G_s_l) + ((n_L - 1) * g_l_l) / (1 - G_l_l) )

##### START UNUSED CODE ######

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