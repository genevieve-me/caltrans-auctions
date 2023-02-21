load("./src/code/Caltrans_Data/caltransdata.RData")
lintr::lint("./src/code/data.R")

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
# num_types_bidders_smry <- my_smry(caltransdata$...)
eng_est_smry <- my_smry(caltransdata$Estimate)
workdays_smry <- my_smry(caltransdata$WorkDays)
summary_stats <- matrix(Reduce(c, list(all_bids_smry, sb_bids_smry,
                                       num_bidders_smry,
                                       #num_types_bidders_smry,
                                       eng_est_smry, workdays_smry)),
                        nrow = 5, byrow = TRUE)
colnames(summary_stats) <- c("Mean", "Standard Deviation", "Minimum", "Maximum")
rownames(summary_stats) <- c("Bids", "Small Business Bids", "Number of Bidders",
                             # "Number of different types of bidders",
                             "Engineer's Estimates", "Workday")
library(xtable)
print(xtable(summary_stats), latex.environments = NULL, booktabs = TRUE)

# how close are the (winning) bids to the engineer's estimate?
find_winning_bid <- function(x) {
  low_bidder <- which.min(x$Bid)
  low_bid <- min(x$Bid)
  if (x$SmallBusinessPreference[low_bidder] == 1 ||
      sum(x$NumberofSmallBusinessBidders)) {
    return(low_bid)
  }
  sb <- x[x$SmallBusinessPreference == 1, ]$Bid
  low_sb_bidder <- which.min(sb)
  low_sb_bid <- min(sb)
  if (low_sb_bid / low_bid < 1.05) {
    return(low_sb_bid)
  } else {
    return(low_bid)
  }
}
winning_bids <- by(caltransdata, factor(caltransdata$ProjectID), find_winning_bid)
mean(winning_bids - caltransdata[auction_rows, ]$Estimate)

# pdfs and cdfs

pdf_sb <- density(sb_bids, bw = "UCV")
png("sb-pdf.png")
hist(sb_bids, breaks = 100, freq = FALSE, main = "Small Business Bids PDF",
     xlab = "Density", ylab = "Bids", xlim = c(0, 5e6), ylim = c(0, 2e-6))
lines(pdf_sb, col = "green")
dev.off()

lb_bids <- caltransdata[caltransdata$SmallBusinessPreference == 0, ]$Bid
lb_bw <- bw.ucv(lb_bids, lower = 1e-6, upper = 1e6)
pdf_lb <- density(lb_bids, bw = lb_bw)
png("lb-pdf.png")
hist(lb_bids, breaks = 800, freq = FALSE, main = "Large Business Bids PDF",
     xlab = "Density", ylab = "Bids", xlim = c(0, 6e6), ylim = c(0, 1.8e-6))
lines(pdf_sb)
dev.off()

log_lb_bids <- log(lb_bids)
hist(log_lb_bids, breaks = 30, freq = FALSE,
     main = "Log of Large Business Bids PDF", xlab = "Density", ylab = "Bids",
     xlim = c(10, 20))
pdf_log_lb <- density(log_lb_bids)
lines(pdf_log_lb)

log_sb_bids <- log(sb_bids)
png("log-sb-pdf.png")
hist(log_sb_bids, breaks = 30, freq = FALSE,
     main = "Log of Small Business Bids PDF", xlab = "ln(Bids)",
     ylab = "Density", xlim = c(10, 18))
pdf_log_sb <- density(log_sb_bids)
lines(pdf_log_sb)
dev.off()

cdf_lb_bids <- cumsum(lb_bids) / sum(lb_bids)
cdf_sb_bids <- cumsum(sb_bids) / sum(sb_bids)

png("cdf.png")
# CDFs of the bids for small and large businesses
plot(ecdf(lb_bids), xlim = c(0, 8e6), col = "red",
     main = "CDF of Large and Small Business Bids", xlab = "bid")
lines(ecdf(sb_bids), col = "blue")
# FIXME: maybe just try ggplot2
legend("bottomright", legend = c("Large - Red", "Small - Blue"))
dev.off()
# https://stackoverflow.com/questions/19053440/

# probably remove this
plot(density(cumsum(lb_bids)))
cs_log_sb_bids <- cumsum(log_sb_bids)
hist(cs_log_sb_bids / sum(log_sb_bids), breaks = 30, freq = FALSE,
     main = "Log of Large Business Bids CDF", xlab = "Density",
     ylab = "Bids", xlim = c(-0.2, 1.2))
lines(density(cs_log_sb_bids / sum(log_sb_bids)))

# regressions TODO

bidders <- caltransdata$NumSmallB + numlarge
# cbind
firstregression <- lm(Bid ~ bidders + WorkDays + Estimate, data = caltransdata)

calt_full <- cbind(num_bidders, caltransdata)

full_reg <- lm(Bid ~ num_bidders + Estimate + WorkDays, data = calt_full)
print(xtable(summary(full_reg)), latex.environments = NULL, booktabs = TRUE)

sb_reg <- lm(Bid ~ NumberofSmallBusinessBidders + Estimate + WorkDays,
	     data = caltransdata[caltransdata$SmallBusinessPreference == 1, ])
summary(sb_reg) 


lb_reg <- lm(Bid ~ NumberofLargeBusinessBidders + Estimate + WorkDays,
	     data = caltransdata[caltransdata$SmallBusinessPreference == 0, ])
summary(lb_reg)
