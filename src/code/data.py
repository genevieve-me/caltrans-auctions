from patsy import dmatrices
import statsmodels.api as sm
import numpy as np
import seaborn as sns
import random

import pandas as pd

random.seed(12345)

caltransdata = pd.read_csv("./src/code/caltransdata.csv")

# take out procurement auctions with only one bidder
caltransdata = caltransdata[
    caltransdata["NumberofSmallBusinessBidders"]
    + caltransdata["NumberofLargeBusinessBidders"]
    > 1
]

# want summary stats for: bids, subset of small/large bus. bids, number/types
# of bidders, estimate, workdays

mystats = ["mean", "std", "min", "max"]
summary_stats = caltransdata.agg(
    {"Bid": mystats, "Estimate": mystats, "WorkDays": mystats, }
).transpose()

# in this data, auction-level data is duplicated, so group by auction
ct_auctions = caltransdata.groupby(by="ProjectID")
# number of types (small/large) of bidders (either 1 or 2)
types_bidders = ct_auctions.first(
)["NumberofSmallBusinessBidders"] + ct_auctions.first()["NumberofLargeBusinessBidders"]

# bids for only small/large businesses
grouped_bids_smry = caltransdata[["Bid", "SmallBusinessPreference"]].groupby(
    "SmallBusinessPreference").describe().reset_index()
# grouped_bids_smry.rename(columns={"SmallBusinessPreference": "IsSmallBusiness", })
grouped_bids_smry.drop("SmallBusinessPreference", axis=1, inplace=True)

summary_stats.to_latex(buf="./src/sections/data-summary.tex",
                       caption="Summary stats for auctions with 2 or more bidders.")


# how close are the (winning) bids to the engineer's estimate?

# first implement the DoT's algorithm to favor small businesses
# takes a data frame from groupby object
def find_winning_bid(auction):
    is_low_bidder_small = auction.loc[auction["Bid"].idxmin(
    )]["SmallBusinessPreference"]
    low_bid = auction["Bid"].min()
    if auction["NumberofSmallBusinessBidders"].sum() == 0 or is_low_bidder_small == 0:
        return low_bid
    lowest_small_bid = auction[auction["SmallBusinessPreference"] == 1]["Bid"].min(
    )
    if lowest_small_bid / low_bid < 1.05:
        return lowest_small_bid
    else:
        return low_bid


# with "_", unpack the tuple and discard the names (IDs)
winning_bids = pd.Series(find_winning_bid(auction)
                         for _, auction in ct_auctions)
bid_est_diff = winning_bids - ct_auctions.first()["Estimate"]
bid_est_diff.describe()


# PDFs and CDFs

sns.set_theme()

sb_bids = caltransdata[caltransdata["SmallBusinessPreference"] == 1]
lb_bids = caltransdata[caltransdata["SmallBusinessPreference"] == 0]

# python libraries do not have appropriate bandwidth selectors (Sheather-Jones)
# for non-Gaussian data points. manually override with the output of R's bw.SJ
# https://github.com/statsmodels/statsmodels/issues/7205
sns.displot(data=sb_bids, x="Bid", kde=True, bins=100, stat="probability",
            kde_kws={"bw_method": 0.1742})

sns.displot(data=lb_bids, x="Bid", kde=True, bins=100, stat="probability",
            kde_kws={"bw_method": 0.2193})

sb_bids["LogBid"] = np.log(sb_bids["Bid"])
lb_bids["LogBid"] = np.log(lb_bids["Bid"])

# estimate Empirical CDFs

sb_bids["BidCDF"] = np.cumsum(sb_bids["Bid"]) / sb_bids["Bid"].sum()
lb_bids["BidCDF"] = np.cumsum(lb_bids["Bid"]) / lb_bids["Bid"].sum()
sns.ecdfplot(data=sb_bids, x="BidCDF")
sns.ecdfplot(data=lb_bids, x="BidCDF")


# Regressions


caltransdata["NumBidders"] = caltransdata["NumberofSmallBusinessBidders"] + \
    caltransdata["NumberofLargeBusinessBidders"]
y, X = dmatrices("Bid ~ NumBidders + Estimate + WorkDays",
                 data=caltransdata, return_type="dataframe")
mod = sm.OLS(y, X)
mod.fit().summary()
