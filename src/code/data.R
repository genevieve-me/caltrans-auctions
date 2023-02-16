load("~/Downloads/Caltrans Auction/caltransdata.RData")

summary(caltransdata$Estimate)
sd(caltransdata$Estimate)

unique(caltransdata$ProjectID)
length(unique(caltransdata$ProjectID))
#705 auctions

pdfSB<-density(caltransdata[caltransdata$SmallBusinessPreference==1,]$Bid)
hist(caltransdata[caltransdata$SmallBusinessPreference==1,]$Bid,breaks=30,freq=FALSE,main="Small Business Bids PDF",xlab="Density",ylab="Bids")
lines(pdfSB)

bidders <- caltransdata$NumSmallB + numlarge
firstregression <- lm(Bid ~ bidders + WorkDays + Estimate, data = caltransdata)
