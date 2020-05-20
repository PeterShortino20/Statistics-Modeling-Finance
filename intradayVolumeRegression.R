rm(list = ls())
library(Hmisc)
intraday_volume <- as.data.frame(read.csv("STATS/aa_volume.csv",header = TRUE))
intraday_volume <- intraday_volume[intraday_volume$Buckets == 26,]

TimeB.f <- factor(intraday_volume$TimeB)
intraday_volume$buckeno <-as.integer(TimeB.f)

bucket_dummies <- model.matrix(~TimeB.f+0)
intraday_volume <- cbind(intraday_volume, bucket_dummies)

reg1 <- lm(Turnover ~ bucket_dummies + 0, data = intraday_volume)
summary(reg1)

plot(reg1$coefficients, xlab = "Time Bucket",ylab = "volume")

intraday_volume <- cbind(intraday_volume, bucket_dummies)

