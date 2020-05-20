rm(list=ls())
library(sas7bdat)
library(Hmisc)

crack<-read.sas7bdat("crack.sas7bdat")

#1
crack$crack_spread <- crack$gas_price-crack$oil_price
crack$d_crack_spread <- 100*crack$crack_spread/crack$CPI
crack$lagd_crack_spread <- Lag(crack$d_crack_spread, +1)
crack$cap_util <- crack$production/crack$capacity

#2
reg1 <- lm(d_crack_spread ~ cap_util + lagd_crack_spread, data=crack)
summary(reg1) 

#3
reg2 <- lm(production ~ year, data=crack)
summary(reg2)

reg3 <- lm(capacity ~ year, data=crack)
summary(reg3)

