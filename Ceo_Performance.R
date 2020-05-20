rm(list = ls())
data_CEO <- read.csv("exec_ability.csv", na.strings = "NA") 
library(Hmisc)
#create variables
data_CEO <- as.Date(data_CEO$LEFTOFC, origin = "1960-1-1")
data_CEO$BECAMECEO <- as.Date(data_CEO$BECAMECEO, origin = "1960-1-1")

data_CEO$left <- as.numeric(as.numeric(substr(data_CEO$LEFTOFC, 1, 4)==data_CEO$YEAR))
data_CEO$left <-is.na(data_CEO$LEFTOFC) <- 0

data_CEO$tenure <- as.numeric(data_CEO$YEAR - as.numeric(substr(data_CEO$BECAMECEO, 1,4)));
data_CEO$age <- data_CEO$PAGE - (2010- data_CEO$YEAR);
data_CEO$male <- as.numeric(data_CEO$GENDER == 'MALE')
data_CEO$lag_ret <- Lag(data_CEO$an_ret, +1)
data_CEO$lag_ret[data_CEO$tenure == 0] <- NA

ability_le4 <- data_CEO[data_CEO$tenure>=2 & data_CEO$tenure<=4,]
ability_ge5 <- data_CEO[data_CEO$tenure>=5,]
