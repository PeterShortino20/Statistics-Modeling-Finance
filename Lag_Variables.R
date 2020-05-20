rm(list-ls())
data <-read.csv("STATS/electricity.csv")
library(Hmisc)
data$June = as.integer(substr(data$Date99,1,1)=="6")
data$July = as.integer(substr(data$Date99,1,1)== "7")
data$August = as.integer(substr(data$Date99,1,1) == "8")
data$September = as.integer(substr(data$Date99,1,1) == "9")

data$lag1_temp = Lag(data$AverageTemperature,1)
data$lag2_temp = Lag(data$AverageTemperature,2)
data$lag3_temp = Lag(data$AverageTemperature,3)

reg1 <- lm(AverageTemperature ~ June + July + August + lag1_temp + lag2_temp + lag3_temp, data = data)
summary(reg1)

reg2 <- lm(AverageTemperature ~ July + August + lag1_temp + lag2_temp, data = data)
summary(reg2)

reg3 <- lm(AverageTemperature ~ July + August + lag1_temp + lag2_temp, data = data)
summary(reg3)

reg4 <- lm(AverageTemperature ~ August + lag1_temp + lag2_temp, data = data)
summary(reg4)

data$lag1_load <- Lag(data$PowerLoad,1)
reg5 <- lm(PowerLoad ~ AverageTemperature + lag1_load, data  = data)
summary(reg5)
