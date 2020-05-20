
library(gdata)
library(readr)
data <- read.csv("STATS/Spread_Commercial_Mortgages.csv", na = "N/A")

data$Origination_Date = as.Date(data$Origination_Date, format = "%m/%d/%y")
data$Fixed_maturity = as.Date(data$Fixed_Maturity, format = "%m/%d/%y")

data$LTV = data$Orig_Note_Amount/data$Recent_Value
data$maturity = (data$Fixed_maturity - data$Origination_Date)/365.25
data$amortization_rate = (data$Orig_Note_Amount - data$Scheduled_Balloon)/data$Orig_Note_Amount
data$age = as.numeric(substr(data$Origination_Date, 1, 4)) - as.numeric(data$Year_Built)
data$NOI = data$Recent_NOI/ data$Recent_Value



#write describe() function to generate the five statistics
describe = function(x){
  result = c(mean(x, na.rm = TRUE),
             median(x,na.rm = TRUE),
             sd(x, na.rm = TRUE),
             min(x,na.rm = TRUE),
             max(x,na.rm = TRUE))
  names(result) = c("mean","median","sd","min","max")
  return(result)
}
describe(data$LTV)
describe(data$maturity)
describe(data$amortization_rate)
describe(data$age)
describe(data$NOI)

delete_age = which(apply(data, 1, function(x) any(is.na(x))))
delete_age = c(delete_age,which(data$age > 200 | data$age < 0))
data = data[-delete_age,]

aggregate(data$LTV, by=list(data$Property_Sector), FUN = mean)
aggregate(data$LTV, by=list(data$Property_Sector), FUN = sd)

aggregate(data$amortization_rate, by=list(data$Property_Sector), FUN = mean)
aggregate(data$amortization_rate, by=list(data$Property_Sector), FUN = sd)

lm_1 = lm(Spread ~ LTV + maturity + amortization_rate + Inv_Bank_Dummy, data = data)


