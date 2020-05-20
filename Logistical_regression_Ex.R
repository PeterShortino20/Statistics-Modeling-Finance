
rm(list = ls())
#-1 indicates NA
data = read.csv("STATS/com_mort-2.csv", row.names = 1, na.strings = -1, stringsAsFactors = TRUE)

#creat variable age, NOI, logvalue, age_le_ten
data$Orginination_Date = as.Date(data$Origination_Date)
data$age <- as.numeric(substr(data$Origination_Date, 1, 4)) - data$Year_Built
data$NOI <- data$Recent_NOI/data$Recent_Value
data$logvalue<- log(data$Recent_Value)
data$age.le.ten <- as.numeric(data$age <= 10)

#use the 7th category (multifamily) as the base case
data$Property_Sector = relevel(data$Property_Sector, ref = 7)

#run logistic regression
fit_logistic <- glm(Inv_Bank_Dummy ~ NOI  + age.le.ten + logvalue  + Physical_Occupancy_At_Securitiza + Property_Sector, data = data, family = binomial())

summary(fit_logistic)
