rm(list = ls())
turkey_data <- read.csv("turkey.csv", na.strings = "NA")
myts <- ts(turkey_data, frequency = 12)
windowed_data<-window(myts, end = 3.99)

decomposed_data<- decompose(windowed_data,"multiplicative")

seasonality_factor <- unique(decomposed_data$seasonal)
plot(seasonality_factor, ylab = "Seasonality factor", xlab = "Month")

deseasonalized_sales <- myts/seasonality_factor
plot(myts, ylab = "Sales")
plot(deseasonalized_sales, ylab = "Deseasonalized Sales")

deseasonalized_sales_f <- data.frame(deseasonalized_sales = deseasonalized_sales,month = turkey_data$month)

 reg1 <- lm(deasonalized_sales ~ month, data = deseasonalized_sales_df)
 summary(reg1)
