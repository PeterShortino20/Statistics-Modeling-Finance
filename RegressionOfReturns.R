library(quantmod)
rm(list=ls())
tmpAMZN <- getSymbols(c('AMZN'), src='yahoo');
tmpSPX <- getSymbols(c('SPY'), src='yahoo');
PriceData <- data.frame(Date=index(SPY),
                        SPY=as.numeric(SPY[,6]),
                        AMZN = as.numeric(AMZN[,6]))
tmp <- as.matrix(PriceData[,c(2,3)])

tmp_returns <- data.frame(SPY = diff(log(tmp[,1])),
                         AMZN = diff(log(tmp[,2])))

tmp_returns <- cbind(Date=PriceData$Date[-1], tmp_returns)

# Linear Regression of AMZN on SPY
lm_result = lm(AMZN ~ SPY, data = tmp_returns)

summary(lm_result)

plot(tmp_returns$SPY, tmp_returns$AMZN, ylab = "AMZN log return", xlab = "S&P 500 log return", main ="")
abline(lm_result)

#Plot the rsiduals against the S&P 500 returns 
lm_result.res = resid(lm_result)

plot(tmp_returns$SPY, lm_result.res, ylab="Residuals",
     xlab ="S&P500 log return", main= "")
abline(0,0)

hist(lm_result.res, breaks=40, main="", xlab="Residuals")

qqnorm(lm_result.res)
qqline(lm_result.res)

new_data = data.frame(SPY = seq(-0.1, 0.1, by = 0.005))

pred_lm = predict.lm(lm_result,newdata = new_data,
                     level = 0.9, interval = "prediction")
matplot(x = new_data$SPY, y = pred_lm, type = 'l', lty = 1,
        xlab = "SPY Return", ylab = "AMZN Return")
legend("topleft", legend = colnames(pred_lm),col =1:3, lty = 1)
