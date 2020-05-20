library("dplyr")
fund_data <- read.csv(file = "STATS/growth_fund_nav.csv", stringsAsFactors = FALSE)
fama_data <- read.csv(file = "STATS/fama_french_factors.csv", row.names = 1)
fund_data$year <- as.numeric(substr(fund_data$DATE, 1, 4))
fund_data$month <-as.numeric(substr(fund_data$DATE, 5, 6))
fund_fama_data <- merge(fund_data,fama_data, by = c("month","year")) 
fund_fama_data$Mkt_RF <- fund_fama_data$Mkt_RF/100
fund_fama_data$RF <- fund_fama_data$RF/100

fund_fama_data$retm_rf <- as.numeric(fund_fama_data$RETM) - fund_fama_data$RF

data_noNA <- filter(fund_fama_data, !is.na(retm_rf))

data_rearranged <- arrange(data_noNA, ICDI)

data_grouped <- group_by(data_rearranged,ICDI)

fit_results <- data_grouped %>% do(model = lm(retm_rf ~ Mkt_RF, data = .))

coef_results <- summarise(fit_results, 
                          alpha = coef(summary(model))[1],
                          beta = coef(summary(model))[2],
                          std_err_alpha = coef(summary(model))[3],
                          std_err_beta = coef(summary(model))[4],
                          t_ratio_alpha = coef(summary(model))[5],
                          t_ratio_beta = coef(summary(model))[6])

# print percentiles for alpha
round(quantile(coef_results$alpha, 
               c(.01, .05, .1, .25, .5, .75, .9, .95, .99), na.rm=TRUE), 3)

# print percentiles for beta

round(quantile(coef_results$beta,  
               c(.01, .05, .1, .25, .5, .75, .9, .95, .99),  na.rm=TRUE), 2)

# print percentiles for the t-statistic for alpha

round(quantile(coef_results$t_ratio_alpha, 
               c(.01, .05, .1, .25, .5, .75, .9, .95, .99), na.rm=TRUE), 2)


# print a histogram for alpha and its t-statistic

hist(coef_results$alpha, xlab="alpha", main="")
hist(coef_results$t_ratio_alpha, xlab="alpha t-statistic", main="")
                          