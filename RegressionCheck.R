rm(list = ls())
library(Hmisc)
library(quantmod)
library(dplyr)
ceo_data <- as.data.frame(read.csv("STATS/ceo.csv"))

ceo_data$SPCODE <- ceo_data$SPCODE
ceo_data$Gender<- ceo_data$GENDER
Graduation_Year_Graduate_School <- as.numeric(substr(ceo_data$Graduation_Year_Graduate_School,1,4))
Graduation_Year_Graduate_School <- as.numeric(ceo_data$Graduation_Year_Graduate_School != 0)




get_CEO_year <- as.numeric(substr(ceo_data$BECAMECEO,1,4))
get_undergrad <- as.numeric(substr(ceo_data$Graduation_Year_College,1,4))
                          
ceo_data$time_to_ceo<- as.numeric(substr(ceo_data$BECAMECEO,1,4))-(as.numeric(substr(ceo_data$Graduation_Year_College,1,4)))                                 
ceo_data$age_at_ceo <- as.numeric(get_CEO_year - as.numeric(substr(ceo_data$Birth_Year,1,4)))

avg_time <- mean(ceo_data$time_to_ceo)
avg_age <- mean(ceo_data$age_at_ceo)

reg1 <- lm(time_to_ceo ~ Gender + Graduation_Year_Graduate_School + ivy + SPCODE, data = ceo_data)
summary(reg1)



#-------#

movie_data$Sunday = relevel(movie_data$Sunday, ref = 3)
rm(list = ls())
> movie_data <- as.data.frame(read.csv("STATS/movie_theater_sales.csv"))
> 
  > movie_data$construction_per_ticket_sales <- movie_data$Ventas_Netas - movie_data$Asistencia
> movie_data$Sun = relevel(movie_data$Sunday, ref = 3)
Error in relevel.default(movie_data$Sunday, ref = 3) : 
  'relevel' only for factors
> reg_movie <- lm(Ingreso_Neto_Taquilla ~ promotion + numbered_seats + Sun, data = movie_data)
> 
  > reg_movie_2 <- lm(Ventas_Netas ~ promotion + numbered_seats + Sun, data = movie_data)