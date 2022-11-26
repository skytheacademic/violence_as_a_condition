# Violence as a Condition: Figures and Plots #
# Sky Kunkel #

#### Set Libraries, read in data ####
library(tidyverse); library(lubridate); library(stargazer)
options(scipen = 999)
setwd("../")
a = read.csv("./data/Kunkel-Ellis-final.csv")
a$event_date = ymd(a$event_date)



#### Naive analyses ####
reg0 = lm(death ~ t_ind + fatalities.lag + gold + diam, data = a)
reg00 <- lm(fatalities ~ t_ind + fatalities.lag + gold + diam, data = a)
summary(reg0)
summary(reg00)


#### Analyses with binary instrument ####
first.stage = lm(t_ind ~ iv, data = a)
instrumented.trt = first.stage$fitted # Generate fitted values
reg1 <- lm(a$death ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg1)
reg2 <- lm(a$fatalities ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg2)


#### Analyses with dose instrument ####
first.stage.1 = lm(t_ind ~ score, data = a)
instrumented.trt = first.stage.1$fitted # Generate fitted values
reg3 <- lm(a$death ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg3)
reg4 <- lm(a$fatalities ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg4)



#### Make into table ####
cov.labs = c("Treatment", "Fatalities Lag", "Gold", "Diamonds")
stargazer(reg0, reg00, 
          style = "ajps", covariate.labels = cov.labs, dep.var.labels =  c("Death (B)", "Fatalities (C)"))

stargazer(reg1, reg3, reg2, reg4, 
          style = "ajps", covariate.labels = cov.labs, dep.var.labels =  c("Death (B)", "Fatalities (C)"))

