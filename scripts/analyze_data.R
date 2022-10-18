# Violence as a Condition: Figures and Plots #
# Sky Kunkel #

#### Set Libraries, read in data ####
library(tidyverse); library(lubridate)

setwd("../")
a = read.csv("./data/Kunkel-Ellis-final.csv")
a$event_date = ymd(a$event_date)



#### Naive analyses ####
reg0 = lm(death ~ t_ind + fatalities.lag, data = a)
reg00 <- lm(fatalities ~ t_ind + fatalities.lag, data = a)
summary(reg0)
summary(reg00)


#### Analyses with instrument ####
first.stage.1 = lm(t_ind ~ iv, data = a)
instrumented.trt = first.stage.1$fitted # Generate fitted values
reg1 <- lm(a$death ~ instrumented.trt) # Second stage
summary(reg1)
reg2 <- lm(a$fatalities ~ instrumented.trt) # Second stage
summary(reg2)
