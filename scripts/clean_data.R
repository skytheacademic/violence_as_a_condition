# Sky Kunkel #
# The Wagner Group and Violence: Russia's Re-Entry Into Cold War African Politics #
# Data Cleaning #
library(tidyverse); library(lubridate)
library(rdrobust); library(rdd)
setwd("../")
options(scipen = 999) # turn off scientific notation

#### Clean Data ####
# load data #
d = read.csv("./data/acled/1900-01-01-2022-09-01-Central_African_Republic.csv") %>%
  select(-c(iso, event_id_cnty, event_id_no_cnty, 
            region, source, source_scale, timestamp))

# create treatment, marking when Wagner was present for DV
d = d %>%
  rowwise() %>%
  mutate(t_ind = +any(str_detect(across(.cols = everything()), 
                                  regex("Wagner", ignore_case = TRUE)))) %>%
  as.data.frame()

# change date to date-time object
d$event_date = dmy(d$event_date) 

# make IV of death
d$death = 0
d$death[d$fatalities > 0] = 1
table(d$death)


# make IVs of types of violence
table(d$event_type)
d$battle = 0
d$battle[d$event_type == "Battles"] = 1
d$remote = 0
d$remote[d$event_type == "Explosions/Remote violence"] = 1
d$protest = 0
d$protest[d$event_type == "Protests"] = 1
d$riot = 0
d$riot[d$event_type == "Riots"] = 1
d$str_d = 0
d$str_d[d$sub_event_type == "Arrests" | d$sub_event_type == "Looting/property destruction"] = 1
d$vac = 0
d$vac[d$event_type == "Violence against civilians"] = 1

# expand the data to cover all time periods #
min(d$event_date[d$t_ind == 1]) # establish first time period to receive "treatment"
# "2018-04-08"
d = subset(d, d$event_date > "2018-04-07") # subset to all data after 2018-04-07
range(d$event_date) # verify it worked


#### Instrumental variable ####
# make the instrument
table(d$t_ind[d$event_date> "2021-11-01"])
d$iv = 0
d$iv[d$event_date> "2021-11-01"] = 1


first.stage.1 = lm(t_ind ~ iv, data = d)
instrumented.trt = first.stage.1$fitted # Generate fitted values
reg1 <- lm(d$death ~ instrumented.trt) # Second stage
summary(reg1)
reg2 <- lm(d$fatalities ~ instrumented.trt) # Second stage
summary(reg2)


#### RDD ####
date = rep(ymd("2021-11-01"), nrow(d))
d$score = date - d$event_date
d$score = as.numeric(d$score)
#Use the MSE-optimal bandwidth to compute the treatment effect using a triangular kernel.
h = rdbwselect(d$death, d$score)$bws[1]
h     

d$kweights = kernelwts(d$score, 0, bw = h, kernel = "triangular")
sum(d$kweights!=0 & !is.na(d$death))
plot(d$score,d$kweights)

reg1 = lm(death ~ t_ind + score + t_ind*score, weights = kweights, data = d)
summary(reg1)

#Provide an RD plot illustrating the treatment effect
rdplot(y = d$death, x = d$score,  h = h, nbins = 100, subset = -h <= d$score & d$score <= h, 
       binselect="esmv", kernel="triangular", p=1, title = "", 
       y.label = "", x.label = "")

summary(rdrobust(d$death,d$score,all=TRUE))

# Density plot to examine sorting
DCdensity(d$score[abs(d$score)<h],0)
# big gaps are bad here, and show evidence of sorting





#### Plot of treatment by admin? ####
# group only by admin to see which units were ever treated
d.ag.test = d %>%
  group_by(admin3) %>%
  summarize(t_ind = sum(t_ind))
d.ag.test$t_ind[d.ag.test$t_ind>0] = 1
table(d.ag.test$t_ind)
# we have 46 adm3 where Wagner was never present, and 96 where they were

# group by admin3 and treatment, then left_join(date,d, by = "date")
table(d$event_date)
d.ag = d %>%
  group_by(admin3, event_date, t_ind) %>%
  summarize(death = (death), fatalities = sum(fatalities), battle = sum(battle),
            remote = sum(remote), protest = sum(protest), riot = sum(riot), 
            str_d = sum(str_d), vac = sum(vac))
# need to verify that this^^^ worked correctly #



# this could be missing days when Wagner committed violence but another violent event happened 
# that wasn't "treated". Need to solve later


date = seq.Date(from = as.Date("2018-04-08"), to = as.Date("2022-08-10"), by = 1) %>%
  as.data.frame()
names(date)[1] = "date" # rename for merging

a = full_join(d, date, by = c("event_date" = "date")) 
