# Sky Kunkel #
# The Wagner Group and Violence: Russia's Re-Entry Into Cold War African Politics #
# Data Cleaning #
library(tidyverse); library(lubridate)
setwd("../")

# load data #
d = read.csv("./data/acled/1900-01-01-2022-09-01-Central_African_Republic.csv") %>%
  select(-c(iso, event_id_cnty, event_id_no_cnty, 
            region, source, source_scale, timestamp))

# create treatment, marking when Wagner was present for DV
d = 
  d %>%
  rowwise() %>%
  mutate(t_ind = +any(str_detect(across(.cols = everything()), 
                                  regex("Wagner", ignore_case = TRUE)))) %>%
  as.data.frame()
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

table(d$admin3)

# expand the data to cover all time periods #
min(d$event_date[d$t_ind == 1]) # establish first time period to receive "treatment"
# "2018-04-08"
d = subset(d, d$event_date > "2018-04-07") # subset to all data after 2018-04-07
range(d$event_date) # verify it worked

# group by admin3 and treatment, then left_join(date,d, by = "date")
table(d$event_date)
d.ag = d %>%
  group_by(admin3, event_date, t_ind) %>%
  summarize(death = (death), fatalities = sum(fatalities), battle = sum(battle),
            remote = sum(remote), protest = sum(protest), riot = sum(riot), 
            str_d = sum(str_d), vac = sum(vac))
###### need to verify that this^^^ worked correctly #######



# this could be missing days when Wagner committed violence but another violent event happened 
# that wasn't "treated". Need to solve later


date = seq.Date(from = as.Date("2018-04-08"), to = as.Date("2022-08-10"), by = 1) %>%
  as.data.frame()
names(date)[1] = "date" # rename for merging

a = full_join(d, date, by = c("event_date" = "date")) 
