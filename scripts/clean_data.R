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
                                  regex("Wagner", ignore_case = TRUE))))
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
min(d$event_date[d$t_ind == 1])
table(d[d$event_date > "2018-04-07"])
