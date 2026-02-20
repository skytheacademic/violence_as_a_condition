# Sky Kunkel #
# Violence as a Condition: Cleaning Data #
library(tidyverse); library(lubridate); library(sf); library(sp); library(spatialEco)
options(scipen = 999) # turn off scientific notation

#### Clean Data ####
## clear environment, set up working directory ##
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../")
# load data #
d = read.csv("./data/acled/1900-01-01-2022-12-10-Central_African_Republic.csv") %>%
  select(-c(iso, event_id_cnty, event_id_no_cnty, 
            region, source, source_scale, timestamp))

# don't want to overcount violence; so using the interaction codes, we're going to
# drop data that shouldn't be counted in the analysis. we explain our reasoning for 
# each in the appendix
# keeping anything with a 1, 3, 7, 8

# remove violence by rebels, sole protests, and 

# create treatment, marking when Wagner was present for DV
d = d %>%
  rowwise() %>%
  mutate(t_ind = +any(str_detect(across(.cols = everything()), 
                                  regex("Wagner", ignore_case = TRUE)))) %>%
  filter(inter1 == 1 | inter1 == 3 | inter1 == 7 | inter1 == 8 |
           inter2 == 1 | inter2 == 3 | inter2 == 7 | inter2 == 8) %>%
  mutate(event_date = dmy(event_date)) %>%
  as.data.frame()

#subsetting by times it was possible to receive the Wagner "treatment"
min(d$event_date[d$t_ind == 1]) # establish first time period to receive "treatment"
# "2018-04-08"
d = subset(d, d$event_date > "2018-04-07") # subset to all data after 2018-04-07
range(d$event_date) # verify it worked

table(d$t_ind)

d = d[!(d$inter1 ==2 & d$inter2 == 3), ] # drop data where only militia and rebels present
d = d[!(d$inter1 ==2 & d$inter2 == 7), ] # drop data where only rebels and militia present
d = d[!(d$inter1 ==3 & d$inter2 == 0), ] # drop data where only militias present
d = d[!(d$inter1 ==3 & d$inter2 == 3), ] # drop data where militias fight each other
d = d[!(d$inter1 ==3 & d$inter2 == 4), ] # drop data where militias fight each other
d = d[!(d$inter1 ==3 & d$inter2 == 5), ] # drop data where militias fight rioters
d = d[!(d$inter1 ==3 & d$inter2 == 7), ] # drop data where militias attack civilians
d = d[!(d$inter1 ==4 & d$inter2 == 7), ] # drop data where militias attack civilians
d = d[!(d$inter1 ==5 & d$inter2 == 7), ] # drop data where rioters attack civilians
d = d[!(d$inter1 ==7 & d$inter2 == 0), ] # drop data where civilians only are present
d = d[!(d$inter1 ==8 & d$inter2 == 0), ] # drop data on external forces movement
d = d[!(d$event_type == "Battles"), ] # drop data on battles


# make dependent variable of death
d$death = 0
d$death[d$fatalities > 0] = 1
table(d$death)

#### Instrumental variable ####
# make the instrument
table(d$t_ind[d$event_date> "2021-11-01"])
table(d$t_ind[d$event_date< "2021-11-01"])
t = subset(d, t_ind == 1)
mean(t$fatalities[t$event_date > "2021-11-01"])
mean(t$fatalities[t$event_date < "2021-11-01"])
d$iv = 0
d$iv[d$event_date> "2021-11-01"] = 1

#### Creating/merging control variables ####
a = read.csv("./data/acled/1900-01-01-2022-12-10-Central_African_Republic.csv") %>%
  select(-c(iso, event_id_cnty, event_id_no_cnty, 
            region, source, source_scale, timestamp)) %>%
  filter(inter1 == 2 | inter1 == 3 | inter1 == 4) %>% # subset to any events by militia or rebels
  mutate(event = 1) %>% # make this for sum of violent events
  mutate(event_date = dmy(event_date)) 

# create sum of events in the past 30 days #
# group by month and year
a$event_date = floor_date(a$event_date, "month")
a$event_date = a$event_date - months(1) # subtract one month to make this a lagged variable
a$month = month(a$event_date)
a = a %>%
  group_by(year, month) %>%
  summarize(event.lag = sum(event), fatalities.lag = sum(fatalities)) %>%
  as.data.frame()

d$month = month(d$event_date)

# merge violent events by month
d = left_join(d, a, by = c("year", "month"))

# load in gold and diamond controls
prio.static = read_csv("./data/prio/PRIO-GRID Static Variables - 2022-11-07.csv")
prio.yearly = read_csv("./data/prio/PRIO-GRID Yearly Variables for 1946-2014 - 2022-11-07.csv")

names(prio.static)[1] = "prio.grid" # rename for merging
names(prio.yearly)[1] = "prio.grid" # rename for merging
prio.static$prio.grid = as.character(prio.static$prio.grid)
prio.yearly$prio.grid = as.character(prio.yearly$prio.grid)

# get rid of NAs
prio.static$diamsec_s[is.na(prio.static$diamsec_s)] <- 0
prio.static$diamprim_s[is.na(prio.static$diamprim_s)] <- 0
prio.static$diam = prio.static$diamsec_s + prio.static$diamprim_s
prio.static$diam[prio.static$diam == 2] <- 1
prio.static$goldplacer_s[is.na(prio.static$goldplacer_s)] <- 0
prio.static$goldvein_s[is.na(prio.static$goldvein_s)] <- 0
prio.static$goldsurface_s[is.na(prio.static$goldsurface_s)] <- 0
prio.static$gold = prio.static$goldplacer_s + prio.static$goldsurface_s + prio.static$goldvein_s
prio.static$gold[prio.static$gold > 1] <- 1
prio.static = subset(prio.static, select = -c(2:10) )

# get rid of NAs
prio.yearly$diamsec_y[is.na(prio.yearly$diamsec_y)] <- 0
prio.yearly$diamprim_y[is.na(prio.yearly$diamprim_y)] <- 0
prio.yearly$diam = prio.yearly$diamsec_y + prio.yearly$diamprim_y
prio.yearly$diam[prio.yearly$diam == 2] <- 1
prio.yearly$goldplacer_y[is.na(prio.yearly$goldplacer_y)] <- 0
prio.yearly$goldvein_y[is.na(prio.yearly$goldvein_y)] <- 0
prio.yearly$goldsurface_y[is.na(prio.yearly$goldsurface_y)] <- 0
prio.yearly$gold = prio.yearly$goldplacer_y + prio.yearly$goldsurface_y + prio.yearly$goldvein_y
prio.yearly$gold[prio.yearly$gold > 1] <- 1
prio.yearly = subset(prio.yearly, select = -c(3:7) )

# group by grid to get rid of year variable
prio.yearly = prio.yearly %>%
  group_by(prio.grid) %>%
  summarize(diam = sum(diam), gold = sum(gold)) %>%
  as.data.frame()
prio.yearly$gold[prio.yearly$gold > 1] <- 1
prio.yearly$diam[prio.yearly$diam > 1] <- 1

# merge prio variables
a = left_join(prio.yearly, prio.static, by = "prio.grid")
a$diam = a$diam.x + a$diam.y
a$gold = a$gold.x + a$gold.y
a$gold[a$gold > 1] <- 1
a$diam[a$diam > 1] <- 1
a = subset(a, select = -c(2:5) )
a$prio.grid = as.numeric(a$prio.grid)

rm(prio.static, prio.yearly)

# read in PRIO shape files from their website
# prio uses WGS84 CRS
prio = st_read(dsn = "./data/prio/shp", 
               layer = "priogrid_cell", 
               stringsAsFactors = F) %>% 
  mutate(gid = as.character(gid))

names(prio)[1] = "prio.grid" # rename for merging
prio$prio.grid = as.numeric(prio$prio.grid) # transform the column into numeric so we can join the data
prio = left_join(prio, a)
prio$diam[is.na(prio$diam)] <- 0
prio$gold[is.na(prio$gold)] <- 0

# transform both datasets into  spatial objects
prio.sp = as(prio, Class = "Spatial") # 

# assign crs system for ACLED data #
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

d <- SpatialPointsDataFrame(d[20:19],         # reading in the dataframe as a spatial object
                                   d,                 # the R object to convert
                                   proj4string = wgs84)   # assign a CRS 


# map the gold and diamond data to individual violence
b = point.in.poly(d, prio.sp, sp = TRUE, duplicate = TRUE)
d = as.data.frame(b) # convert to dataframe

d = d %>%
  select(-c(prio.grid, xcoord, ycoord, col, row, coords.x1, coords.x2))

date = rep(ymd("2021-11-01"), nrow(d))
d$score = date - d$event_date
d$score = as.numeric(d$score)
d$score = d$score*(-1)
d$score[d$score < 1] = 0
d$score = log(d$score + 1)

#### Export Data ####
write.csv(d, "./data/Kunkel-Ellis-final.csv", row.names=FALSE)


