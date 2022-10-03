# Violence as a Condition: Figures and Plots #
# Sky Kunkel #

#### Set Libraries, read in data ####
library(tidyverse); library(sp); library(ggstream); library(lubridate)

# read in data
setwd("../")
a = read.csv("./data/Kunkel-Ellis-final.csv")
car0 = st_read(dsn = "./data/humdata/adm0", 
              layer = "caf_admbnda_adm0_200k_sigcaf_reach_itos_v2", 
              stringsAsFactors = F)
car2 = st_read(dsn = "./data/humdata/adm2", 
               layer = "caf_admbnda_adm2_200k_sigcaf_reach_itos_v2", 
               stringsAsFactors = F)
car3 = st_read(dsn = "./data/humdata/adm3", 
               layer = "caf_admbnda_adm3_200k_sigcaf_reach_itos_v2", 
               stringsAsFactors = F)

# Convert ACLED data to geolocational data

# assign crs system for ACLED data #
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# convert ACLED data to SP data
a.sp <- SpatialPointsDataFrame(a[20:19],         # reading in the dataframe as a spatial object
                                   a,                 # the R object to convert
                                   proj4string = wgs84)   # assign a CRS 
a$wagner = "State Violence"
a$wagner[a$t_ind == 1] = "Wagner"
ggplot() + geom_point(data = a, aes(x = longitude, y = latitude, size = fatalities, colour = wagner))


ggplot() + geom_sf(aes(fill = b.join.0$fatalities, geometry = b.join.0$prio_geometry)) +
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,2050)) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) + 
  geom_sf(aes(geometry = uga_01$geometry), alpha = 0) +
  geom_sf(aes(geometry = uga_00$geometry), size = 2, fill = alpha("red",0)) +
  geom_point(data = b.join.0, aes(x = prio_xcoord, y = prio_ycoord, size=radpko_pko_deployed_any), alpha=0.4, shape = 19, colour = "#5b92e5") +
  xlim(29.12,31.38) + ylim(0.61,2.88) + theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.position="none")


#### Plot violence severity by treatment over time ####
d = a
d$event_date = ymd(d$event_date)
d$event_date <- floor_date(d$event_date, "month")
d = subset(d, d$event_date > "2020-12-01")
d = d %>%
  group_by(event_date, wagner) %>%
  summarize(death = mean(death), fatalities = sum(fatalities)) %>%
  as.data.frame()
d$event_date = ymd(d$event_date)

ggplot(d, aes(x = event_date, y = fatalities, fill = wagner)) +
  geom_stream(type = "ridge")
ggplot(d) +
  geom_line(aes(x = event_date, y = fatalities, color = wagner))

plot(d$wagner, d$event_date)

#### Plot of Violent Events over time by event type ####
d = a
d$event_date = ymd(d$event_date)
d$event_date <- floor_date(d$event_date, "month")
d = subset(d, event_type == "Battles" | event_type == "Violence against civilians")
d = d %>%
  group_by(event_date, event_type) %>%
  summarize(death = mean(death), fatalities = sum(fatalities)) %>%
  as.data.frame()
d$event_date = ymd(d$event_date)

ggplot(d, aes(x = event_date, y = fatalities, fill = event_type)) +
  geom_stream(type = "ridge")
ggplot(d) +
  geom_line(aes(x = event_date, y = fatalities, color = wagner))

