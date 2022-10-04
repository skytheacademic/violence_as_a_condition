# Violence as a Condition: Figures and Plots #
# Sky Kunkel #

#### Set Libraries, read in data ####
library(tidyverse); library(sp); library(ggstream); library(lubridate)
library(sf); library(tmap); library(tmaptools)
# read in data
setwd("../")
a = read.csv("./data/Kunkel-Ellis-final.csv")

#### Read in map data ####
car0 = st_read(dsn = "./data/gadm/caf", 
              layer = "gadm40_CAF_0", 
              stringsAsFactors = F)
car1 = st_read(dsn = "./data/gadm/caf", 
               layer = "gadm40_CAF_1", 
               stringsAsFactors = F)
car2 = st_read(dsn = "./data/gadm/caf", 
               layer = "gadm40_CAF_2", 
               stringsAsFactors = F)

# chd0 = st_read(dsn = "./data/gadm/chd", 
#                layer = "gadm41_TCD_0", 
#                stringsAsFactors = F)
# cmr0 = st_read(dsn = "./data/gadm/cmr", 
#                layer = "gadm41_CMR_0", 
#                stringsAsFactors = F)
# cog0 = st_read(dsn = "./data/gadm/congo", 
#                layer = "gadm40_COG_0", 
#                stringsAsFactors = F)
# drc0 = st_read(dsn = "./data/gadm/drc", 
#                layer = "gadm40_COD_0", 
#                stringsAsFactors = F)
# ssd0 = st_read(dsn = "./data/gadm/ssd", 
#                layer = "gadm40_SSD_0", 
#                stringsAsFactors = F)
# sdn0 = st_read(dsn = "./data/gadm/sdn", 
#                layer = "gadm41_SDN_0", 
#                stringsAsFactors = F)

a$wagner = "State Violence"
a$wagner[a$t_ind == 1] = "Wagner"

# Convert ACLED data to geolocational data

# assign crs system for ACLED data #
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# convert ACLED data to SP data
a.sp <- SpatialPointsDataFrame(a[20:19],         # reading in the dataframe as a spatial object
                                   a,                 # the R object to convert
                                   proj4string = wgs84)   # assign a CRS 
bbox_car <- st_bbox(car0)  #current bounding box

ggplot() + 
  geom_point(data = a, aes(x = longitude, y = latitude, size = fatalities, colour = wagner)) +
  geom_sf(aes(geometry = car2$geometry), alpha = 0) +
  geom_sf(aes(geometry = car0$geometry), alpha = 0) +
  xlim(bbox_car[1], bbox_car[3]) + ylim(bbox_car[2], bbox_car[4]) + theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

a.wg = subset(a, wagner == "Wagner")
a.st = subset(a, wagner == "State Violence")
# dsc.1 =
  ggplot() + geom_sf(aes(geometry = car0$geometry), alpha = 0.3,fill = NA) +
  geom_point(data = a.wg, aes(x = longitude, y = latitude, size=fatalities, colour = "#e5695b"), alpha=0.4, shape = 19) +
  geom_point(data = a.st, aes(x = longitude, y = latitude, size=fatalities, colour = "#5b92e5"), alpha=0.5, shape = 19) +
  scale_fill_viridis_c(option="E") +
  scale_size(range = c(.1, 20), name="Count", labels = c("20,000", "40,000", "60,000"), breaks = c(20000, 40000,60000)) +
  theme_void()

dsc =
  dsc.1 + labs(colour = "Variable") +
  scale_color_manual(labels = c("PKs Deployed", "Violence"), values = c("#5b92e5", "#e5695b")) +
  theme(legend.background = element_rect(color = "black"), legend.position = c(0.25, 0.3),
        plot.margin = unit(c(0,0,0,0), "cm"), legend.margin=margin(c(5,5,5,5)),
        legend.key.size = unit(0.2, 'cm')) +
  guides(shape = guide_legend(order = 1),col = guide_legend(order = 2), legend.direction="vertical")
pdf("./results/violence_car")
dsc
dev.off()



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

