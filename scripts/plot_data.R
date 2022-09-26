# Violence as a Condition: Figures and Plots #
# Sky Kunkel #

#### Set Libraries, read in data ####
library(tidyverse); library(sp)
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


# assign crs system for ACLED data #
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# convert ACLED data to SP data
a.sp <- SpatialPointsDataFrame(a[20:19],         # reading in the dataframe as a spatial object
                                   a,                 # the R object to convert
                                   proj4string = wgs84)   # assign a CRS 
