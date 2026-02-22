#### Violence as a Condition: Structure, Composition, and the Use of Lethal Force ####
### Cleaning Data ###
## Sky Kunkel ##
## Personal site: https://www.skytheacademic.com
## Github repos: https://github.com/skytheacademic
# This script contains all the code need to create the data used in the 
# "analyze_data.R" and "plot_data.R"
library(tidyverse); library(lubridate); library(sf)
options(scipen = 999) # turn off scientific notation

#### Clean Data ####
## clear environment, set up working directory ##
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../")

# load data #
d = read.csv("./data/acled/1900-01-01-2022-12-10-Central_African_Republic.csv") %>%
  select(-c(
    iso, event_id_cnty, event_id_no_cnty, region, source, source_scale, timestamp
  ))

# create treatment indicator for Wagner presence
d = d %>%
  rowwise() %>%
  mutate(t_ind = +any(str_detect(across(.cols = everything()), 
                                  regex("Wagner", ignore_case = TRUE)))) %>%
  # keeping anything with a 1, 3, 7, 8, as these are all state/external actor based
  filter(inter1 %in% c(1, 3, 7, 8) | inter2 %in% c(1, 3, 7, 8)) %>%
  mutate(event_date = dmy(event_date))

#subsetting by times it was possible to receive the Wagner "treatment"
min(d$event_date[d$t_ind == 1]) # establish first time period to receive "treatment"
# "2018-04-08"
d = d %>%
  filter(event_date > ymd("2018-04-07")) # subset to all data after 2018-04-07
range(d$event_date) # verify it worked

# don't want to overcount violence; so using the interaction codes, we're going to
# drop data that shouldn't be counted in the analysis. we explain our reasoning for 
# each in the appendix

# drop non-state/PMC related events: militia-only, rebel-only,
# civilian-only, and external forces movement
drop_codes <- tibble(
  inter1 = c(2, 2, 3, 3, 3, 3, 3, 4, 5, 7, 8),
  inter2 = c(3, 7, 0, 3, 4, 5, 7, 7, 7, 0, 0)
) 
# drops: militia-rebel (2-3, 2-7), militia-only (3-0), militia infighting
  # (3-3, 3-4), militia v. rioters (3-5), militia/rioters v. civilians
  # (3-7, 4-7, 5-7), civilian-only (7-0), external forces movement (8-0)
d <- d %>%
  anti_join(drop_codes, by = c("inter1", "inter2")) %>%
  filter(event_type != "Battles")

rm(drop_codes)
# make dependent variable of death
d <- d %>%
  mutate(death = as.integer(fatalities > 0))
table(d$death)

#### Instrumental variable ####
# check the data to examine general trends
table(d$t_ind[d$event_date> "2021-11-01"])
table(d$t_ind[d$event_date< "2021-11-01"])
mean(d$fatalities[d$event_date > "2021-11-01" & d$t_ind == 1])
mean(d$fatalities[d$event_date < "2021-11-01" & d$t_ind == 1])
# make the instrument
d <- d %>%
  mutate(iv = as.integer(event_date > ymd("2021-11-01")))

#### Creating/merging control variables ####
a <- read.csv("./data/acled/1900-01-01-2022-12-10-Central_African_Republic.csv") %>%
  select(-c(iso, event_id_cnty, event_id_no_cnty, 
            region, source, source_scale, timestamp)) %>%
  # subset to any events by militia or rebels
  filter(inter1 %in% c(2, 3, 4)) %>% 
  # create sum of events in the past 30 days #
  mutate(event = 1, # make this for sum of violent events
         event_date = dmy(event_date),
         # subtract one month to make this a lagged variable
         event_date = floor_date(event_date, "month") - months(1),
         month = month(event_date)) %>%
  # group by month and year
  group_by(year, month) %>%
  summarize(event.lag = sum(event), fatalities.lag = sum(fatalities), .groups = "drop")

# merge violent events by month
d = d %>%
  mutate(month = month(event_date)) %>%
  left_join(a, by = c("year", "month"), relationship = "many-to-one")

# load in gold and diamond controls
prio.static = read_csv("./data/prio/PRIO-GRID Static Variables - 2022-11-07.csv")
prio.yearly = read_csv("./data/prio/PRIO-GRID Yearly Variables for 1946-2014 - 2022-11-07.csv")

prio.static = prio.static %>%
  rename(prio.grid = gid) %>% # rename for merging
  mutate(prio.grid = as.character(prio.grid),
    ## PRIO's data is coded as 1 or NA, where the NA values are actually 0s 
      # (e.g., no gold mines in a PRIO grid would be coded as NA)
    # Replace NAs w/ zeros
         across(c(diamsec_s, diamprim_s, goldplacer_s, goldvein_s, goldsurface_s), 
                ~replace_na(., 0)),
         diam = as.integer((diamsec_s + diamprim_s) > 0),
         gold = as.integer((goldplacer_s + goldsurface_s + goldvein_s) > 0)) %>%
  select(prio.grid, diam, gold)

prio.yearly = prio.yearly %>%
  rename(prio.grid = gid) %>% # rename for merging
  mutate(prio.grid = as.character(prio.grid),
         across(c(diamsec_y, diamprim_y, goldplacer_y, goldvein_y, goldsurface_y), 
                ~replace_na(., 0)),
         diam = as.integer((diamsec_y + diamprim_y) > 0),
         gold = as.integer((goldplacer_y + goldsurface_y + goldvein_y) > 0)) %>%
  select(prio.grid, diam, gold) %>%
  ## Diamond and gold caches are static/non-moving, at least on a non-geological 
  # time-scale
    # group by grid
  group_by(prio.grid) %>%
  summarize(diam = as.integer(sum(diam) > 0), 
            gold = as.integer(sum(gold) > 0), .groups = "drop")

# merge prio variables
a = prio.yearly %>% 
  left_join(prio.static, by = "prio.grid", suffix = c("_y", "_s")) %>%
  mutate(diam = as.integer((diam_y + diam_s) > 0),
         gold = as.integer((gold_y + gold_s) > 0),
         prio.grid = as.numeric(prio.grid)) %>%
  select(prio.grid, diam, gold)

rm(prio.static, prio.yearly)

# read in PRIO shape files from their website
# prio uses WGS84 CRS
prio = st_read(dsn = "./data/prio/shp", 
               layer = "priogrid_cell", 
               stringsAsFactors = F) %>% 
  rename(prio.grid = gid) %>%  # rename for merging
  # transform the column into numeric so we can join the data
  mutate(prio.grid = as.numeric(prio.grid)) %>%
  left_join(a, by = "prio.grid") %>%
  mutate(diam = replace_na(diam, 0),
         gold = replace_na(gold, 0))

rm(a)
# spatial join: map gold/diamond data to individual violence events
# convert ACLED points to sf, join with PRIO polygons
d_sf = st_as_sf(d, coords = c("longitude", "latitude"), 
  # set to the PRIO CRS
  crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
d = st_join(d_sf, prio) %>%
  select(-c(prio.grid, xcoord, ycoord, col, row))

rm(d_sf, prio)
# create score variable for RDD
d = d %>%
  mutate(score = as.numeric(ymd("2021-11-01") - event_date),
        score = score * (-1),
        score = if_else(score < 1, 0, score),
        score = log(score + 1))

## final edits/cleaning in prep for exporting
# can't export a geometry column to a CSV
d = d %>%
  mutate(
    longitude = st_coordinates(geometry)[, 1],
    latitude  = st_coordinates(geometry)[, 2]
  ) %>%
  relocate(c(latitude, longitude), .after = location)


#### Export Data ####
saveRDS(d, "./data/Kunkel-Ellis-final.RDS")

d_csv = d %>% 
  as.data.frame() %>% 
  select(-geometry)
write.csv(d_csv, "./data/Kunkel-Ellis-final.csv", row.names=FALSE)

# for all the Stata users out there, I haven't forgotten you :)
d_stata = d %>%
  rename(event_lag = event.lag, fatalities_lag = fatalities.lag) %>%
  as.data.frame() %>%
  select(-geometry)
haven::write_dta(d_stata, "./data/Kunkel-Ellis-final.dta")

rm(d, d_csv, d_stata)

#### Version Control ####
sessionInfo()
# R version 4.5.1 (2025-06-13 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26200)

# Matrix products: default
#   LAPACK version 3.12.1

# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

# time zone: America/New_York
# tzcode source: internal

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
#  [1] sf_1.0-23       lubridate_1.9.4 forcats_1.0.1   stringr_1.5.2   dplyr_1.1.4     purrr_1.1.0     readr_2.1.5     tidyr_1.3.1     tibble_3.3.0   
# [10] ggplot2_4.0.0   tidyverse_2.0.0

# loaded via a namespace (and not attached):
#  [1] s2_1.1.9           generics_0.1.4     class_7.3-23       KernSmooth_2.23-26 stringi_1.8.7      hms_1.1.3          magrittr_2.0.4    
#  [8] grid_4.5.1         timechange_0.3.0   RColorBrewer_1.1-3 e1071_1.7-16       DBI_1.2.3          scales_1.4.0       cli_3.6.5         
# [15] rlang_1.1.6        crayon_1.5.3       units_1.0-0        bit64_4.6.0-1      withr_3.0.2        tools_4.5.1        parallel_4.5.1    
# [22] tzdb_0.5.0         vctrs_0.6.5        R6_2.6.1           proxy_0.4-27       lifecycle_1.0.4    classInt_0.4-11    bit_4.6.0         
# [29] vroom_1.6.6        foreign_0.8-90     pkgconfig_2.0.3    pillar_1.11.1      gtable_0.3.6       glue_1.8.0         Rcpp_1.1.0        
# [36] haven_2.5.5        tidyselect_1.2.1   rstudioapi_0.17.1  farver_2.1.2       wk_0.9.4           compiler_4.5.1     S7_0.2.0   