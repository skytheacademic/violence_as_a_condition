# Sky Kunkel #
# The Wagner Group and Violence: Russia's Re-Entry Into Cold War African Politics #
# Data Cleaning #
library(tidyverse)
setwd("../")

# load data #
d <- read.csv("./data/acled/1900-01-01-2022-06-16-Central_African_Republic.csv") %>%
  select(-c(data_id, iso, event_id_cnty, event_id_no_cnty, 
            region, source, source_scale, timestamp))
table(d$actor1)
table(d$actor2)
table(d$assoc_actor_1)
table(d$assoc_actor_2)


dd = subset(d, actor1 == "Wagner Group" | actor2 == "Wagner Group" | 
             assoc_actor_1 == "Wagner Group" | assoc_actor_2 == "Wagner Group")

a = str_detect(d$notes, regex("Wagner", ignore_case = TRUE))
a = data.frame(a)
a = cbind(d, a)
a = subset(a, a == "TRUE") %>%
  select(-c("a"))

aa = subset(a, actor1 != "Wagner Group" & actor2 != "Wagner Group" & 
              assoc_actor_1 != "Wagner Group" & assoc_actor_2 != "Wagner Group")

b = str_detect(d$notes, regex("Wagner", ignore_case = TRUE))


# IMPORTANT #
### Eventually will need to find a way to distinguish these; ignore for now ###


df = subset(d, d$actor1 == "Wagner Group")
