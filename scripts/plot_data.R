#### Violence as a Condition: Structure, Composition, and the Use of Lethal Force ####
### Data Plotting ###
## Sky Kunkel ##
## Personal site: https://www.skytheacademic.com
## Github repos: https://github.com/skytheacademic
# This script contains all the code need to reproduce the descripts maps/figures in 
# the manuscript not related to the statistical analysis.

#### Set Libraries, read in data ####
library(tidyverse); library(sp); library(lubridate)
library(sf); library(rstatix); library(ggpubr)
library(ggExtra); library(stargazer); library(cowplot)

# read in data
options(scipen = 999) # turn off scientific notation

## clear environment, set up working directory ##
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder

## read in data ##
a = read.csv("./data/Kunkel-Ellis-final.csv")

# modify columns to make plotting/figures more clear and easy to make
a$event_date = ymd(a$event_date)
a$wagner = "State Violence"
a$wagner[a$t_ind == 1] = "Wagner"


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

# Convert ACLED data to geolocational data
# assign crs system for ACLED data #
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# convert ACLED data to SP data
a.sp <- SpatialPointsDataFrame(a[20:19],         # reading in the dataframe as a spatial object
                                   a,                 # the R object to convert
                                   proj4string = wgs84)   # assign a CRS 
bbox_car <- st_bbox(car0)  #current bounding box

#### Plot of violence by actor in CAR ####
## note: this plot was created and submitted during the first submission to JCR. This was later
# revised into three figures per one reviewer's request. The code remains here for transparency,
# but is otherwise not in the manuscript or appendix. 
ggplot() + 
  geom_point(data = a, aes(x = longitude, y = latitude, size = fatalities, colour = wagner)) +
  geom_sf(aes(geometry = car2$geometry), alpha = 0) +
  geom_sf(aes(geometry = car0$geometry), alpha = 0) +
  xlim(bbox_car[1], bbox_car[3]) + ylim(bbox_car[2], bbox_car[4]) + theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

# plot violence by group #
a.wg = subset(a, wagner == "Wagner")
a.st = subset(a, wagner == "State Violence")
dsc.1 =
  ggplot() + geom_sf(aes(geometry = car0$geometry), alpha = 0.7,fill = "white") +
  geom_sf(aes(geometry = car2$geometry), alpha = 0) +
  geom_point(data = a.wg, aes(x = longitude, y = latitude, size=fatalities, colour = "#000000"), alpha=0.4, shape = 19) +
  geom_point(data = a.st, aes(x = longitude, y = latitude, size=fatalities, colour = "#A52A2A"), alpha=0.5, shape = 19) +
  scale_fill_viridis_c(option="E") +
  scale_size(range = c(.1, 20), name="Fatalities Count", labels = c("25", "50", "75", "100", "125"), 
             breaks = c(25, 50, 75, 100, 125)) +
  theme_void()

dsc =
  dsc.1 + labs(colour = "Actor") +
  scale_color_manual(labels = c("Wagner", "State Forces"), values = c("#000000", "#A52A2A")) +
  theme(legend.background = element_rect(color = "black"), legend.position = c(0.5, -0.125),
        plot.margin = unit(c(1,1,1,1), "cm"), legend.margin = margin(5, 5, 5, 5, "pt"),
        legend.key.size = unit(0.05, 'cm'), legend.direction="horizontal") +
  guides(shape = guide_legend(order = 1),col = guide_legend(order = 2))
pdf("./results/violence_by_actor_21-22.pdf")
dsc
dev.off()

## same plot but by year ##
# 2020
a2020 = subset(a, year == 2020)
a2020.wg = subset(a2020, wagner == "Wagner")
a2020.st = subset(a2020, wagner == "State Violence")

p_2020 =
  ggplot() +
  geom_sf(aes(geometry = car0$geometry), alpha = 0.7, fill = "white") +
  geom_sf(aes(geometry = car2$geometry), alpha = 0) +
  geom_point(data = a2020.wg, aes(x = longitude, y = latitude, size = fatalities),
             colour = "#000000", alpha = 0.4, shape = 19) +
  geom_point(data = a2020.st, aes(x = longitude, y = latitude, size = fatalities),
             colour = "#A52A2A", alpha = 0.5, shape = 19) +
  scale_size(range = c(.1, 20),
             breaks = c(25, 50, 75, 100, 125),
             labels = c("25", "50", "75", "100", "125"),
             guide = "none") +
  theme_void()

pdf("./results/violence_by_actor_2020.pdf", width = 5, height = 6)
p_2020
dev.off()

# 2021
a2021 = subset(a, year == 2021)
a2021.wg = subset(a2021, wagner == "Wagner")
a2021.st = subset(a2021, wagner == "State Violence")

p_2021 =
  ggplot() +
  geom_sf(aes(geometry = car0$geometry), alpha = 0.7, fill = "white") +
  geom_sf(aes(geometry = car2$geometry), alpha = 0) +
  geom_point(data = a2021.wg, aes(x = longitude, y = latitude, size = fatalities),
             colour = "#000000", alpha = 0.4, shape = 19) +
  geom_point(data = a2021.st, aes(x = longitude, y = latitude, size = fatalities),
             colour = "#A52A2A", alpha = 0.5, shape = 19) +
  scale_size(range = c(.1, 20),
             breaks = c(25, 50, 75, 100, 125),
             labels = c("25", "50", "75", "100", "125"),
             guide = "none") +
  theme_void()

pdf("./results/violence_by_actor_2021.pdf", width = 5, height = 6)
p_2021
dev.off()

# 2022
a2022 = subset(a, year == 2022)
a2022.wg = subset(a2022, wagner == "Wagner")
a2022.st = subset(a2022, wagner == "State Violence")

p_2022 =
  ggplot() +
  geom_sf(aes(geometry = car0$geometry), alpha = 0.7, fill = "white") +
  geom_sf(aes(geometry = car2$geometry), alpha = 0) +
  geom_point(data = a2022.wg, aes(x = longitude, y = latitude, size = fatalities),
             colour = "#000000", alpha = 0.4, shape = 19) +
  geom_point(data = a2022.st, aes(x = longitude, y = latitude, size = fatalities),
             colour = "#A52A2A", alpha = 0.5, shape = 19) +
  scale_size(range = c(.1, 20),
             breaks = c(25, 50, 75, 100, 125),
             labels = c("25", "50", "75", "100", "125"),
             guide = "none") +
  theme_void()

pdf("./results/violence_by_actor_2022.pdf", width = 5, height = 6)
p_2022
dev.off()

# legend only plot #
dsc_legend =
  dsc.1 +
  labs(colour = "Actor") +
  scale_color_manual(labels = c("Wagner", "State Forces"),
                     values = c("#000000", "#A52A2A")) +
  scale_size(range = c(.1, 20),
             name = "Fatalities Count",
             breaks = c(25, 50, 75, 100, 125),
             labels = c("25", "50", "75", "100", "125")) +
  theme_void() +
  theme(
    legend.background = element_rect(color = "black"),
    legend.position = "bottom",
    legend.margin = margin(5, 5, 5, 5, "pt"),
    legend.key.size = unit(0.05, "cm"),
    legend.direction = "horizontal"
  )

leg = cowplot::get_legend(dsc_legend)
legend_plot = cowplot::ggdraw(leg)

pdf("./results/violence_by_actor_legend.pdf")
legend_plot
dev.off()

#### Plot Wagner Violence Severity over time ####
rm(list=ls())
a = read.csv("./data/Kunkel-Ellis-final.csv")
a$event_date = ymd(a$event_date)
a$wagner = "State Violence"
a$wagner[a$t_ind == 1] = "Wagner"

# joyplot #
library(ggridges) # geom_density_ridges_gradient
library(viridis)  # scale_fill_viridis
# library(hrbrthemes) # theme_ipsum
## note: the R version I am using is different from when I first started this project.
  # current version: R version 4.5.1 (2025-06-13 ucrt) -- "Great Square Root"
# while most code still works, theme_ipsum does not work anymore :(
# See: install.packages("hrbrthemes")
# package ‘hrbrthemes’ is not available for this version of R
# I located the original code creating theme_ipsum and am pasting it below. you can find
# that code here: 
# https://github.com/hrbrmstr/hrbrthemes/blob/d3fd02949fc201c6db616ccaffbb9858aec6fd2b/R/theme-ipsum.r
theme_ipsum <- function(base_family="Arial Narrow", base_size = 11.5,
                        plot_title_family=base_family, plot_title_size = 18,
                        plot_title_face="bold", plot_title_margin = 10,
                        subtitle_family=base_family, subtitle_size = 12,
                        subtitle_face = "plain", subtitle_margin = 15,
                        strip_text_family = base_family, strip_text_size = 12,
                        strip_text_face = "plain",
                        caption_family = base_family, caption_size = 9,
                        caption_face = "italic", caption_margin = 10,
                        axis_text_size = base_size,
                        axis_title_family = subtitle_family, axis_title_size = 9,
                        axis_title_face = "plain", axis_title_just = "rt",
                        plot_margin = margin(30, 30, 30, 30),
                        grid_col = "#cccccc", grid = TRUE,
                        axis_col = "#cccccc", axis = FALSE, ticks = FALSE) {

  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)

  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + theme(panel.grid=element_line(color=grid_col, linewidth=0.2))
    ret <- ret + theme(panel.grid.major=element_line(color=grid_col, linewidth=0.2))
    ret <- ret + theme(panel.grid.minor=element_line(color=grid_col, linewidth=0.15))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }

  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color="#2b2b2b", linewidth=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color=axis_col, linewidth=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color=axis_col, linewidth=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color=axis_col, linewidth=0.15))
      ret <- ret + theme(axis.line.y=element_line(color=axis_col, linewidth=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(linewidth=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(linewidth=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(linewidth=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  ret <- ret + theme(axis.text.x=element_text(size=axis_text_size, margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(size=axis_text_size, margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y.right=element_text(hjust=yj, size=axis_title_size, angle=90,
                                                     family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)

  ret
}



round(mean(a$fatalities[a$iv == 0 & a$t_ind == 1 & a$fatalities > 0]), digits = 2)
# [1] 5.89
round(mean(a$fatalities[a$iv == 1 & a$t_ind == 1 & a$fatalities > 0]), digits = 2)
# [1] 9.07
round(mean(a$fatalities[a$iv == 0 & a$t_ind == 0 & a$fatalities > 0]), digits = 2)
# [1] 2.42
round(mean(a$fatalities[a$iv == 1 & a$t_ind == 0 & a$fatalities > 0]), digits = 2)
# [1] 1.74

# build grouping variable
a$act = NA
a$act[a$iv == 0 & a$t_ind == 1] = "Wagner, Pre-Ukraine"
a$act[a$iv == 1 & a$t_ind == 1] = "Wagner, Post-Ukraine"
a$act[a$iv == 0 & a$t_ind == 0] = "State, Pre-Ukraine"
a$act[a$iv == 1 & a$t_ind == 0] = "State, Post-Ukraine"
# svg("./results/violence_joyplot.svg")
joy = ggplot(a, aes(x = `fatalities`, y = `act`, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, alpha = 0.5) +
  scale_fill_gradient(low = "#ff3b3b", high = "#000000", space = "Lab",
                      guide = "colourbar", aesthetics = "fill") +
  #  scale_fill_viridis(name = "fatalities", option = "e") +
  xlim(-1.75,26) + labs(title = 'Violence in the CAR, pre- and post-Ukraine') +
  theme_ipsum() + 
  ylab("") +
  annotate(geom="text", x=20.5, y=4.5, label= "bar(x)", color="black", parse=T) +
  annotate(geom="text", x=23.5, y=4.5, label= "= 5.89", color="black") +
  annotate(geom="text", x=23.5, y=3.5, label="   9.07", color="black") +
  annotate(geom="text", x=23.5, y=2.5, label="   2.42", color="black") +
  annotate(geom="text", x=23.5, y=1.5, label="   1.74", color="black") +
  # theme_pubr() + 
  theme(plot.title = element_text(family="Times"), legend.position="none", panel.spacing = unit(0.1, "lines"), 
        strip.text.x = element_text(size = 8), 
        axis.title.x = element_text(size =12, hjust = 0.4)) +
  xlab("Fatalities")

## Figure 1 ##
cairo_pdf("./results/violence_joyplot.pdf", width = 7, height = 5)
joy
dev.off()

### descriptive statistics for the context of fatalities in the CAR ###
rm(list = ls())
# load data #
d = read.csv("./data/acled/1900-01-01-2022-12-10-Central_African_Republic.csv") %>%
  select(-c(iso, event_id_cnty, event_id_no_cnty, 
            region, source, source_scale, timestamp)) %>%
  filter(event_type %in% c("Violence against civilians", "Battles"))

summary_by_type = bind_rows(
  ## FULL sample: 1997–2022
  d %>%
    filter(event_type %in% c("Violence against civilians", "Battles")) %>%
    mutate(
      event_type_period = ifelse(
        event_type == "Violence against civilians",
        "OSV (1997--2022)",
        "Battles (1997--2022)"
      )
    ),
  ## SUBSET: 2018–2022
  d %>%
    filter(
      event_type %in% c("Violence against civilians", "Battles"),
      year >= 2018
    ) %>%
    mutate(
      event_type_period = ifelse(
        event_type == "Violence against civilians",
        "OSV (2018--2022)",
        "Battles (2018--2022)"
      )
    )
) %>%
  group_by(event_type_period) %>%
  summarise(
    n_events          = n(),
    mean_fatalities   = mean(fatalities, na.rm = TRUE),
    median_fatalities = median(fatalities, na.rm = TRUE),
    prop_zero         = mean(fatalities == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    event_type_period = factor(
      event_type_period,
      levels = c(
        "OSV (1997--2022)",
        "OSV (2018--2022)",
        "Battles (1997--2022)",
        "Battles (2018--2022)"
      )
    )
  ) %>%
  arrange(event_type_period)

## Table 4 ##
stargazer(
  as.data.frame(summary_by_type),
  type = "latex",
  summary = FALSE,
  rownames = FALSE,
  digits = 2,
  title = paste(
    "Event-Level Fatalities by Event Type and Period, CAR.",
    "\\textit{Note: Battle fatalities are included for context,",
    "but are not otherwise present in any statistical models.}"
  ),
  label = "tab:event_fatalities_by_type",
  covariate.labels = c(
    "Event type",
    "\\# of events",
    "\\shortstack{Mean}",
    "\\shortstack{Median}",
    "\\shortstack{Zero-fatality\\\\events}"
  ),
  out = "./results/car_violence_over_time.txt"
)


#### scatter plot of violence since 2021-11-01 #####
## note: this figure was created and submitted during the first submission to JCR. This was later
# removed per one reviewers request. The code remains here for transparency, but is otherwise not 
# in the manuscript or appendix. 

rm(list=ls())
a = read.csv("./data/Kunkel-Ellis-final.csv") %>%
  mutate(event_date = ymd(event_date)) %>%
  mutate(event_date = floor_date(event_date, "week"))
a$wagner = "State"
a$wagner[a$t_ind == 1] = "Wagner"
date = rep(ymd("2021-11-01"), nrow(a))
a$score = date - a$event_date
d = a %>%
  mutate(score = score*(-1)) %>%
  filter(score > -500) %>%
  group_by(score, wagner) %>%
  summarize(death = mean(death), fatalities = sum(fatalities)) %>%
  as.data.frame()

library(tidyquant); library(ggpubr)
death = 
  ggplot(d) + 
  geom_point(aes(x = score, y = fatalities, colour = wagner)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "black") +
  geom_ma(ma_fun = SMA, n = 14, aes(x = score, y = fatalities, 
                                   colour = wagner, linetype = "solid")) +
  xlab("Days Before and After Nov. 1, 2021") + ylab("Fatalities") +
  labs(colour = "Actor") + guides(linetype = "none") + ylim(0,120) +
  scale_color_manual(labels = c("State Forces", "Wagner"), values = c("#A52A2A", "#000000")) +
  theme_pubr()

# with marginal histogram
pdf("./results/death_scatter.pdf")
death
dev.off()


#### make boxplot by type of violence and treatment ####
## note: this figure was created for an early version of the manuscript, but was later removed
# in favor of other figures. The code remains here for transparency, but is otherwise not in 
# the manuscript or appendix. 
rm(list=ls())
a = read.csv("./data/Kunkel-Ellis-final.csv")
a$event_date = ymd(a$event_date)
a$wagner = "State"
a$wagner[a$t_ind == 1] = "Wagner"
a$log.f = log(a$fatalities)

desc = ggboxplot(a, x = "iv", y = "log.f", add = c("jitter"), 
                 color = "wagner", palette = "lancet") + 
  geom_vline(xintercept = 1.5, linetype = "longdash", color = "black")



pdf("./results/desc.time.pdf")
ggpar(desc, main = "Violence Before and After Nov. 2021", xlab = "Before (0) / After (1)",
      ylab = "Log Fatalities", legend.title = "Actor", legend = c(0.1,0.90))
dev.off()
