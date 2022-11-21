# Violence as a Condition: Figures and Plots #
# Sky Kunkel #

#### Set Libraries, read in data ####
library(tidyverse); library(sp); library(ggstream); library(lubridate);
library(sf); library(rstatix); library(ggpubr); library(ggExtra); library(stargazer)
# read in data
options(scipen = 999) # turn off scientific notation
setwd("../")
a = read.csv("./data/Kunkel-Ellis-final.csv")
a$event_date = ymd(a$event_date)
a$wagner = "State Violence"
a$wagner[a$t_ind == 1] = "Wagner"

#### Run analyses, put in table #####
#### Analyses with binary instrument ####
first.stage = lm(t_ind ~ iv, data = a)
instrumented.trt = first.stage$fitted # Generate fitted values
reg1 <- lm(a$death ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg1)
reg2 <- lm(a$fatalities ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg2)

first.stage.1 = lm(t_ind ~ score, data = a)
instrumented.trt = first.stage.1$fitted # Generate fitted values
reg1 <- lm(a$death ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg1)
reg2 <- lm(a$fatalities ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg2)

# need to write this again
stargazer()

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


# Convert ACLED data to geolocational data

# assign crs system for ACLED data #
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# convert ACLED data to SP data
a.sp <- SpatialPointsDataFrame(a[20:19],         # reading in the dataframe as a spatial object
                                   a,                 # the R object to convert
                                   proj4string = wgs84)   # assign a CRS 
bbox_car <- st_bbox(car0)  #current bounding box

#### Plot of violence by actor in CAR ####

ggplot() + 
  geom_point(data = a, aes(x = longitude, y = latitude, size = fatalities, colour = wagner)) +
  geom_sf(aes(geometry = car2$geometry), alpha = 0) +
  geom_sf(aes(geometry = car0$geometry), alpha = 0) +
  xlim(bbox_car[1], bbox_car[3]) + ylim(bbox_car[2], bbox_car[4]) + theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

# plot violence since 2021 when Wagner starts ramping up operations #
a.wg = subset(a, wagner == "Wagner" & event_date >"2020-12-31")
a.st = subset(a, wagner == "State Violence" & event_date >"2020-12-31")
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
        plot.margin = unit(c(1,1,1,1), "cm"), legend.margin=margin(c(5,5,5,5)),
        legend.key.size = unit(0.05, 'cm'), legend.direction="horizontal") +
  guides(shape = guide_legend(order = 1),col = guide_legend(order = 2))
pdf("./results/violence_by_actor_21-22.pdf")
dsc
dev.off()

theme(legend.background = element_rect(color = "black"), legend.position = c(0.25, 0.3),
      plot.margin = unit(c(0,0,0,0), "cm"), legend.margin=margin(c(5,5,5,5)), 
      legend.key.size = unit(0.2, 'cm')) + 


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

ggplot(d, aes(x = event_date, y = death, fill = wagner)) +
  geom_stream(type = "ridge")
ggplot(d) +
  geom_line(aes(x = event_date, y = death, color = wagner))

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

ggplot(d, aes(x = event_date, y = death, fill = event_type)) +
  geom_stream(type = "ridge")
ggplot(d) +
  geom_line(aes(x = event_date, y = fatalities, color = wagner))


#### Plot Wagner Violence Severity over time ####
rm(list=ls())
a = read.csv("./data/Kunkel-Ellis-final.csv")
a$event_date = ymd(a$event_date)
a$wagner = "State Violence"
a$wagner[a$t_ind == 1] = "Wagner"

# Aggregate by week #
a$event_date <- floor_date(a$event_date, "month")
a = subset(a, t_ind == 1)
a = subset(a, event_type == "Battles" | event_type == "Violence against civilians")
d = a %>%
  group_by(event_date, event_type) %>%
  summarize(death = mean(death), fatalities = sum(fatalities)) %>%
  as.data.frame()

# make into plot
ggplot(d, aes(x = event_date, y = fatalities, fill = event_type)) +
  geom_stream(type = "ridge")
ggplot(data = a, mapping = aes(x = event_date, y=fatalities)) + 
  geom_density(stat = "identity")

# scatter plot
a$event = 1
boxplot(a$iv, a$event)



#### make boxplot by type of violence and treatment ####
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

# let's try making joyplot still need to log deaths?
library(ggridges) # geom_density_ridges_gradient
library(viridis)  # scale_fill_viridis
library(hrbrthemes) # theme_ipsum


round(mean(a$fatalities[a$iv == 0 & a$t_ind == 1]), digits = 2)
round(mean(a$fatalities[a$iv == 1 & a$t_ind == 1]), digits = 2)
round(mean(a$fatalities[a$iv == 0 & a$t_ind == 0]), digits = 2)
round(mean(a$fatalities[a$iv == 1 & a$t_ind == 0]), digits = 2)



a$act = NA
a$act[a$iv == 0 & a$t_ind == 1] = "Wagner, Pre-Ukraine"
a$act[a$iv == 1 & a$t_ind == 1] = "Wagner, Post-Ukraine"
a$act[a$iv == 0 & a$t_ind == 0] = "State, Pre-Ukraine"
a$act[a$iv == 1 & a$t_ind == 0] = "State, Post-Ukraine"
pdf("./results/violence_joyplot.pdf")
svg("./results/violence_joyplot.svg")
ggplot(a, aes(x = `fatalities`, y = `act`, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01, alpha = 0.5) +
  scale_fill_gradient(low = "#ff3b3b", high = "#000000", space = "Lab",
                      guide = "colourbar", aesthetics = "fill") +
  #  scale_fill_viridis(name = "fatalities", option = "e") +
  xlim(-1.75,26) + labs(title = 'Violence in the CAR, pre- and post-Ukraine') +
  theme_ipsum() + ylab("") +
  annotate(geom="text", x=21.5, y=4.5, label= "bar(x)", color="black", parse=T) +
  annotate(geom="text", x=23.5, y=4.5, label= "= 2.82", color="black") +
  annotate(geom="text", x=23.5, y=3.5, label="   5.24", color="black") +
  annotate(geom="text", x=23.5, y=2.5, label="   1.61", color="black") +
  annotate(geom="text", x=23.5, y=1.5, label="   1.86", color="black") +
  theme(plot.title = element_text(family="Times"), legend.position="none", panel.spacing = unit(0.1, "lines"), 
        strip.text.x = element_text(size = 8), 
        axis.title.x = element_text(size =12, hjust = 0.4))
dev.off()



#### scatter plot of violence since 2021-11-01 #####
rm(list=ls())
a = read.csv("./data/Kunkel-Ellis-final.csv")
a$event_date = ymd(a$event_date)
a$wagner = "State"
a$wagner[a$t_ind == 1] = "Wagner"
a$event_date <- floor_date(a$event_date, "week")
date = rep(ymd("2021-11-01"), nrow(a))
a$score = date - a$event_date
a$score = as.numeric(a$score)
a$score = a$score*(-1)
a = subset(a, score > -500)
d = a %>%
  group_by(score, wagner) %>%
  summarize(death = mean(death), fatalities = sum(fatalities)) %>%
  as.data.frame()

library(tidyquant)
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




#### Run some analyses, plot ####

## Naive analyses, regular OLS ##

b = a
b$fatalities[b$fatalities == 0] = 0.1
b$fatalities = log(b$fatalities)
table(b$fatalities)
fs = glm(t_ind ~ score, data = b)
is.trt = fs$fitted.values
ts = glm(b$death ~ is.trt)
ts2 = glm(b$fatalities ~ is.trt)
summary(ts)
summary(ts2)

fs = glm(t_ind ~ score, data = a)
is.trt = fs$fitted.values
ts = glm(a$death ~ is.trt)
ts2 = glm(a$fatalities ~ is.trt)
summary(ts)
summary(ts2)


# b = a
# b$fatalities[b$fatalities == 0] = 0.1
# b$fatalities = log(b$fatalities)
# table(b$fatalities)
# fs = glm(t_ind ~ iv, data = b)
# is.trt = fs$fitted.values
# ts = glm(b$death ~ is.trt)
# ts2 = glm(b$fatalities ~ is.trt)
# summary(ts)
# summary(ts2)




library(glm.predict)
## Analyses ##
reg0 = glm(death ~ t_ind, data = a)
reg00 <- glm(fatalities ~ t_ind, data = a, family = negative.binomial(theta = 1))
summary(reg0)
summary(reg00)

first.stage.1 = glm(t_ind ~ score, data = a)
instrumented.trt = first.stage.1$fitted # Generate fitted values
reg1 <- glm(a$death ~ instrumented.trt) # Second stage
summary(reg1)
reg2 <- glm(a$fatalities ~ instrumented.trt, family = negative.binomial(theta = 1)) # Second stage
summary(reg2)
library(ggeffects)
ggpredict(reg0, terms = "t_ind") # OLS, binary
ggpredict(reg00, terms = "t_ind") # NB, non-logged continuous
ggpredict(ts, terms = "is.trt") # OLS, binary (instrumented)
ggpredict(ts2, terms = "is.trt") # OLS, logged continuous (instrumented)
ggpredict(reg1, terms = "instrumented.trt") # OLS, binary (instrumented)
ggpredict(reg2, terms = "instrumented.trt") # NB, non-logged continuous (instrumented)


## Instrumented analyses ###
## Analyze Data ##
first.stage.1 = lm(t_ind ~ score, data = a)
instrumented.trt = first.stage.1$fitted # Generate fitted values
reg1 <- glm(a$death ~ instrumented.trt + a$fatalities.lag, family = negative.binomial(theta = 1)) # Second stage
summary(reg1)
reg2 <- glm(a$fatalities ~ instrumented.trt + a$fatalities.lag, family = negative.binomial(theta = 1)) # Second stage
summary(reg2)

# library(AER)
# summary(ivreg(fatalities ~ t_ind + fatalities.lag | iv, data = a))
# 
# ggplot(a, aes(instrumented.trt, fatalities)) +
#   geom_point() +
#   geom_smooth(method='lm') +  scale_x_continuous(breaks = seq(0,1,1)) +
#   ylim(0,50)


#### Odds Ratios Plots ####
reg1.cf = exp(reg2$coefficients) %>%
  as.data.frame()
reg1.ci = exp(confint(reg2)) %>%
  as.data.frame()
reg1.cf = cbind(reg1.cf, reg1.ci)
names(reg1.cf)[1] = "fatalities"
names(reg1.cf)[2] = "ci_low"
names(reg1.cf)[3] = "ci_high"
reg1.cf$row_names = row.names(reg1.cf)
y_labs = rev(c("Wagner Violence", "Fatalities Lag","State Violence (Intercept)"))
level_order = rev(c("Fatalities", "a$fatalities.lag", "(Intercept)"))
pdf("./results/or_death.pdf")
ggplot(reg1.cf, aes(y = factor(row_names, level = level_order), x = fatalities)) + 
  geom_vline(aes(xintercept = 1), size = .15, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height = 
                   .15, color = "gray50") + geom_point(size = 3, color = "#A52A2A") +
  theme_pubclean() + scale_y_discrete(labels = y_labs) + ylab("") +
  scale_x_continuous(limits = c(0,31), breaks = c(0,1,10,30)) +
  theme(plot.title = element_text(size = 18)) +
  xlab("Odds ratio") + ggtitle("Wagner v. State Violence Risk of Death by Actor")
dev.off()
