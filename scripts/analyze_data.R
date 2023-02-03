# Violence as a Condition: Figures and Plots #
# Sky Kunkel #

#### Set Libraries, read in data ####
library(tidyverse); library(lubridate); library(stargazer); library(MASS)
options(scipen = 999)
setwd("../")
a = read.csv("./data/Kunkel-Ellis-final.csv") %>%
  mutate(event_date = ymd(event_date))



#### Naive analyses ####
reg0 = glm(death ~ t_ind + fatalities.lag + gold + diam, data = a, family = negative.binomial(theta = 1))
reg00 <- glm(fatalities ~ t_ind + fatalities.lag + gold + diam, data = a, family = negative.binomial(theta = 1))
summary(reg0)
summary(reg00)


#### Analyses with binary instrument ####
first.stage = lm(t_ind ~ iv, data = a)
instrumented.trt = first.stage$fitted # Generate fitted values
reg1 <- lm(a$death ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg1)
reg2 <- lm(a$fatalities ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg2)


#### Analyses with dose instrument ####
first.stage.1 = lm(t_ind ~ score, data = a)
instrumented.trt = first.stage.1$fitted # Generate fitted values
reg3 <- lm(a$death ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg3)
reg4 <- lm(a$fatalities ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
summary(reg4)



#### Make into table ####
cov.labs = c("Treatment", "Fatalities Lag", "Gold", "Diamonds")
stargazer(reg0, reg00, apply.coef = exp, t.auto=F, p.auto=F,
          style = "apsr", covariate.labels = cov.labs, dep.var.labels =  c("Death (B)", "Fatalities (C)"),
          notes = "Negative Binomial Logit Models transformed into odds ratios. (B) = Binary Outcome, (C) = Count Outcome.", 
          out = "./results/logit.txt")

stargazer(reg1, reg3, reg2, reg4, 
          style = "apsr", covariate.labels = cov.labs, dep.var.labels =  c("Death (B)", "Fatalities (C)"),
          notes = "2-Stage Least Squares Regression. (B) = Binary Outcome, (C) = Count Outcome.",
          out = "./results/2sls.txt")


#### Appendix tables and figures ####

# Print Instrument Tables #
stargazer(first.stage, first.stage.1, style = "apsr", covariate.labels = c("Binary Instrument", "Continuous Instrument"),
          dep.var.labels = "Treatment", out = "./results/st_1.txt")


# RDD Robustness check #
library(rdrobust); library(rdd)
date = rep(ymd("2021-11-01"), nrow(a))
a$score_rdd = date - a$event_date
a$score_rdd = as.numeric(a$score_rdd)
#Use the MSE-optimal bandwidth to compute the treatment effect using a triangular kernel.
h = rdbwselect(a$death, a$score_rdd)$bws[1]
h1 = rdbwselect(a$fatalities, a$score_rdd)$bws[1]
a$kweights = kernelwts(a$score_rdd, 0, bw = h, kernel = "triangular")
a$kweights1 = kernelwts(a$score_rdd, 0, bw = h1, kernel = "triangular")

pdf("./results/rdd_death.pdf")
plot(a$score_rdd,a$kweights, xlab = "Score", ylab = "Weights")
dev.off()

pdf("./results/rdd_fatalities.pdf")
plot(a$score_rdd,a$kweights1, xlab = "Score", ylab = "Weights")
dev.off()

####

reg1 = lm(death ~ t_ind + score_rdd + t_ind*score_rdd, weights = kweights, data = a)
reg2 = lm(fatalities ~ t_ind + score_rdd + t_ind*score_rdd, weights = kweights1, data = a)

stargazer(reg1, reg2, style = "apsr", covariate.labels = c("Treatment", "Score", "Treatment * Score"),
          dep.var.labels = c("Pr(Fatality)", "Total Fatalities"), notes = "Regression Discontinuity Design output.",
          out = "./results/rdd.txt")

#Provide an RD plot illustrating the treatment effect
pdf("./results/rdplot_death.pdf")
rdplot(y = a$death, x = a$score_rdd,  h = h, nbins = 100, subset = -h <= a$score_rdd & a$score_rdd <= h, 
       binselect="esmv", kernel="triangular", p=1, title = "", 
       y.label = "Pr(Fatality)", x.label = "")
dev.off()
pdf("./results/rdplot_fatalities.pdf")
rdplot(y = a$fatalities, x = a$score_rdd,  h = h, nbins = 100, subset = -h <= a$score_rdd & a$score_rdd <= h, 
       binselect="esmv", kernel="triangular", p=1, title = "", 
       y.label = "Total Fatalities", x.label = "", y.lim = c(0,10))
dev.off()

# Density plot to examine sorting
pdf("./results/rdd_density.pdf")
DCdensity(a$score_rdd[abs(a$score_rdd)<h],0)
dev.off()
# big gaps are bad here, and show evidence of sorting


#### OLD - TO DELETE ####

# Instrument tests
# X = as.matrix(subset(a, select = c(fatalities.lag, gold, diam)))
# # Replace NAs w/ 0s
# X <- X %>% 
#   as.tibble() %>%
#   mutate(across(fatalities.lag:diam, 
#                 ~replace_na(.x, 0))) %>%
#     as.matrix()
# 
# test1 = ivmodel(Y = a$death, D = a$t_ind, Z = a$iv, X = X)
# summary(test1)
# iv.diagnosis.plot(iv.diagnosis(Y = a$death, D = a$t_ind, Z = a$iv, X = X), bias.ratio = F)
# test2 = ivmodel(Y = a$fatalities, D = a$t_ind, Z = a$iv, X = X)
# summary(test2)
# test3 = ivmodel(Y = a$death, D = a$t_ind, Z = a$score, X = X)
# summary(test3)
# test4 = ivmodel(Y = a$fatalities, D = a$t_ind, Z = a$score, X = X)
# summary(test4)