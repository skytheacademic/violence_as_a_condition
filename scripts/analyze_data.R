#### Violence as a Condition: Structure, Composition, and the Use of Lethal Force ####
### Analysis ###
## Sky Kunkel ##
## Personal site: https://www.skytheacademic.com
## Github repos: https://github.com/skytheacademic
# This script contains all the code need to reproduce every model, table, and figure 
# in the manuscript directly related to statistical models, from Section 5 "Analysis", 
# onwards. The code for descriptive statistics and figures can be found in "plot_data.R"

#### Set Libraries, read in data ####
library(tidyverse); library(lubridate); library(stargazer)
library(MASS); library(lmtest); library(sandwich)
library(rdrobust); library(rddapp); library(ivDiag)
options(scipen = 999)

## clear environment, set up working directory ##
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder

## read in data ##
a = read.csv("./data/Kunkel-Ellis-final.csv") %>%
  mutate(event_date = ymd(event_date))

#### Manuscript main models/tables ####
### 5 Analysis ###
## naive models ##
reg0 = glm(death ~ t_ind + fatalities.lag + gold + diam, data = a, 
       family = negative.binomial(theta = 1))
names(reg0$coefficients) = c("(Intercept)", "Treatment", "Fatalities Lag", "Gold", "Diamonds")
se_reg0 <- round(coeftest(reg0, vcov = vcovPL(reg0, cluster = a$admin1)),4)

reg00 <- glm(fatalities ~ t_ind + fatalities.lag + gold + diam, data = a, 
       family = negative.binomial(theta = 1))
names(reg00$coefficients) = c("(Intercept)", "Treatment", "Fatalities Lag", "Gold", "Diamonds")
se_reg00 <- round(coeftest(reg00, vcov = vcovPL(reg00, cluster = a$admin1)),4)

se_reg0
se_reg00

## binary instrument ##
first.stage = lm(t_ind ~ iv, data = a)
instrumented.trt = first.stage$fitted # Generate fitted values

# binary outcome #
reg1 <- lm(a$death ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
names(reg1$coefficients) = c("(Intercept)", "Treatment", "Fatalities Lag", "Gold", "Diamonds")
se_reg1 <- round(coeftest(reg1, vcov = vcovPL(reg1, cluster = a$admin1)),4)

# count outcome #
reg2 <- lm(a$fatalities ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
names(reg2$coefficients) = c("(Intercept)", "Treatment", "Fatalities Lag", "Gold", "Diamonds")
se_reg2 <- round(coeftest(reg2, vcov = vcovPL(reg2, cluster = a$admin1)),4)

se_reg1
se_reg2

## dose instrument ##
# generate instrument #
first.stage.1 = lm(t_ind ~ score, data = a)
instrumented.trt = first.stage.1$fitted # Generate fitted values

## models ##
# binary outcome #
reg3 <- lm(a$death ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
names(reg3$coefficients) = c("(Intercept)", "Treatment", "Fatalities Lag", "Gold", "Diamonds")
se_reg3 <- round(coeftest(reg3, vcov = vcovPL(reg3, cluster = a$admin1)),4)

# count outcome #
reg4 <- lm(a$fatalities ~ instrumented.trt + a$fatalities.lag + a$gold + a$diam) # Second stage
names(reg4$coefficients) = c("(Intercept)", "Treatment", "Fatalities Lag", "Gold", "Diamonds")
se_reg4 <- round(coeftest(reg4, vcov = vcovPL(reg4, cluster = a$admin1)),4)

se_reg3
se_reg4

#### Make into table ####
## clustered SE vectors from coeftest objects
se_list_nb  = list(
  se_reg0[,"Std. Error"],
  se_reg00[,"Std. Error"]
)

se_list_iv = list(
  se_reg1[,"Std. Error"],
  se_reg3[,"Std. Error"],
  se_reg2[,"Std. Error"],
  se_reg4[,"Std. Error"]
)

cov.labs = c("Treatment", "Fatalities Lag", "Gold", "Diamonds")

## TABLE 1 #
# naive NB models with clustered SEs
stargazer(
  reg0, reg00,
  se = se_list_nb,
  apply.coef = exp,
  t.auto = TRUE, p.auto = TRUE,
  style = "apsr",
  covariate.labels = cov.labs,
  label = "tab:nb",
  dep.var.labels = c("Death (B)", "Fatalities (C)"),
  notes = "Negative Binomial models with SEs clustered at the 1st-level administrative unit. Coefficients reported as odds ratios. (B) = Binary Outcome, (C) = Count Outcome.",
  out = "./results/logit.txt"
)

## TABLE 2 #
# IV models with clustered SEs
stargazer(
  reg1, reg3, reg2, reg4,
  se = se_list_iv,
  t.auto = TRUE, p.auto = TRUE,
  style = "apsr",
  covariate.labels = cov.labs,
  label = "tab:instrument",
  dep.var.labels = c("Death (B)", "Fatalities (C)"),
  notes = "2SLS regressions with SEs clustered at the 1st-level administrative unit. (B) = Binary Outcome, (C) = Count Outcome.",
  out = "./results/2sls.txt"
)

#### Appendix tables and figures ####

### C Instrumental Variable Validity ###
## robust f-testing ##
reg1 = ivDiag(
       data = a, 
       Y = "death", 
       D = "t_ind", 
       Z = "score",
       controls = c("fatalities.lag", "gold", "diam"),
       FE = "admin1",
       cl = "admin1",
       seed = 8675309 # hey jenny
); gc()

reg2 = ivDiag(
       data = a, 
       Y = "fatalities", 
       D = "t_ind", 
       Z = "score",
       controls = c("fatalities.lag", "gold", "diam"),
       FE = "admin1",
       cl = "admin1",
       seed = 8675309 # hey jenny
); gc()

reg3 = ivDiag(
       data = a, 
       Y = "death", 
       D = "t_ind", 
       Z = "iv",
       controls = c("fatalities.lag", "gold", "diam"),
       FE = "admin1",
       cl = "admin1",
       seed = 8675309 # hey jenny
); gc()

reg4 = ivDiag(
       data = a, 
       Y = "fatalities", 
       D = "t_ind", 
       Z = "iv",
       controls = c("fatalities.lag", "gold", "diam"),
       FE = "admin1",
       cl = "admin1",
       seed = 8675309 # hey jenny
); gc()

## first-stage & effective F by instrument (Binary outcome reps Dose/Binary)
feff_dose   = round(reg1$F_stat["F.robust"], 3)
feff_binary = round(reg3$F_stat["F.robust"], 3)

tf_dose     = round(reg1$tF["F"], 3)
tf_binary   = round(reg3$tF["F"], 3)

## AR by instrument x outcome
ar_dose_binary   = round(reg1$AR$Fstat["F"], 3)
ar_dose_count    = round(reg2$AR$Fstat["F"], 3)
ar_binary_binary = round(reg3$AR$Fstat["F"], 3)
ar_binary_count  = round(reg4$AR$Fstat["F"], 3)

## build LaTeX lines
lines = c(
  "\\begin{table}[!htbp] \\centering",
  "\\begin{tabular}{lccccc}",
  "\\hline",
  "Instrument & \\(F_{Eff}\\) & tF & AR (Binary) & AR (Count) \\\\",
  "\\hline",
  paste0(
    "Dose   & ", feff_dose,   " & ", tf_dose,   " & ",
    ar_dose_binary, " & ", ar_dose_count, " \\\\"
  ),
  paste0(
    "Binary & ", feff_binary, " & ", tf_binary, " & ",
    ar_binary_binary, " & ", ar_binary_count, " \\\\"
  ),
  "\\hline",
  "\\end{tabular}",
  "\\caption{Reported \\textcite{F}-statistics recommended by \\textcite{lal_how_2024}. ",
  "\\textit{Note: AR is reported separately for each outcome because the AR statistic is calculated from the reduced form of the outcome on the instrument and thus varies between binary and count models.}}",
  "\\label{tab:instrument_robust_tests}",
  "\\end{table}"
)

# Table A1 #
cat(paste(lines, collapse = "\n"))
writeLines(text = lines, con = "./results/instrument_robust_tests.txt")

### D Regression Discontinuity Robustness Check ###
# RDD Robustness check #
date = rep(ymd("2021-11-01"), nrow(a))
a$score_rdd = date - a$event_date
a$score_rdd = as.numeric(a$score_rdd)

#Use the MSE-optimal bandwidth to compute the treatment effect using a triangular kernel.
h = rdbwselect(a$death, a$score_rdd)$bws[1]
h1 = rdbwselect(a$fatalities, a$score_rdd)$bws[1]
a$kweights = rddapp:::wt_kern(a$score_rdd, 0, bw = h, kernel = "triangular")
a$kweights1 = rddapp:::wt_kern(a$score_rdd, 0, bw = h1, kernel = "triangular")

reg1 = lm(death ~ t_ind + score_rdd + t_ind*score_rdd, weights = kweights, data = a)
reg2 = lm(fatalities ~ t_ind + score_rdd + t_ind*score_rdd, weights = kweights1, data = a)

# Table A2 #
stargazer(reg1, reg2, style = "apsr", covariate.labels = c("Treatment", "Score", "Treatment * Score"),
          dep.var.labels = c("Pr(Fatality)", "Total Fatalities"), notes = "Regression Discontinuity Design output.",
          out = "./results/rdd.txt")

# McCrary Test statistic #
out = dc_test(a$score_rdd[abs(a$score_rdd) < h], 0, ext.out = TRUE)

tab = data.frame(
  theta = out$theta,
  se = out$se,
  z = out$z,
  p = out$p
)

colnames(tab) = c(
  "Log diff. (theta)",
  "Std. Error",
  "z-stat",
  "p-value"
)

# Table A3 #
stargazer(tab, summary = FALSE, rownames = FALSE,
          title = "McCrary Sorting Test",
          label = "tab:mccrary",
          digits = 3,
          out = "./results/mccrary_test.txt")

# Figure A1 #
# Density plot to examine sorting
pdf("./results/rdd_density.pdf")
dc_test(a$score_rdd[abs(a$score_rdd)<h],0)
dev.off()
# big gaps are bad here, and show evidence of sorting

# Figure A2a #
pdf("./results/rdd_death.pdf")
plot(a$score_rdd,a$kweights, xlab = "Score", ylab = "Weights")
dev.off()

# Figure A2b #
pdf("./results/rdd_fatalities.pdf")
plot(a$score_rdd,a$kweights1, xlab = "Score", ylab = "Weights")
dev.off()

#Provide an RD plot illustrating the treatment effect
# Figure A3a #
pdf("./results/rdplot_death.pdf")
rdplot(y = a$death, x = a$score_rdd,  h = h, nbins = 100, subset = -h <= a$score_rdd & a$score_rdd <= h, 
       binselect="esmv", kernel="triangular", p=1, title = "", 
       y.label = "Pr(Fatality)", x.label = "")
dev.off()

# Figure A3b #
pdf("./results/rdplot_fatalities.pdf")
rdplot(y = a$fatalities, x = a$score_rdd,  h = h, nbins = 100, subset = -h <= a$score_rdd & a$score_rdd <= h, 
       binselect="esmv", kernel="triangular", p=1, title = "", 
       y.label = "Total Fatalities", x.label = "", y.lim = c(0,10))
dev.off()

### E Region Fixed Effects ###
## naive models ##
reg0 = glm(death ~ t_ind + as.factor(admin1), data = a, 
       family = negative.binomial(theta = 1))
se_reg0 <- round(coeftest(reg0, vcov = vcovPL(reg0, cluster = a$admin1)),4)

reg00 <- glm(fatalities ~ t_ind + as.factor(admin1), data = a, 
       family = negative.binomial(theta = 1))
se_reg00 <- round(coeftest(reg00, vcov = vcovPL(reg00, cluster = a$admin1)),4)

## binary instrument ##
first.stage = lm(t_ind ~ iv + as.factor(admin1), data = a)
instrumented.trt = first.stage$fitted # Generate fitted values

# binary outcome #
reg1 <- lm(a$death ~ instrumented.trt + as.factor(a$admin1)) # Second stage
se_reg1 <- round(coeftest(reg1, vcov = vcovPL(reg1, cluster = a$admin1)),4)

# count outcome #
reg2 <- lm(a$fatalities ~ instrumented.trt + as.factor(a$admin1)) # Second stage
se_reg2 <- round(coeftest(reg2, vcov = vcovPL(reg2, cluster = a$admin1)),4)

## dose instrument ##
# generate instrument #
first.stage.1 = lm(t_ind ~ score + as.factor(admin1), data = a)
instrumented.trt = first.stage.1$fitted # Generate fitted values

## models ##
# binary outcome #
reg3 <- lm(a$death ~ instrumented.trt + as.factor(a$admin1)) # Second stage
se_reg3 <- round(coeftest(reg3, vcov = vcovPL(reg3, cluster = a$admin1)),4)

# count outcome #
reg4 <- lm(a$fatalities ~ instrumented.trt + as.factor(a$admin1)) # Second stage
se_reg4 <- round(coeftest(reg4, vcov = vcovPL(reg4, cluster = a$admin1)),4)

#### Make into table ####
## clustered SE vectors from coeftest objects
se_list_nb  = list(
  se_reg0[,"Std. Error"],
  se_reg00[,"Std. Error"]
)

se_list_iv = list(
  se_reg1[,"Std. Error"],
  se_reg3[,"Std. Error"],
  se_reg2[,"Std. Error"],
  se_reg4[,"Std. Error"]
)

# cov.labs = c("Treatment", "Fatalities Lag", "Gold", "Diamonds")

# Table A4 # 
# naive NB models with clustered SEs and fixed effects
stargazer(
  reg0, reg00,
  se = se_list_nb,
  apply.coef = exp,
  t.auto = TRUE, p.auto = TRUE,
  style = "apsr",
  label = "tab:nb_fe",
  dep.var.labels = c("Death (B)", "Fatalities (C)"),
  notes = "Negative Binomial models with SEs clustered at the 1st-level administrative unit. Coefficients reported as odds ratios. (B) = Binary Outcome, (C) = Count Outcome.",
  omit = "as.factor\\(admin1\\)",                                         # remove FE
  add.lines = list(
    c("Fixed Effects", "$\\checkmark$", "$\\checkmark$")                               # ✓ under each model
  ),
  out = "./results/logit_region_fe.txt"
)

# Table A5 #
# IV models with clustered SEs and fixed effects 
stargazer(
  reg1, reg3, reg2, reg4,
  se = se_list_iv,
  t.auto = TRUE, p.auto = TRUE,
  style = "apsr",
  label = "tab:2sls_fe",
  dep.var.labels = c("Death (B)", "Death (B)", "Fatalities (C)", "Fatalities (C)"),
  notes = "2SLS regressions with SEs clustered at the 1st-level administrative unit. (B) = Binary Outcome, (C) = Count Outcome.",
  omit = "admin1\\)",
  add.lines = list(
    c("Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
  ),
  out = "./results/2sls_region_fe.txt"
)
