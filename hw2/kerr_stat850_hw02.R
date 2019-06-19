## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(knitr)
library(car)

set.seed(1104)             # make random results reproducible

this_file <- "kerr_stat850_hw02.Rmd"  # used to automatically generate code appendix

## ------------------------------------------------------------------------
#Problem 2a
#Read in donut data set
donut <- read.csv("~/2019spring/STAT850/hw2/donut.txt", sep="")
donut$fat = factor(donut$fat)
#Perform F test for one-way anova
donut_lm <- lm(donut$gfa ~ donut$fat)
anova(donut_lm)
#Plot raw/studentized residuals versus predicted values
plot(resid(donut_lm) ~ fitted(donut_lm))
plot(ls.diag(donut_lm)$stud.res ~ fitted(donut_lm))

## ------------------------------------------------------------------------
#Problem 2b
#Attempt to fit the overparametrized model
summary(lm(gfa ~ x1 + x2 + x3 + x4, data = donut))

## ------------------------------------------------------------------------
#Problem 2c
#Fit the models
lm1 = lm(gfa ~ x1 + x2 + x3, data = donut)
lm2 = lm(gfa ~ x3 + x2 + x1, data = donut)
lm3 = lm(gfa ~ z1 + z2 + z3, data = donut)
lm4 = lm(gfa ~ z3 + z2 + z1, data = donut)
#Perform anova for each model
anova1 = anova(lm1)
anova2 = anova(lm2)
anova3 = anova(lm3)
anova4 = anova(lm4)
#Demonstrate how to calculate F statistic
#This shows fitting order does not matter for overall F test
sum(anova1$`Mean Sq`[1:3]/3)/anova1$`Mean Sq`[4]
sum(anova2$`Mean Sq`[1:3]/3)/anova2$`Mean Sq`[4]
sum(anova3$`Mean Sq`[1:3]/3)/anova3$`Mean Sq`[4]
sum(anova4$`Mean Sq`[1:3]/3)/anova4$`Mean Sq`[4]

## ---- warning=FALSE------------------------------------------------------
#Problem 3a
#Load the data into R
g1 = c(15,15) %>% cbind(c(67,63))
g2 = rep(25,4) %>% cbind(c(62,71,62,67))
g3 = rep(100,7) %>% cbind(c(121,114,79,151,72,180,72))
pain = data.frame(rbind(g1,g2,g3)) 
pain = rename(pain,dose = `.`, time = V2)
pain$dose = factor(pain$dose)
#Make plot of data
scatter.smooth(pain)
#Perform anova
pain_lm = lm(time ~ dose, data = pain)
anova(pain_lm)
#Plot residuals
plot(ls.diag(pain_lm)$stud.res ~ fitted(pain_lm), ylab = "studentized residuals")

## ------------------------------------------------------------------------
#Problem 3b
#Perform Levene's test with Brown-Forsythe mod
leveneTest(pain_lm, center=median)

## ------------------------------------------------------------------------
#Problem 3c
#Perform kruskal test
kruskal.test(time ~ dose, data = pain)

## ------------------------------------------------------------------------
#Problem 3d
#Perform the transforms
pain_log <- mutate(pain, time = log(time))
pain_sqrt <- mutate(pain, time = sqrt(time))

#Look at the residuals
pain_log_lm <- lm(time ~ dose, data = pain_log)
pain_sqrt_lm <- lm(time ~ dose, data = pain_sqrt)
par(mfrow = c(1,2))
plot(ls.diag(pain_log_lm)$stud.res ~ fitted(pain_log_lm), ylab = "stud. residuals", main = "log transform", ylim = c(-2,2))
plot(ls.diag(pain_sqrt_lm)$stud.res ~ fitted(pain_sqrt_lm), ylab = "stud. residuals", main = "square root transform", ylim = c(-2,2))

## ----code = readLines(purl(this_file, documentation = 1)), echo = T, eval = F----
## # this R markdown chunk generates a code appendix

