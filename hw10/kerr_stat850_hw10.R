## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(knitr)
library(car)
library(ggplot2)
library(MASS)
library(lme4)
library(lmerTest)
library(tidyr)
library(cowplot)
library(multcomp)
library(lsmeans)

set.seed(1104)             # make random results reproducible

this_file <- "kerr_stat850_hw10.Rmd"  # used to automatically generate code appendix

## ------------------------------------------------------------------------
#Problem 1
potroast_num <- potroast <- read.csv("~/2019spring/STAT850/hw10/potroast.csv")
potroast$day = factor(potroast$day)
potroast$temperature = factor(potroast$temperature)

#Make a suitable plot to begin analysis
ggplot(potroast, aes(x = day, y = cooktime, color = pan)) + aes(group = pan, y = cooktime) + geom_point(aes(shape = temperature), size = 4) + geom_line()

## ------------------------------------------------------------------------
#Problem 2a
roast_fit <- lmer(cooktime ~ temperature*pan + (1|day), data = potroast)
anova(roast_fit)

## ---- warning=FALSE, error=FALSE, message=FALSE--------------------------
#Problem 2b
roast_fit <- lmer(cooktime ~ temperature*pan + (1|day), data = potroast)
lsmeans(roast_fit, pairwise ~ temperature)

## ------------------------------------------------------------------------
#Problem 3b
# Fit with each day having its own slope and intercept
potroast_num$day = factor(potroast_num$day)
roast_fit <- lmer(cooktime ~ temperature*pan + (1|day), data = potroast_num)

## ------------------------------------------------------------------------
#Problem 3b
#Remove the interaction term
roast_fit_null <- update(roast_fit, . ~ . -temperature:pan, REML = FALSE)
 
#Perform the LRT
anova(roast_fit_null, roast_fit)

## ---- warning=FALSE, error=FALSE, message=FALSE--------------------------
#Problem 3b
#Define likelihood ratio function
lr = function(m){
  #Add interaction back
  m.withint = with(m@frame, lmer(cooktime ~ temperature*pan + (1|day), REML=FALSE))
  #Calculate LRT statistic
  x2 = as.numeric(2*(logLik(m.withint) - logLik(m)))
  
  return(c(lr = x2))
}

#Now, perform bootstrap
set.seed(4)
if (!exists('bag')) {
  #Perform the bootstrap
  bag = bootMer(roast_fit_null, lr, nsim = 2000)
}
bag_df <- as.data.frame(bag)

#Now find the p-value
bag_df$lr[bag_df$lr<0] = 0.0
pval = mean(bag_df$lr >= bag$t0["lr"])
paste("The p-value for the parametric bootstrap test is: ",pval)

#Plot the null distribution
hist(bag_df$lr, freq = FALSE, breaks = 30, xlab = "-2 loglik ratio", main = "")
curve(dchisq(x,df=2), add = T, n = 100)

## ------------------------------------------------------------------------
#Problem 3b
anova(roast_fit)

## ------------------------------------------------------------------------
#Problem 3c
 
#Perform the LRT
ranova(roast_fit)

## ---- warning=FALSE, error=FALSE, message=FALSE--------------------------
#Problem 3c
#Define bootstrap function
oneX2 = function(x){
  #Define null model to generate data
  null_model = lm(cooktime ~ temperature*pan, data = potroast_num)
  y = simulate(null_model)$sim_1
  
  #Make alternative model from data simulated by null
  alt_model = lmer(y ~ temperature*pan + (1|day), data = potroast_num, REML = FALSE)
  null_model_fit = lm(y ~ temperature*pan, data = potroast_num)
  
  #Return the LRT statistic
  x2 = as.numeric(2*(logLik(alt_model) - logLik(null_model_fit)))
  x2 = ranova(alt_model)$LRT[2]
  
  return(x2)
}

#Now, perform bootstrap
set.seed(4)
if (!exists('bag_c')) {
  #Perform the bootstrap
  bag_c = unlist(parallel::mclapply(1:2000,oneX2))
}
bag_df <- as.data.frame(bag_c)

#Now find the p-value
bag_df$bag_c[bag_df$bag_c<0] = 0.0
pval = mean(bag_df$bag_c >= 9.7877)
paste("The p-value for the parametric bootstrap test is: ",pval)

#Plot the null distribution
hist(bag_df$bag_c, freq = FALSE, breaks = 30, xlab = "-2 loglik ratio", main = "")
curve(dchisq(x,df=1), add = T, n = 100)

## ------------------------------------------------------------------------
#Problem 3d
#No interaction
roast_fit <- lmer(cooktime ~ temperature + pan + (1|day), data = potroast_num)
summary(roast_fit)

## ------------------------------------------------------------------------
#Problem 4a
roast_fit_null <- lmer(cooktime ~ temperature + pan + (1|day), data = potroast_num, REML = FALSE)
roast_fit_alt <- lmer(cooktime ~ as.factor(temperature) + pan + (1|day), data = potroast_num, REML = FALSE)

#LRT with ChiSq test statistic
anova(roast_fit_null, roast_fit_alt)

## ---- warning=FALSE, error=FALSE, message=FALSE--------------------------
#Problem 4b
#Define likelihood ratio function
lr = function(m){
  #Change to categorical
  m.cat = with(m@frame, lmer(cooktime ~ as.factor(temperature) + pan + (1|day), REML=FALSE))
  #Calculate LRT statistic
  x2 = as.numeric(2*(logLik(m.cat) - logLik(m)))
  
  return(c(lr = x2))
}

#Now, perform bootstrap
set.seed(4)
if (!exists('bag_4b')) {
  #Perform the bootstrap
  bag_4b = bootMer(roast_fit_null, lr, nsim = 2000)
}
bag_df <- as.data.frame(bag_4b)

#Now find the p-value
bag_df$lr[bag_df$lr<0] = 0.0
pval = mean(bag_df$lr >= bag_4b$t0["lr"])
paste("The p-value for the parametric bootstrap test is: ",pval)

#Plot the null distribution
hist(bag_df$lr, freq = FALSE, breaks = 30, xlab = "-2 loglik ratio", main = "")
curve(dchisq(x,df=2), add = T, n = 100)


## ------------------------------------------------------------------------
#Problem 4c
#Approximate F test
anova(roast_fit_null)
anova(roast_fit_alt)

## ----code = readLines(purl(this_file, documentation = 1)), echo = T, eval = F----
## # this R markdown chunk generates a code appendix

