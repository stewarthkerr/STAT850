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

set.seed(1104)             # make random results reproducible

this_file <- "kerr_stat850_hw08.Rmd"  # used to automatically generate code appendix

## ------------------------------------------------------------------------
#Problem 1b
#Load in the data
cholesterol <- read.csv("~/2019spring/STAT850/hw8/cholesterol.csv")
cholesterol$patient = factor(cholesterol$patient)
cholesterol$run = factor(cholesterol$run)

#Fit the data (fixed for ANOVA table)
cholesterol_fixed <- lm(cholesterol ~ patient/run, data = cholesterol)
cholesterol_lmer <- lmer(cholesterol ~ 1 + (1|patient/run), data = cholesterol)
anova(cholesterol_fixed)
summary(cholesterol_lmer)

## ---- warning=FALSE------------------------------------------------------
#Problem 2
Fpower <- function(n,v){
    #Calculate noncentral parameter
    noncentralp = v*n*(log(10)^2)/(2*(0.77+0.02*n))
    #Find F that would cause us to reject at 0.95 percent
    rejectionf = qf(0.95,1,v-1)
    #calculate power
    power = pf(rejectionf, 1, v-1, ncp = noncentralp, lower.tail = FALSE)
    return(power)
}

#This calculates the power for a bunch of different n,v combinations
n = seq(from = 1, to = 500)
v = seq(from = 2, to = 4)
df = expand.grid(v,n) %>% 
  rename(v = Var1, n = Var2) %>%
  mutate(power = Fpower(n,v))

#Remove values of n & v that have too low of power and high power, return head
kable(filter(df, power >= 0.7 & power <= 0.85) %>% arrange(-power) %>% head())

## ------------------------------------------------------------------------
#Problem 3a
Fmaxpower <- function(a){
    #Calculate noncentral parameter
    n = floor(a/2)
    noncentralp = 12*n*(1-(n/a))
    #Find F that would cause us to reject at 0.95 percent
    rejectionf = qf(0.95,a-1,24)
    #calculate power
    power = pf(rejectionf, a-1, 24, ncp = noncentralp, lower.tail = FALSE)
    paste("The max power for a = ", a, " is ", round(power,4))

}
Fmaxpower(4)
Fmaxpower(5)

## ------------------------------------------------------------------------
#Problem 3b
Fminpower <- function(a){
  noncentralp = 2
  rejectionf = qf(0.95,a-1,24)
  power = pf(rejectionf, a-1, 24, ncp = noncentralp, lower.tail = FALSE)
  paste("The min power for a  = ",a, " is ", round(power,4))
}
Fminpower(4)
Fminpower(5)

## ------------------------------------------------------------------------
#Problem 4
p4power <- function(n0,n){
  sp = ((n0-1)+(n-1))/(n0+n-2)
  #Power is proportional to the following
  power = 1/(sqrt(sp)*sqrt((1/n0) + (1/n)))
  return(power)
}

#Let's assume N is set to 100 and p = 4. Then I will build a line of possible sample sizes
n = seq(1,20)
n0 = 100-4*n
df <- data.frame(n0,n) %>% mutate(prop_power = p4power(n0,n))
head(arrange(df, -prop_power))

## ------------------------------------------------------------------------
#problem 4
n = seq(1,100)
n0 = 1000-9*n
df <- data.frame(n0,n) %>% mutate(prop_power = p4power(n0,n))
head(arrange(df, -prop_power))

## ----code = readLines(purl(this_file, documentation = 1)), echo = T, eval = F----
## # this R markdown chunk generates a code appendix

