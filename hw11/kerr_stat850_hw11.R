## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(knitr)
library(car)
library(ggplot2)
library(MASS)
library(lme4)
library(lmerTest)
#library(tidyr)
#library(cowplot)
#library(multcomp)
#library(lsmeans)

set.seed(1104)             # make random results reproducible

this_file <- "kerr_stat850_hw11.Rmd"  # used to automatically generate code appendix

## ---- echo = FALSE, message = FALSE--------------------------------------
## define functions for this problem
## function to simulate data:
## ng groups, nr replicates (or individuals) per group, 1 observation / individual
## fsd = SD of group random effects, indsd = SD of individual random effects
## b = fixed effect coefficients, of size 2: intercept and slope
simfun = function(ng=4, nr=10, fsd=1, indsd=0.5, b=c(1,2)) {
  ntot = nr*ng
  b.reff = rnorm(ng, sd=fsd)
  b.rind = rnorm(ntot, sd=indsd)
  x = runif(ntot) # predictor
  dd = data.frame(x, f=factor(rep(LETTERS[1:ng], each=nr)), obs=factor(1:ntot))
  dd$eta0 = as.vector(model.matrix(~x,data=dd) %*% b)
  dd$bi = b.rind
  dd$eta = with(dd, eta0 + b.reff[f] + b.rind) # log E(Yi) = Xbeta + group RE + indiv RE bi
  dd$mu = exp(dd$eta)
  dd$y = with(dd, rpois(ntot, lambda=mu))
  dd
}

## function to extract the variance components and fixed effects
cfun = function(d) {
  m = glmer(y ~ x + (1|f) + (1|obs), family="poisson", data=d)
  c(sqrt(unlist(VarCorr(m))), fixef(m))
}

p3_plot <- function(df, title){
  ggplot(data = df, aes(x = x,y = y, color = f)) +
    geom_point() + 
    geom_smooth(method = "glm", se = F) + 
    ggtitle(title)
}

## ---- message=FALSE------------------------------------------------------
## Problem 3a, part i
set.seed(12345)
#simulate data set
p3_1 = simfun(ng=4, nr=10, fsd=0, indsd=0, b=c(1,2))
#plot dataset
p3_plot(p3_1, "Problem 3a, Part i")
#Fit the data sets
p3_1f1 <- glm(y ~ x, family="quasipoisson", data = p3_1)
p3_1f2 <- glm(y ~ x + f, family="quasipoisson", data = p3_1)
#Look at the fits
summary(p3_1f1)
summary(p3_1f2)

## ---- message=FALSE------------------------------------------------------
## Problem 3a, part ii
set.seed(12345)
#simulate data set
p3_2 = simfun(ng=4, nr=10, fsd=1, indsd=0, b=c(1,2))
#plot dataset
p3_plot(p3_2, "Problem 3a, Part ii")
#Fit the data sets
p3_2f1 <- glm(y ~ x, family="quasipoisson", data = p3_2)
p3_2f2 <- glm(y ~ x + f, family="quasipoisson", data = p3_2)
#Look at the fits
summary(p3_2f1)
summary(p3_2f2)

## ---- message=FALSE------------------------------------------------------
## Problem 3a, part iii
set.seed(12345)
#simulate data set
p3_3 = simfun(ng=4, nr=10, fsd=1, indsd=0.5, b=c(1,2))
#plot dataset
p3_plot(p3_3, "Problem 3a, Part ii")
#Fit the data sets
p3_3f1 <- glm(y ~ x, family="quasipoisson", data = p3_3)
p3_3f2 <- glm(y ~ x + f, family="quasipoisson", data = p3_3)
#Look at the fits
summary(p3_3f1)
summary(p3_3f2)

## ------------------------------------------------------------------------
#Problem 3b
m0 <- glmer(y ~ x + (1|f), family="poisson", data=p3_3)
m1 <- glmer(y ~ x + (1|f) + (1|obs), family="poisson", data=p3_3)

#Perform LRT
drop1(m0, test = "Chisq")
drop1(m1, test = "Chisq")

## ---- message=FALSE, warning=FALSE---------------------------------------
#Problem 3c
oneX2 = function(x){
  #Define null model to generate data
  null_model = glmer(y ~ 1 + (1|f) + (1|obs), family="poisson", data = p3_3)
  simy = simulate(null_model)$sim_1
  
  #Make alternative model from data simulated by null
  alt_model = glmer(simy ~ x + (1|f) + (1|obs), family="poisson", data = p3_3)

  #Return the LRT statistic
  x2 = drop1(alt_model, test="Chisq")$LRT[2]
  
  return(x2)
}

#Now, perform bootstrap
set.seed(4)
if (!exists('bag_3c')) {
  #Perform the bootstrap
  bag_3c = unlist(parallel::mclapply(1:2000,oneX2))
}
bag_df <- as.data.frame(bag_3c)

#Now find the p-value
bag_df$bag_3c[bag_df$bag_3c<0] = 0.0
pval = mean(bag_df$bag_3c >= 257.73)
paste("The p-value for the parametric bootstrap test is: ",pval)

#Plot the null distribution
hist(bag_df$bag_3c, freq = FALSE, breaks = 30, xlab = "-2 loglik ratio", main = "")
curve(dchisq(x,df=1), add = T, n = 100)

## ---- message=FALSE, warning=FALSE---------------------------------------
# Problem 3d
## repeat many times
rr = replicate(100, cfun(simfun()))
#Find means
obs_eff_mean <- mean(rr[1,])
group_eff_mean <- mean(rr[2,])
int_eff_mean <- mean(rr[3,])
x_eff_mean <- mean(rr[4,])
eff_mean <- rbind(obs_eff_mean, group_eff_mean, int_eff_mean, x_eff_mean)

#Find variance
obs_eff_var <- var(rr[1,])
group_eff_var <- var(rr[2,])
int_eff_var <- var(rr[3,])
x_eff_var <- var(rr[4,])
eff_var <- rbind(obs_eff_var, group_eff_var, int_eff_var, x_eff_var)


#Actual effects
obs_eff_real <- 0.5
group_eff_real <- 1.0
int_eff_real <- 1.0
x_eff_real <- 2.0
eff_real <- rbind(obs_eff_real, group_eff_real, int_eff_real, x_eff_real)


#cbind for table
p3d_table <- cbind(eff_real,eff_mean,eff_var)
rownames(p3d_table) <- c("Observation RE","Group RE","Intercept FE","Slope FE")
kable(p3d_table, 
      row.names = TRUE,
      col.names = c("Actual","Mean","Variance"),
      digits = 3)


## ----code = readLines(purl(this_file, documentation = 1)), echo = T, eval = F----
## # this R markdown chunk generates a code appendix

