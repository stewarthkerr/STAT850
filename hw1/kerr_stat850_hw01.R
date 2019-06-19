## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(knitr)

set.seed(1104)             # make random results reproducible

this_file <- "kerr_stat850_hw01.Rmd"  # used to automatically generate code appendix

## ----fig.height = 5------------------------------------------------------
#Problem 1a
#Read in the data and perform the regression
outlier <- read.csv("~/2019spring/STAT850/hw1/outlier.txt", sep="")
outlier_lm <- lm(outlier$y ~ outlier$x, qr = T)
plot(outlier$y ~ outlier$x, main = "Plot of y vs. x")

#Perform diagnostics by looking at externally studentized residuals
outlier_lm_diag <- ls.diag(outlier_lm)
plot(outlier_lm_diag$stud.res ~ outlier_lm$fitted.values, ylab = "Externally Studentized Residuals", xlab = "fitted values", main = "Plot of externally studentized residuals vs. fitted values")

## ---- fig.height = 5-----------------------------------------------------
#Problem 1b
#i. Remove observation 9 from the data set
outlier_mod <- filter(outlier, indic == 0)

#ii. Regress the new data set
outlier_mod_lm <- lm(y~x ,data = outlier_mod)
summary(outlier_mod_lm)

#iii. Predict value when x = 12 and calculate standard error
y_pred = 8.7656 + 2.1992*12
mse = sum((outlier_mod_lm$residuals^2)/12)
ssqx = sum((outlier_mod$x-mean(outlier_mod$x))^2)
xbar = mean(outlier_mod$x)
se_pred = sqrt(mse*(1+(1/14)+(((12-xbar)^2)/ssqx)))
paste("The standard error for prediction is:  ",round(se_pred,3))

#iv. Test whether the questionable observation is within sampling error
t = (26 - y_pred)/(se_pred)
paste("The observed T statistic is ", round(t,3), "with 12 degrees of freedom")

#v. Compute the p-value and apply Bonferroni
paste("The p-value for this test statistic is: ",round(2*pt(t,12),4))


## ---- fig.height=5-------------------------------------------------------
#Problem 1c
#i. Perform a multiple linear regression of y on x and indic
outlier_mlm <- lm(y ~ x + indic, data = outlier)
summary(outlier_mlm)

## ---- fig.height=4-------------------------------------------------------
#Problem 2c
#Generate data from two parallel lines
x = seq(2,6, by = 0.1)
error1 = rnorm(41,0,0.5)
error2 = rnorm(41,0,0.5)
y1 = 5*x + 4 + error1
y2 = 5*x + error2
xc = c(x,x); yc = c(y1,y2)

#Plot the example data
plot(yc~xc)
lm_example = lm(yc~xc)
plot(lm_example$residuals ~ lm_example$fitted.values, ylab = "residuals", xlab = "fitted values")

## ---- fig.height=4-------------------------------------------------------
#Generate a data from a line
x = seq(2,6, by = 0.1)
error1 = rnorm(41,0,0.5)
error2 = rnorm(41,0,0.5)
y1 = 5*x + error1
plot(y1~x)

#Introduce dependent data points
y2 = y1 + 4 + error2
xc = c(x,x); yc = c(y1,y2)

#Plot dependent data
plot(yc~xc)
lm_example = lm(yc~xc)
plot(lm_example$residuals ~ lm_example$fitted.values, ylab = "residuals", xlab = "fitted values")

## ----code = readLines(purl(this_file, documentation = 1)), echo = T, eval = F----
## # this R markdown chunk generates a code appendix

