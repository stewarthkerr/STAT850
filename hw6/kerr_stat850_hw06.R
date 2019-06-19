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

set.seed(1104)             # make random results reproducible

this_file <- "kerr_stat850_hw06.Rmd"  # used to automatically generate code appendix

## ------------------------------------------------------------------------
#Problem 1a
#Load the data in
grapes <- read.csv("~/2019spring/STAT850/hw6/grapes.csv")
grapes$fertilizer = factor(grapes$fertilizer)
grapes$pruning = factor(grapes$pruning)

#Construct plots
with(grapes, interaction.plot(pruning,fertilizer, sweetness))

## ------------------------------------------------------------------------
#Problem 1b
#Add a column for "bench"
grapes$bench = with(grapes, vineyard:fertilizer)

#make fit
grape_fit = lmer(sweetness ~ vineyard + fertilizer + pruning + fertilizer:pruning + (1 | bench)
                 + (1 | trunk), grapes)
summary(grape_fit)
anova(grape_fit)

## ------------------------------------------------------------------------
#Problem 1c
## observed versus fitted values by bench
plot(grape_fit, sweetness ~ fitted(.) | bench, abline = c(0,1),
     ylab = "observed sweetness", xlab = "fitted sweetness")

## ------------------------------------------------------------------------
#Problem 1c
plot(resid(grape_fit, scaled=TRUE) ~ fitted(grape_fit), xlab = "Fitted values", ylab = "Standardized residuals")

## ------------------------------------------------------------------------
#Problem 1c
plot(grape_fit, bench ~ resid(., scaled = TRUE), xlab = "Standardized residuals")

## ------------------------------------------------------------------------
#Problem 2b
#Read data in and create new columns
ion <- read.csv("~/2019spring/STAT850/hw6/ion.csv") %>%
  mutate(energy = substr(energy_quantity,1,1), quantity = substr(energy_quantity,2,2))
ion$temp = factor(ion$temp)

#Average across months
ion_month_avg = summarise(group_by(ion, energy, quantity, temp), quality_average = mean(quality))

#Make plots
ggplot(data=ion_month_avg, aes(x=temp, y=quality_average, group=energy)) +
geom_point(aes(color=energy)) + 
geom_line(aes(color=energy)) + 
facet_wrap(~quantity) + 
labs(title = "Energy level, temperature, and ion quantity interaction plot", subtitle = "Ion quantity", x= "Temperature", y="Quality measure", color="Energy level")  

## ---- warning = FALSE----------------------------------------------------
#Problem 2c
#Create the horizontal strips and vertical strips
Hstrip = with(ion, temp:month)
Vstrip = with(ion, energy_quantity:month)

#Make the fit
ion_fit = lmer(quality ~ energy*quantity*temp + month + (1|Hstrip) + (1|Vstrip), data = ion)
anova(ion_fit)

## ------------------------------------------------------------------------
#Problem 2e
p1 = ggplot(data.frame(resid=ranef(ion_fit)$Hstrip[,1]), aes(sample=resid)) + geom_qq() + labs(subtitle = "Horizontal random effect QQ plot")
p2 = ggplot(data.frame(resid=ranef(ion_fit)$Vstrip[,1]), aes(sample=resid)) + geom_qq() + labs(subtitle = "Vertical random effect QQ plot")
cowplot::plot_grid(p1,p2)

## ------------------------------------------------------------------------
#Problem 2e
#Predicted vs. residuals
p3 = data.frame(predicted = predict(ion_fit), residuals(ion_fit), temp = ion$temp, energy_quantity = ion$energy_quantity) %>%
  ggplot(aes(x=predicted, y = residuals.ion_fit., color = energy_quantity)) + geom_point(aes(shape=temp)) + labs(title = "Predicted vs. residuals", ylab = "residuals")

#QQplot
p4 = ggplot(data.frame(resid=residuals(ion_fit)), aes(sample=resid)) + geom_qq() + labs(title = "QQplot of overall residuals")

plot(p3)
plot(p4)

## ----code = readLines(purl(this_file, documentation = 1)), echo = T, eval = F----
## # this R markdown chunk generates a code appendix

