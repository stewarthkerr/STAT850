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

this_file <- "kerr_stat850_hw07.Rmd"  # used to automatically generate code appendix

## ------------------------------------------------------------------------
#Problem 1c
#Load the data in 
breadvolume <- read.csv("~/2019spring/STAT850/hw7/breadvolume.csv")
breadvolume$fat = factor(breadvolume$fat)
breadvolume$surfactant = factor(breadvolume$surfactant)
breadvolume$day = factor(breadvolume$day)

#Build model and do ANOVA
bv_lm <- lm(volume ~ fat*surfactant + day, data = breadvolume)
anova(bv_lm)

## ------------------------------------------------------------------------
#Problem 1c
#Compare to drop1
drop1(bv_lm, test = "F")

## ------------------------------------------------------------------------
#Problem 1c
#Look at interaction coefficients
summary(bv_lm)

## ------------------------------------------------------------------------
#Problem 1d
#change the baseline
breadvolume$fat = factor(breadvolume$fat, levels = c("3","2","1"))
bv_contrasts = contrasts(breadvolume$fat)

#Now repeat c
bv_lm2 <- lm(volume ~ fat*surfactant + day, data = breadvolume, contrasts = bv_contrasts)
drop1(bv_lm2, test = "F")
summary(bv_lm2)

## ------------------------------------------------------------------------
#Problem 1e
breadvolume$fs = breadvolume$fat:breadvolume$surfactant
qplot(fs, volume, geom = "boxplot", data = breadvolume, xlab = "Fat:Surfactant")

## ------------------------------------------------------------------------
#Problem 1e
bv_aov <- aov(volume ~ fat:surfactant + day, data = breadvolume)
TukeyHSD(bv_aov, "fat:surfactant", ordered = TRUE)

## ------------------------------------------------------------------------
#Problem 2b
#Load the data in 
milk <- read.csv("~/2019spring/STAT850/hw7/milkcontamination.csv")
milk$lab = factor(milk$lab)
milk$sample = factor(milk$sample)

#Visualize data
qplot(lab,bacterialcount, geom = "boxplot", data = milk)
with(milk, interaction.plot(lab,sample,bacterialcount))

## ---- warning=FALSE------------------------------------------------------
#Problem 2b
#Fit the lmer model
milk_lmer <- lmer(bacterialcount ~ 1 + (1|lab) + (1|sample) + (1|lab:sample), data = milk)
summary(milk_lmer)

## ------------------------------------------------------------------------
#Problem 2b
milk_fixed <- lm(bacterialcount ~ lab*sample, data = milk)
summary(milk_fixed)

## ----code = readLines(purl(this_file, documentation = 1)), echo = T, eval = F----
## # this R markdown chunk generates a code appendix

