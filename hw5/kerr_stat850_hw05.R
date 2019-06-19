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

set.seed(1104)             # make random results reproducible

this_file <- "kerr_stat850_hw05.Rmd"  # used to automatically generate code appendix

## ------------------------------------------------------------------------
#Problem 2a
#Load the data in
apple <- read.csv("~/2019spring/STAT850/hw4/apple.csv")
apple_clean = apple
apple$treatment = factor(apple$treatment)
apple$block = factor(apple$block)
apple = mutate(apple, tree = row_number())

#Reshape data so that we have 1 row per branch
apple1 = dplyr::select(apple, -weight_b2) %>% rename(weight = weight_b1)
apple2 = dplyr::select(apple, -weight_b1) %>% rename(weight = weight_b2)
apple = rbind(apple1,apple2)

## ------------------------------------------------------------------------
#ANOVA LM with block
apple_lmer <- lmer(weight ~ treatment + block + (1|tree), data = apple)
anova(apple_lmer)

#Plot residuals
ggplot(apple, aes(y=weight-fitted(apple_lmer), x=fitted(apple_lmer), color = treatment)) + geom_point() + labs(title = "subsample residuals vs fitted", x = "fitted", y = "residuals")


## ------------------------------------------------------------------------
#Problem 2b
#Plot the data
#Create a grouped boxplot
ggplot(apple, aes(x=treatment, y=weight, group=treatment)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  ggtitle('Boxplots of total weight by treatment')
ggplot(apple, aes(x=block, y=weight, group=block)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  ggtitle('Boxplots of total weight by block')

## ------------------------------------------------------------------------
#Problem 2c
apple_aov = aov(weight ~ treatment + block + (1|tree), data = apple)
model.tables(apple_aov, type = "means", se = TRUE)

LSD = qt(0.975, df = 18)*327.2*sqrt(2/4)
paste("The LSD for the apple data is: ", round(LSD,1))

## ---- warning = FALSE----------------------------------------------------
#Problem 3a
#Read data in and look for "typos"
prairie <- read.csv("~/2019spring/STAT850/hw5/prairiespecies.csv")

#Make sure number of treatments = 20
kable(count(group_by(prairie,trt)))

#Make sure number of each species = 20
kable(count(group_by(prairie,species))) 
#Looks like some of our As are labeled as Cs - fixing below
prairie = bind_rows(filter(prairie, trt != "T1"), mutate(filter(prairie, trt == "T1"), species = "A"))
prairie$species = factor(prairie$species)

#Make sure we have 4 data points for each plot
kable(count(group_by(prairie, plot)))

#Make sure we have 20 points in each square
kable(count(group_by(prairie,square)))

## ------------------------------------------------------------------------
#Problem 3b
#Graph data
ggplot(prairie, aes(x=species, y=seedling, group=species)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  ggtitle('Boxplots of seedlings by species')

#Analyze difference among species with random effect of plot
prairie_lmer <- lmer(seedling ~ species + (1 | species:plot), data = prairie)
anova(prairie_lmer)

#Look at residuals, LSD
prairie_re = ranef(prairie_lmer)
tmp = data.frame(resid = prairie_re[[1]][,1], species = substr(rownames(prairie_re[[1]]),1,1))
tmp$fitted = predict(prairie_lmer, tmp, re.form = NA)
plot(tmp$resid ~ tmp$fitted, main = "Residual Sampling vs. Fitted values", xlab = "Fitted values", ylab = "Residuals")
difflsmeans(prairie_lmer, test.effs = "Group")

## ------------------------------------------------------------------------
#Problem 3b
#Group the mature and transitional species
prairie <- mutate(prairie, species_type = case_when(
  species == "A" | species == "D" ~ "mature",
  species == "C" | species == "B" ~ "transitional"))
prairie$species_type = factor(prairie$species_type)

#Perform a t-test on the grouped means
t.test(seedling ~ species_type, data = prairie, alternative = "greater")

## ------------------------------------------------------------------------
#Problem 3c
prairie_lm = lm(seedling ~ species + species:plot, data = prairie)
anova(prairie_lm)

## ----code = readLines(purl(this_file, documentation = 1)), echo = T, eval = F----
## # this R markdown chunk generates a code appendix

