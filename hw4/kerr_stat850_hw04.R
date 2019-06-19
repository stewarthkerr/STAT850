## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(knitr)
library(car)
library(ggplot2)

set.seed(1104)             # make random results reproducible

this_file <- "kerr_stat850_hw04.Rmd"  # used to automatically generate code appendix

## ------------------------------------------------------------------------
#Problem 1a
#Load in data and contrasts
prob1 <- read.csv('hw04.csv')

#Plot the data
plot(prob1$y ~ prob1$trt)

#Build the contrasts for each test
c1 = c(-1/2,1/2,1/2,-1/2) #Main effect of A
c2 = c(-1/2,-1/2,1/2,1/2) #Main effect of B
c3 = c(1/2,-1/2,1/2,-1/2) #Interaction of AB
cmat = cbind(c1,c2,c3)

#Load the contrasts in
contrasts(prob1$trt) <- cmat

prob1_aov <- aov(y ~ trt, data = prob1)
summary.aov(prob1_aov, split = list(trt = list("Main effect of A" = 1, "Main effect of B" = 2, "Interaction effect of AB" = 3)))


## ------------------------------------------------------------------------
#Problem 1b
prob1_fit = lm(y ~ a * b, data = prob1)
anova(prob1_fit)
#Note that anova() gives type I SS but since our design is balanced this is equal to type III SS

## ------------------------------------------------------------------------
#Problem 1c
#Calculate mean and se for each treatment
prob1_plot <- summarise(group_by(prob1,a,b), ybar = mean(y), sd = sd(y))
prob1_plot$a = factor(prob1_plot$a)
prob1_plot$b = factor(prob1_plot$b)

gp <- ggplot(prob1_plot, aes(x=a, y=ybar, colour=b, group=b))
gp + geom_line(aes(linetype=b), size=.6) + 
     geom_point(aes(shape=b), size=3) + 
     geom_errorbar(aes(ymax=ybar+sd, ymin=ybar-sd), width=.1) + 
     ylab("Average y")

## ------------------------------------------------------------------------
#Problem 1d
#Perform regression and anova
prob1_reg <- lm(y ~ x1 + x2 + x3, data = prob1)
anova(prob1_reg)

## ------------------------------------------------------------------------
#Problem 1d
prob1_reg <- lm(y ~ x3 + x2 + x1, data = prob1)
anova(prob1_reg)

## ------------------------------------------------------------------------
#Problem 2b
#Load data
orange <- read.csv("~/2019spring/STAT850/hw4/orange.txt", sep="")
orange$calcium = factor(orange$calcium)
orange$pH = factor(orange$pH)

#Construct plots
with(orange, interaction.plot(pH, calcium, diam))

## ------------------------------------------------------------------------
#Problem 2c
orange_aov <- aov(diam ~ pH * calcium, data = orange)
anova(orange_aov)

## ------------------------------------------------------------------------
#Problem 2d
model.tables(orange_aov, type = "means", se = TRUE)

## ------------------------------------------------------------------------
dfError = 24
LSD_pH = qt(0.975, df = dfError)*0.1227*sqrt(2/9)
LSD_Ca = qt(0.975, df = dfError)*0.1063*sqrt(2/12)
LSD_Comb = qt(0.975, df = dfError)*0.2126*sqrt(2/3)
kable(rbind(LSD_pH,LSD_Ca,LSD_Comb), col.names = "LSD" )

## ------------------------------------------------------------------------
#Problem 2e
plot(rstudent(orange_aov) ~ fitted(orange_aov), xlab = "fitted values", ylab = "studentized residuals")

## ------------------------------------------------------------------------
#Problem 4a
#Load the data in and create sum variable
apple <- read.csv("~/2019spring/STAT850/hw4/apple.csv") %>%
  mutate(weight_tot = weight_b1+weight_b2)
apple$treatment = factor(apple$treatment)
apple$block = factor(apple$block)

#ANOVA LM with block
apple_lm <- lm(weight_tot ~ treatment + block, data = apple)
anova(apple_lm)

#Plot residuals
plot(rstudent(apple_lm) ~ fitted(apple_lm), xlab = "fitted values", ylab = "studentized residuals")

## ------------------------------------------------------------------------
#Problem 4b
#Plot the data
#Create a grouped boxplot
ggplot(apple, aes(x=treatment, y=weight_tot, group=treatment)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  ggtitle('Boxplots of total weight by treatment')
ggplot(apple, aes(x=block, y=weight_tot, group=block)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  ggtitle('Boxplots of total weight by block')

## ------------------------------------------------------------------------
#Problem 4c
apple_aov = aov(weight_tot ~ treatment + block, data = apple)
model.tables(apple_aov, type = "means", se = TRUE)

LSD = qt(0.975, df = 18)*327.2*sqrt(2/4)
paste("The LSD for the apple data is: ", round(LSD,1))

## ----code = readLines(purl(this_file, documentation = 1)), echo = T, eval = F----
## # this R markdown chunk generates a code appendix

