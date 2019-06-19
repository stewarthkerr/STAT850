## ----include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(knitr)
library(car)
library(ggplot2)

set.seed(1104)             # make random results reproducible

this_file <- "kerr_stat850_hw03.Rmd"  # used to automatically generate code appendix

## ------------------------------------------------------------------------
#Problem 1a
mouse <- read.csv("~/2019spring/STAT850/hw3/mouse.txt", sep="")

#Create a grouped boxplot
ggplot(mouse, aes(x=diet, y=score, group=diet)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2))

## ---- warning = FALSE----------------------------------------------------
#Problem 2c
#Construct a residuals plot
mouse$diet = factor(mouse$diet)
mouse_lm = lm(score ~ diet, data = mouse)
plot(ls.diag(mouse_lm)$stud.res ~ fitted(mouse_lm), ylab = "studentized residuals")

## ------------------------------------------------------------------------
#Problem 1d
#Perform anova
anova(mouse_lm)

#Collecting variables for computation 
alpha = 0.05
dfError = 12
MSE = anova(mouse_lm)$`Mean Sq`[2]
SE = sqrt(MSE)
n = 3
k = 6

#LSD
LSD = LSD = qt(1-alpha/2,dfError)*SE*sqrt(2/n)

#Tukey's
Tukey = qtukey(1-alpha,k,dfError)*SE*sqrt(1/n)

#Hayter
Hayter = (1/sqrt(2))*qtukey(1-alpha,k-1,dfError)*SE*sqrt(2/n)

#Scheffe
Scheffe = SE*sqrt(2/n)*sqrt((k-1)*qf(1-alpha,k-1,dfError))

#Now make a table of the "yardsticks"
yardsticks = c(LSD,Tukey,Hayter,Scheffe) 
names(yardsticks) <- c("LSD","Tukey's","Hayter","Scheffe")
yardsticks

#Make a table of means for each group
labels <- c("Std","Std + E", "Std + C","Std + E + C","Std + 2C","Std + B6") %>% cbind(c(1,2,3,4,5,6))
labels <- data.frame(labels) %>% rename(diet = V2)
scoreMeans <- summarise(group_by(mouse,diet), mean = mean(score))
scoreMeans <- inner_join(labels,scoreMeans, by = c('diet')) %>% select(-diet)
kable(scoreMeans)

## ------------------------------------------------------------------------
#Problem 1e
lht(lm(score~diet-1, data = mouse),hypothesis.matrix = c(1,0,-2,0,1,0))

## ------------------------------------------------------------------------
#Problem 1f
#Create each of the contrasts
contr = contrasts(mouse$diet)
contr[,1] = c(1,1,-1,-1,0,0)
contr[,2] = c(1,-1,1,-1,0,0)
contr[,3] = c(1,-1,-1,1,0,0)

#Perform the linear regression with contrasts
mouse_lm2 <- lm(score ~ diet, data = mouse, contrasts = list(diet = contr))
summary(mouse_lm2)

## ------------------------------------------------------------------------
#Problem 2
F = 4.90
alpha = 0.05
df(F,3,50) < alpha

## ------------------------------------------------------------------------
#Problem 2
#Calculate the pooled variance - we will use for the t-tests
grandMean = sum((2/54)*(39.3),(25/54)*(40.1),(25/54)*(42.0),(2/54)*(43.0))
MSR = sum((2/3)*(39.3-grandMean)^2,
  (25/3)*(40.1-grandMean)^2,
  (25/3)*(42.0-grandMean)^2,
  (2/3)*(43.0-grandMean)^2)
MSE = MSR/F
SE = sqrt(MSE)
harmonicN = 1/4*((1/2) + (1/25) + (1/25) + (1/2))
LSD = qt(0.975,50)*SE*sqrt(2*harmonicN)
paste("The Fisher's LSD to compare treatment mean differences against is ",round(LSD,2))

## ------------------------------------------------------------------------
#Problem 3c
X = matrix(byrow = TRUE, nrow = 6, ncol = 4, data = c(1,1,0,0,
                                  1,1,0,0,
                                  1,1,0,0,
                                  1,0,1,0,
                                  1,0,1,0,
                                  1,0,0,1))
G1 = matrix(byrow = TRUE, nrow = 4, ncol = 4, data = c(0,0,0,0,
                                  0,1/3,0,0,
                                  0,0,1/2,0,
                                  0,0,0,1))
G2 = matrix(byrow = TRUE, nrow = 4, ncol = 4, data = c(1,-1,-1,0,
                                  -1,4/3,1,0,
                                  -1,1,3/2,0,
                                  0,0,0,0))
G3 = (1/18)*matrix(byrow = TRUE, nrow = 4, ncol = 4, 
                   data = c(0,2,3,6,
                            0,4,-3,-6,
                            0,-2,6,-6,
                            0,-2,-3,12))
G4 = (1/54)*matrix(byrow = TRUE, nrow = 4, ncol = 4, 
                   data = c(17,-11,-8,1,
                            -11,23,2,-7,
                            -8,2,26,-10,
                            1,-7,-10,35))

#Check to see if the matrices are is generalized inverses
paste("G1 is a generalized inverse: ",(t(X)%*%X)%*%G1%*%(t(X)%*%X) %>% identical(t(X)%*%X))
paste("G2 is a generalized inverse: ",(t(X)%*%X)%*%G2%*%(t(X)%*%X) %>% identical(t(X)%*%X))
paste("G3 is a generalized inverse: ",(t(X)%*%X)%*%G3%*%(t(X)%*%X) %>% all.equal(t(X)%*%X))
paste("G4 is a generalized inverse: ",(t(X)%*%X)%*%G4%*%(t(X)%*%X) %>% all.equal(t(X)%*%X))

## ------------------------------------------------------------------------
#Problem 3c
Y = matrix(nrow = 6, ncol = 1, data = c(72,36,12,48,12,36))
B1 = G1%*%t(X)%*%Y
B2 = G2%*%t(X)%*%Y
B3 = G3%*%t(X)%*%Y
B4 = G4%*%t(X)%*%Y
paste("mu: ",round(B1,2),
      " ,alpha1: ", round(B2,2),
      " ,alpha2: ", round(B3,2),
      " ,alpha3: ", round(B4,2))

## ------------------------------------------------------------------------
#Problem 3d
Y = matrix(nrow = 6, ncol = 1, data = c(39,52,99,12,36,48))
B1 = G1%*%t(X)%*%Y
B2 = G2%*%t(X)%*%Y
B3 = G3%*%t(X)%*%Y
B4 = G4%*%t(X)%*%Y
paste("mu: ",round(B1,2),
      " ,alpha1: ", round(B2,2),
      " ,alpha2: ", round(B3,2),
      " ,alpha3: ", round(B4,2))

## ----code = readLines(purl(this_file, documentation = 1)), echo = T, eval = F----
## # this R markdown chunk generates a code appendix

