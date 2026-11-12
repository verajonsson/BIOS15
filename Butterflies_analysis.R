library(tidyverse)


#Read data
butterflies <- read.csv("butterflies.csv")

#Check what the data looks like
head(butterflies)

#Summary statistics
butterflies$MaternalHost <- paste0(butterflies$MaternalHost, "M")
butterflies$LarvalHost <- paste0(butterflies$LarvalHost, "L")

means <- tapply(butterflies$GrowthRate, list(butterflies$MaternalHost, butterflies$LarvalHost), mean)

# Perform ANOVA developmental time

mod1 <- lm(formula = DevelopmentTime ~ MaternalHost * LarvalHost, data = butterflies)

#Diagnostic plots
par(mfrow = c(2, 2)) #get all four in the same window  
plot(mod1)
par(mfrow = c(1, 1)) #reset


summary(mod1)

anova(mod1)



# Perform ANOVA growth rate

mod2 <- lm(formula = GrowthRate ~ MaternalHost * LarvalHost, data = butterflies)

#Diagnostic plots
par(mfrow = c(2, 2)) #get all four in the same window  
plot(mod2)
par(mfrow = c(1, 1)) #reset


summary(mod2)

anova(mod2)





