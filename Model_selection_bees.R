
library(MuMIn)
library(MASS)

#Importing data
dat <- read.csv("Eulaema.csv")

#Overview of relationships in data
pairs(dat[, c(1, 5:12)], panel = panel.smooth)


#Get some summary statistics
dat |> 
  summarise(mean(forest.), min(forest.), max(forest.),
            mean(MAP), min(MAP), max(MAP),
            mean(Eulaema_nigrita), min(Eulaema_nigrita), max(Eulaema_nigrita))


#Poisson regression
m <- glm(Eulaema_nigrita ~ effort + forest. + MAP, "poisson", data = dat)

summary(m) #overdispersion (residual deviance >> df) 
#-> using negative binomial instead


#Trying different models and comparing AIC

m1 <- glm.nb(Eulaema_nigrita ~ forest. + MAP + effort, data = dat)
m2 <- glm.nb(Eulaema_nigrita ~ MAP + forest., data = dat)
m3 <- glm.nb(Eulaema_nigrita ~ forest. + effort, data = dat)
m4 <- glm.nb(Eulaema_nigrita ~ MAP + effort, data = dat)
m5 <- glm.nb(Eulaema_nigrita ~ effort, data = dat)
m6 <- glm.nb(Eulaema_nigrita ~ 1, data = dat) #null model


mlist <- list(m1, m2, m3, m4, m5, m6) #putting all models in a list
AICTab <- AIC(m1, m2, m3, m4, m5, m6) #getting the AIC

AICTab$logLik <- unlist(lapply(mlist, logLik)) #adding log likelihood as a column
AICTab <- AICTab[order(AICTab$AIC, decreasing=F),] #putting in decreasing order
AICTab$delta <- round(AICTab$AIC - min(AICTab$AIC), 2) #adding delta-AIC as column
lh <- exp(-0.5*AICTab$delta)
AICTab$w <- round(lh/sum(lh), 2) #calculating weight

#Adding the r squared to the AIC table
AICTab$r_squared <- c(r.squaredGLMM(m1)[3, 1], r.squaredGLMM(m2)[3, 1], 
               r.squaredGLMM(m3)[3, 1], r.squaredGLMM(m4)[3, 1],
               r.squaredGLMM(m5)[3, 1], r.squaredGLMM(m6)[3, 1])


AICTab

#Since m1 had lowest AIC and highest pseudo r squared I chosed to use that model
summary(m1)
