
# Multilevel try
library(lme4)
mlmVIVS <- lmer(Gini ~ logODA + logrem + logFDI  + loginf + GDPbil + polity2 + (1 + year| country), data = db)
summary(mlmVIVS)

db1 <- subset(db, year < 1992)
db2 <- subset(db, year >= 1992)

model1 <- glm(Gini ~ logODA + logrem + logFDI  + loginf + GDP_pcap + polity2 + 
                as.factor(year) + country + GDP_pcap*logrem, data = db)
summary(model1)
stargazer(model1)
model2 <- glm(Gini ~ remgdp + fdigdp + odagdp + GDP, data = db)

model3 <- glm(Gini ~ remgdp + fdigdp + odagdp + GDP + loginf + polity2, data = db1)
model4 <- glm(Gini ~ remgdp + fdigdp + odagdp + GDP, data = db2)
model5 <- glm(Gini ~ Remittances + FDI + ODA2, data = db)

#pdf(file = "Polity.pdf")
ggplot(db, aes(logrem, loggdp)) +
    geom_jitter(aes(colour = polity2)) +
    geom_smooth(method=lm)
    #geom_text(aes(label=cy), size=1)
#dev.off()
#### Predicted probabilities ####
summary(oda_sim)
oda_sim <- seq(min(db$ODA2, na.rm=TRUE), max(db$ODA2, na.rm=TRUE), length=1000)
fdi_sim <- seq(min(db$FDI, na.rm=TRUE), max(db$FDI, na.rm=TRUE), length=1000)
rem_sim <- seq(min(db$Remittances, na.rm=TRUE), max(db$Remittances, na.rm=TRUE), length=1000)

#ODA predicted probability
newdata <- with(db, data.frame(logODA = seq(min(db$logODA, na.rm=TRUE), max(db$logODA, na.rm=TRUE), length=1000),
                               logFDI=mean(logFDI, na.rm=TRUE), logrem=mean(logrem, na.rm=TRUE),
                               loginf=mean(loginf, na.rm=TRUE), GDP=mean(GDP, na.rm=TRUE),
                               Migrants=mean(Migrants, na.rm=TRUE), polity2=median(polity2, na.rm=TRUE)))

preds <- predict(model1, newdata, type = "response", se.fit=TRUE)

predf <- preds$fit # predicted
lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
upper <- preds$fit + (1.96*preds$se.fit) # upper bounds

#ODA plot
plot(oda_sim, predf, type="l", ylab="Predicted Gini Index", xlab="ODA")
lines(oda_sim, lower, lty=2)
lines(oda_sim, upper, lty=2)

#FDI Predicted Probability
newdata1 <- with(db, data.frame(logFDI = seq(min(db$logFDI, na.rm=TRUE), max(db$logFDI, na.rm=TRUE), length=1000),
                               logODA=mean(logODA, na.rm=TRUE), logrem=mean(logrem, na.rm=TRUE),
                               loginf=mean(loginf, na.rm=TRUE), GDP=mean(GDP, na.rm=TRUE),
                               Migrants=mean(Migrants, na.rm=TRUE), polity2=median(polity2, na.rm=TRUE)))

preds1 <- predict(model1, newdata1, type = "response", se.fit=TRUE)

predf1 <- preds1$fit # predicted
lower1 <- preds1$fit - (1.96*preds1$se.fit) # lower bounds
upper1 <- preds1$fit + (1.96*preds1$se.fit) # upper bounds

#FDI plot
plot(fdi_sim, predf1, type="l", ylab="Predicted Gini Index", xlab="FDI")
lines(fdi_sim, lower1, lty=2)
lines(fdi_sim, upper1, lty=2)

#Remittances predicted probability
newdata2 <- with(db, data.frame(logrem = seq(min(db$logrem, na.rm=TRUE), max(db$logrem, na.rm=TRUE), length=1000),
                                logODA=mean(logODA, na.rm=TRUE), logFDI=mean(logFDI, na.rm=TRUE),
                                loginf=mean(loginf, na.rm=TRUE), GDP=mean(GDP, na.rm=TRUE),
                                Migrants=mean(Migrants, na.rm=TRUE), polity2=median(polity2, na.rm=TRUE)))

preds2 <- predict(model1, newdata2, type = "response", se.fit=TRUE)

predf2 <- preds2$fit # predicted
lower2 <- preds2$fit - (1.96*preds2$se.fit) # lower bounds
upper2 <- preds2$fit + (1.96*preds2$se.fit) # upper bounds

#Remittances plot
plot(rem_sim, predf2, type="l", ylab="Predicted Gini Index", xlab="Remittances")
lines(rem_sim, lower2, lty=2)
lines(rem_sim, upper2, lty=2)



#### Alternative way ####
b <- model1$coefficients
vcov <- vcov(model1)
nsim = 1000

set.seed(06122017)
S <- mvrnorm(nsim, b, vcov)


#logrem_sim <- seq(min(db$logrem, na.rm=TRUE), max(db$logrem, na.rm=TRUE), length=1000)
#oda_sim <- seq(min(db$logODA, na.rm=TRUE), max(db$logODA, na.rm=TRUE), length=1000)
oda_sim <- as.vector(c(rep(min(db$ODA2, na.rm = TRUE), 334), rep(mean(db$ODA2, na.rm = TRUE), 333), 
                       rep(max(db$ODA2, na.rm = TRUE), 333)))

scenario <- cbind(1,
                  oda_sim,
                  mean(db$logrem, na.rm=TRUE),
                  mean(db$logFDI, na.rm=TRUE),
                  mean(db$loginf, na.rm=TRUE),
                  mean(db$GDP, na.rm=TRUE),
                  mean(db$Migrants, na.rm=TRUE),
                  median(db$polity2, na.rm=TRUE))

mu1 <- S %*% t(scenario)
p1 <- 1/(1 + exp(-mu1))

pmean1 <- apply(p1, 2, mean)
pquants1 <- apply(p1, 2, quantile, probs = c(.025, .975), na.rm=TRUE)

newdata1 <- data.frame(cbind(oda_sim,
                             pmean1,
                             t(pquants1)))

ggplot(newdata1, aes(x = oda_sim, y = pmean1)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.2, fill = "red") +
  geom_abline(intercept = 0, slope = 0) + 
  scale_x_continuous("Logged ODA") +
  scale_y_continuous("Predicted Probability of Gini index") +
  geom_line(aes(x = oda_sim, y = pmean1, color = "Logged ODA")) +
  theme_bw() + theme(legend.title = element_blank()) +
  ggtitle("Predicted Probabilities Graph (Model 1)") +
  theme(plot.title = element_text(hjust = 0.5))


### Database w/o CPI
#db <- read.csv("C:/Users/User/Desktop/MA Thesis/db.csv", fill = TRUE, check.names=T)
#db$X <- NULL
#db$postsoc <- 0
#db$postsoc[db$year>=1992] <- 1