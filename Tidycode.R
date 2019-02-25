library(reshape2)
library(foreign)
library(ggplot2)
library(stargazer)
library(RColorBrewer)
library(corrplot)
library(car)
library(plyr)
summary(wb$Poverty)
wb <- read.csv("C:/Users/User/Desktop/MA Thesis/wb.csv", fill = TRUE, check.names=T)
summary(wb$growth)
wb_dev <- subset(wb, OECD == 0)
no_dev <- subset(wb_dev, anocracy == 0)
modelx <- glm(palmaratio ~ remgdp + odagdp + fdigdp + GDP_gross + GDP_pcap + Inflation + incomegroup + edu + fd + region + remgdp*autocracy, data = wb_dev)
summary(modelx)
modely <- glm(growth ~ Remittances + ODA + FDI + GDP_gross + GDP_pcap + incomegroup + region +
                edu + fd + democracy + logrem*democracy, data = no_dev)
summary(modely)

b <- modely$coefficients
vcov <- vcov(modely)
nsim = 1000

set.seed(07052018)
S <- mvrnorm(nsim, b, vcov)


#polity2_sim <- seq(min(MAR$polity22, na.rm=TRUE), max(MAR$polity22, na.rm=TRUE), length=1000)
#GPRO_sim <- seq(min(MAR$GPRO), max(MAR$GPRO), length=1000)
rem_sim <- seq(min(no_dev$logrem, na.rm = TRUE), max(no_dev$logrem, na.rm = TRUE), length=100)

scenario1 <- cbind(1,
                   rem_sim,
                   mean(no_dev$logODA, na.rm=TRUE),
                   mean(no_dev$logFDI, na.rm=TRUE),
                   mean(no_dev$loggdp, na.rm=TRUE),
                   mean(no_dev$loggdpcap, na.rm=TRUE),
                   mean(no_dev$incomegroup_num, na.rm=TRUE),
                   mean(no_dev$region_num, na.rm=TRUE),
                   mean(no_dev$edu, na.rm=TRUE),
                   mean(no_dev$fd, na.rm=TRUE),
                   1,
                   rem_sim*1)

scenario2 <- cbind(1,
                   rem_sim,
                   mean(no_dev$logODA, na.rm=TRUE),
                   mean(no_dev$logFDI, na.rm=TRUE),
                   mean(no_dev$loggdp, na.rm=TRUE),
                   mean(no_dev$loggdpcap, na.rm=TRUE),
                   mean(no_dev$incomegroup_num, na.rm=TRUE),
                   mean(no_dev$region_num, na.rm=TRUE),
                   mean(no_dev$edu, na.rm=TRUE),
                   mean(no_dev$fd, na.rm=TRUE),
                   0,
                   rem_sim*0)
## mu and p
mu1 <- S %*% t(scenario1)
mu2 <- S %*% t(scenario2)

p1 <- 1/(1 + exp(-mu1))
p2 <- 1/(1 + exp(-mu2))

## means and quants
pmean1 <- apply(p1, 2, mean)
pquants1 <- apply(p1, 2, quantile, probs = c(.025, .975), na.rm=TRUE)

pmean2 <- apply(p2, 2, mean)
pquants2 <- apply(p2, 2, quantile, probs = c(.025, .975), na.rm=TRUE)

newdata1 <- data.frame(cbind(rem_sim,
                             pmean1,
                             t(pquants1),
                             pmean2,
                             t(pquants2)))


#pdf(file = "Final graph model 2.pdf")
#require(gridExtra)
ggplot(newdata1, aes(x = rem_sim, y = pmean1)) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.2, fill = "red") +
  geom_ribbon(aes(ymin = X2.5..1, ymax = X97.5..1), alpha = 0.2, fill = "blue") +
  #geom_ribbon(aes(ymin = X2.5..2, ymax = X97.5..2), alpha = 0.2, fill = "brown") +
  #geom_ribbon(aes(ymin = X2.5..3, ymax = X97.5..3), alpha = 0.2, fill = "black") +
  geom_abline(intercept = 0, slope = 0) + 
  scale_x_continuous("Logged remittances") +
  scale_y_continuous("Predicted Palma Ratio") +
  geom_line(aes(x = rem_sim, y = pmean1, color = "Democracy = 1")) +
  geom_line(aes(x = rem_sim, y = pmean2, color = "Democracy = 0")) +
  #geom_line(aes(x = LANG_sim, y = pmean3, color = "LANG = 2")) +
  #geom_line(aes(x = GPRO_sim, y = pmean4, color = "GC11 = 3")) +
  theme_bw() + theme(legend.title = element_blank()) +
  ggtitle("Predicted Probabilities Graph (Model 6)") +
  theme(plot.title = element_text(hjust = 0.5))
#dev.off()

summary(wb$incomegroup)
