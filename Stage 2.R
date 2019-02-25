finaldev <- read.csv("C:/Users/User/Desktop/MA Thesis/finaldev.csv", fill = TRUE, check.names=T)

no_dev$loggdp <- log(no$GDP_gross)
no$loggdpcap <- log(no$GDP_pcap)
no_dev$incomegroup_num <- as.numeric(as.factor(as.character(no_dev$incomegroup)))
no_dev$region_num <- as.numeric(as.factor(as.character(no_dev$region)))                   
mean(no$incomegroup, na.rm = TRUE)

wb_dev <- subset(wb, OECD == 0)
  
summary(wb_dev$fdigdp)
summary(wb_dev$odagdp)
summary(wb_dev$remgdp)

demo <- subset(wb, democracy == 1)
no <- subset(wb, anocracy == 0)
modelx <- glm(palmaratio ~ logrem + logODA + logFDI + GDP_gross + GDP_pcap + incomegroup + OECD + edu + fd + region + logrem*autocracy, data = wb)
summary(modelx)
modely <- glm(palmaratio ~ logrem + logODA + logFDI + loggdp + loggdpcap + incomegroup_num + region_num +
                OECD + edu + fd + autocracy + logrem*autocracy, data = no)
summary(modely)


#### Inequality ####
model1 <- glm(gini ~ odalag + remlag + fdilag + odagdp + remgdp + fdigdp + GDP_gross + GDP_pcap + country + year + autocracy*odalag + autocracy*remlag + autocracy*fdilag + autocracy*odagdp + autocracy*remgdp + autocracy*fdigdp + democracy*odalag + democracy*remlag + democracy*fdilag + democracy*odagdp + democracy*remgdp + democracy*fdigdp + GDP_pcap*remlag + GDP_pcap*remgdp, data = finaldev)
model2 <- glm(ginicon ~ odalag + remlag + fdilag + odagdp + remgdp + fdigdp + GDP_gross + GDP_pcap + country + year + autocracy*odalag + autocracy*remlag + autocracy*fdilag + autocracy*odagdp + autocracy*remgdp + autocracy*fdigdp + democracy*odalag + democracy*remlag + democracy*fdilag + democracy*odagdp + democracy*remgdp + democracy*fdigdp + GDP_pcap*remlag + GDP_pcap*remgdp, data = finaldev)
model3 <- glm(palmaratio ~ odalag + remlag + fdilag + odagdp + remgdp + fdigdp + GDP_gross + GDP_pcap + country + year + autocracy*odalag + autocracy*remlag + autocracy*fdilag + autocracy*odagdp + autocracy*remgdp + autocracy*fdigdp + democracy*odalag + democracy*remlag + democracy*fdilag + democracy*odagdp + democracy*remgdp + democracy*fdigdp + GDP_pcap*remlag + GDP_pcap*remgdp, data = finaldev)
model4 <- glm(theil ~ odalag + remlag + fdilag + odagdp + remgdp + fdigdp + GDP_gross + GDP_pcap + country + year + autocracy*odalag + autocracy*remlag + autocracy*fdilag + autocracy*odagdp + autocracy*remgdp + autocracy*fdigdp + democracy*odalag + democracy*remlag + democracy*fdilag + democracy*odagdp + democracy*remgdp + democracy*fdigdp + GDP_pcap*remlag + GDP_pcap*remgdp, data = finaldev)


stargazer(model1, model2, model3, model4)
stargazer(model5, model6, model7, model8)
#### Growth ####
model11 <- glm(growth ~ odalag + remlag + fdilag + odagdp + remgdp + fdigdp + democracy + autocracy + GDP_gross + GDP_pcap + country + year + democracy*odalag + democracy*remlag + democracy*fdilag + democracy*odagdp + democracy*remgdp + democracy*fdigdp +  + autocracy*odalag + autocracy*remlag + autocracy*fdilag + autocracy*odagdp + autocracy*remgdp + autocracy*fdigdp, data = noaz)
summary(model11)
stargazer(model11)
#### Share ####
model21 <- glm(share1_4 ~ odalag + remlag + fdilag + odagdp + remgdp + fdigdp + democracy + GDP_gross + GDP_pcap + country + year + democracy*odalag + democracy*remlag + democracy*fdilag + democracy*odagdp + democracy*remgdp + democracy*fdigdp + autocracy*odalag + autocracy*remlag + autocracy*fdilag + autocracy*odagdp + autocracy*remgdp + autocracy*fdigdp + GDP_pcap*remlag + GDP_pcap*remgdp, data = finaldev)
summary(model21)
stargazer(model21)

#### Poverty ####
model31 <- glm(Poverty ~ ODA + Remittances + FDI + coldwar + democracy + GDP_gross + GDP_pcap + region + year + coldwar*ODA, data = data)
summary(model31)

#### Democratization ####
model41 <- glm(polity2 ~ ODA + Remittances + FDI + GDP_gross + GDP_pcap + country + year, data = data)
summary(model41)

# Logged models
model51 <- glm(theil ~ logODA + logrem + logFDI + polity2 + region + year, data = finaldev)
summary(model51)

# Regression diagnostics
plot(model11)

sum(finaldev$FDI)

#### Appendix ####

demo <- subset(dbfin, democracy == 1)
auto <- subset(dbfin, autocracy == 1)
sum(demo$Remittances, na.rm = TRUE) / 618
sum(auto$Remittances, na.rm = TRUE) / 157
sum(demo$ODA, na.rm = TRUE) / 618
sum(auto$ODA, na.rm = TRUE) / 157
sum(demo$FDI, na.rm = TRUE) / 618
sum(auto$FDI, na.rm = TRUE) / 157

countriesToDrop<-c("Azerbaijan");
noaz <- finaldev[!(finaldev[,45]%in%countriesToDrop),]

####### Correlogram ####
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
my_data <- finaldev[,c(8,10,29,66)]
p.mat <- cor.mtest(my_data)
head(p.mat[, 1:5])

res <- cor(my_data, method = "pearson", use = "complete.obs")
corrplot(res, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank", title = "Correlogram", mar=c(0,0,1,0))


#### Difference between income and consumption Gini ####
finaldev <- finaldev[,c(1:8, 66, 9:65)]
agg <- aggregate(finaldev[, 8:9], list(finaldev$country), mean, na.omit = T)
agg$diff <- agg$gini - agg$ginicon

agg2 <- aggregate(finaldev[, 11:30], list(finaldev$country), mean, na.omit = T)
agg2$diff <- agg2$palmaratio - agg2$theil

ggplot(agg2, aes(agg2$diff)) + 
  geom_histogram(col="white", 
                 fill="black") + 
  labs(title="Comparison: consumption and income Gini") +
  labs(x="Difference between consumption and income Gini", y="Number of countries")

