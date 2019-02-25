##### 0. Packages & Dataset #####
#smth <- available.packages()
#deps <- tools::package_dependencies("reshape2", smth)$reshape2
#install.packages(deps)
#install.packages("gridExtra")
#install.packages("readstata13")
#install.packages("wbstats")
#install.packages("DataCombine")
#install.packages("reldist")
library(wbstats)
library(reshape2)
library(foreign)
library(readstata13)
library(ggplot2)
library(MASS)
library(stargazer)
library(RColorBrewer)
library(corrplot)
library(car)
library(DataCombine)
library(plyr)
library(gini)
library(reldist)
rm(list=ls())

wb <- merge()
wb$cy2 <- paste(wb$iso2c, wb$year)
wiidtest$iso2c <- wiidtest$Group.1
wiidtest$OECD <- wiidtest$Group.3
wiidtest$Group.3 <- NULL
wiidtest$incomegroup <- wiidtest$Group.2
wiidtest$Group.4 <- NULL
wb <- read.csv("C:/Users/User/Desktop/MA Thesis/Data/R/wb.csv", fill = TRUE, check.names=T)
wiidtest <- aggregate(wiid[, 5], list(wiid$Countrycode2, wiid$Incomegroup, wiid$OECD, wiid$Country), mean, na.omit = T)


wiid <- read.csv("C:/Users/User/Desktop/MA Thesis/WIID.csv", fill = TRUE, check.names=T)
wiid$cy <- paste(wiid$Country, wiid$Year)
wiid <- wiid[,c(56, 2:55)]



final <- read.csv("C:/Users/User/Desktop/MA Thesis/final.csv", fill = TRUE, check.names=T)
income <- read.csv("C:/Users/User/Desktop/MA Thesis/income.csv", fill = TRUE, check.names=T)
income$cy <- paste(income$country, income$year)
income <- income[,c(5,27)]
summary(wb$incomegroup)
wiid <- wiid[,c(4,7,44)]
wiid$Countrycode2 <- NULL
wb <- merge(wb, wiidtest, by = "iso2c", all.x=TRUE)
wiid$Gini <- NULL
wiid$cy <- paste(wiid$Country, wiid$Year)
final$cy <- paste(final$country, final$year)
wiidagg <- aggregate(wiid[, 5:54], list(wiid$cy), mean, na.omit = T)
summary(final$gini)
wiidagg$cy <- wiidagg$Group.1

rm(list=("wiid"))

wiidagg <- wiidagg[-c(1:6,8:51)]

#### 1.1. Get data ####
new_wb_cache <- wbcache() 
#wbsearch("corruption", cache = new_wb_cache)

wb_dat <- wb(indicator = c("BX.TRF.PWKR.CD.DT", 
                           "BX.KLT.DINV.CD.WD",
                           "DT.ODA.ALLD.CD",
                           "SI.POV.DDAY", 
                           "NY.GDP.MKTP.CD", 
                           "SP.POP.TOTL", 
                           "FP.CPI.TOTL.ZG", 
                           "NY.GDP.PCAP.CD"))


#### 1.2. Clean up ####
wb_countries <- wbcountries()
names(wb_countries)

wb_dat <- merge(wb_dat, y = wb_countries[c("iso2c", "region")], by = "iso2c", all.x = TRUE)
wb_dat <- subset(wb_dat, region != "Aggregates") # this also removes NAs

wb_dat$indicatorID[wb_dat$indicatorID == "BX.TRF.PWKR.CD.DT"] <- "Remittances"
wb_dat$indicatorID[wb_dat$indicatorID == "BX.KLT.DINV.CD.WD"] <- "FDI"
wb_dat$indicatorID[wb_dat$indicatorID == "DT.ODA.ALLD.CD"] <- "ODA"
wb_dat$indicatorID[wb_dat$indicatorID == "SI.POV.DDAY"] <- "Poverty"
wb_dat$indicatorID[wb_dat$indicatorID == "NY.GDP.MKTP.CD"] <- "GDP_gross"
wb_dat$indicatorID[wb_dat$indicatorID == "SP.POP.TOTL"] <- "Population"
wb_dat$indicatorID[wb_dat$indicatorID == "FP.CPI.TOTL.ZG"] <- "Inflation"
wb_dat$indicatorID[wb_dat$indicatorID == "NY.GDP.PCAP.CD"] <- "GDP_pcap"

#The dataset
wb_dat <- dcast(wb_dat, iso2c + country + date + region ~ indicatorID,  value.var = 'value')

wb_dat$Remittances[wb_dat$Remittances<=0] <- NA
wb_dat$FDI[wb_dat$FDI<=0] <- NA
wb_dat$ODA[wb_dat$ODA<=0] <- NA

wb_dat$logODA <- log(wb_dat$ODA)
wb_dat$logFDI <- log(wb_dat$FDI)
wb_dat$logrem <- log(wb_dat$Remittances)

wb <- wb_dat

rm(list = ("wb_dat"))

wb$cy <- paste(wb$country, wb$date)
wb$year <- wb$date
wb$date <- NULL

summary(finaldev$fdigdp)
#wbnk <- subset(wb, date == 2000 | date == 2005 | date == 2010 | date == 2015)
#wbagg <- aggregate(wb[, 5:11], list(wb$country), mean, na.omit = T)
#Plot
#ggplot(subset(wb, region == "South Asia"), aes(x = FDI, y = GINI)) + geom_point() + geom_smooth(method=lm)

#### 1.3. Combined dataset ####
comb <- merge(wiidagg, wb, by = "cy")
head(wiidagg$cy)
comb$Group.1 <- NULL
comb$GINI <- NULL
comb1 <- subset(comb, OECD==0)
write.csv(wb, file = "wb.csv")

which( colnames(comb1)=="EU" )

db <- comb1[ -c(3:5, 7:38, 41:51, 66) ]
db <- db[,c(1,6,7,8,9,2,3,10:24,4,5)]
rm(list = c("comb", "comb1", "wb", "wiidagg", "wb_countries", "new_wb_cache"))
db$Population.x <- NULL
wb$fdigdp <- wb$FDI/wb$GDP_gross
wb$odagdp <- wb$ODA/wb$GDP_gross
wb$remgdp <- wb$Remittances/wb$GDP_gross
db$logrem <- log(db$Remittances)
db$Inflation <- abs(db$Inflation)
db$Inflation[db$Inflation==0] <- NA
db$loginf <- log(db$Inflation)
db$population <- db$Population.x
write.csv(db, file = "db.csv")

POL <- read.csv("D:/Documents/Mannheim MA Political Science/III semester/Violence, Conflict, Peace/Paper/Minorities at risk database/p4v2016.csv", fill = TRUE)
POL$cy <- paste(POL$country, POL$year)
POL <- POL[ -c(1,6,7,22:36) ]
POL <- subset(POL, year >= 1960) 
POL <- POL[,c(8,19)]
wb <- merge(wb, POL, by = "cy", all.x=TRUE)

db$country.y <- NULL
db$date <- NULL
db$country <- db$country.x
db$country.x <- NULL

wb$democracy[wb$polity2>=6] <- 1
wb$democracy[wb$polity2<6] <- 0
wb$autocracy[wb$polity2<= -6] <- 1
wb$autocracy[wb$polity2> -6] <- 0
wb$anocracy[wb$polity2> -6 & wb$polity2< 6] <- 1
wb$anocracy[wb$polity2<= -6] <- 0
wb$anocracy[wb$polity2>= 6] <- 0

db$GDPbil <- db$GDP_gross/1000000000

which( colnames(db)=="country" )
db <- db[ -c(5:7, 9:18) ]
#db <- db[,c(1:3, 6, 30, 4, 7, 5, 8, 14, 9, 15:17, 20, 10:11, 34, 12:13, 18:19, 21:22, 28:29, 25:27, 23:24, 31:34)]
rm(POL)
write.csv(db, file = "db.csv")

#### 1.4. Adding CPI####
cpi <- read.csv("C:/Users/User/Desktop/MA Thesis/cpi.csv", fill = TRUE, check.names=F)
cpi <- melt(cpi, id=c("country"))
cpi$date <- cpi$variable
cpi$cpi <- cpi$value
cpi$variable <- NULL
cpi$value <- NULL
cpi$cy <- paste(cpi$country, cpi$date)
cpi$cpivalue <- as.numeric(cpi$cpi)
cpi$cpi <- NULL
cpi$country <- NULL
cpi$date <- NULL

wb <- merge(wb, cpi, by = "cy", all.x = TRUE)
rm(cpi)

write.csv(dbcpi, file = "dbcpi.csv")

#### 2.1. Analysis Part ####
# Main model
dbcpi <- read.csv("C:/Users/User/Desktop/MA Thesis/dbcpi.csv", fill = TRUE, check.names=T)
dbcpi$X <- NULL
dbcpi$logGDPpcap <- log(dbcpi$GDP_pcap)
finaldev$coldwar[finaldev$year<1992] <- 1
finaldev$coldwar[finaldev$year>=1992] <- 0

#logrem*cpivalue
summary(model1)
stargazer(model1, model2)

# Melting & Plotting
chart_data <- db[c(4,22,23,29)]
agg <- aggregate(chart_data[, 2:4], list(chart_data$year), mean, na.omit = T)

chart_data1 <- melt(chart_data, id='year')
names(chart_data1) <- c('x', 'func', 'value')

ggplot() +
  geom_line(data = chart_data1, aes(x = x, y = value, color = func), size = 1)+
  xlab("x axis") +
  ylab("y axis")

# Overall, I think it is better to stick with dbcpi due to the fact that theere is no data on
# remittances in 1960s, and only scarce data on remittances and ODA2 in 60s and 70s. Will
# decide on 80s later.
summary(dbcpi$Poverty2)
dbcpi$region
harale <- subset(dbcpi, region=="Latin America & Caribbean")

db$logGDP <- log(db$GDPbil)
ggplot(db, aes(Gini, logGDP)) +
  geom_jitter(aes(colour = democracy)) +
  geom_smooth(method=lm)

dbcpi$afr <- dbcpi$region
qplot(dbcpi$logGDPpcap)

# Herzer and Nunnekamp
herz <- subset(db, year >= 1970 & year <= 1995)
herz2 <- subset(herz, country == "Argentina" | country == "Bangladesh" | country == "Brazil" | 
                country == "Chile" | country == "China" | country == "Colombia" | country == "Costa Rica" 
                | country == "Egypt" | country == "El Salvador" | country == "Guatemala" | country == "India" |
                  country == "Indonesia" | country == "Jordan" | country == "Kenya" | country == "Madagascar" |
                  country == "Malawi" | country == "Malaysia" | country == "Mauritius" | country == "Mexico" |
                  country == "Morocco" | country == "Pakistan" | country == "Panama" | country == "Philippinnes" |
                  country == "Sierra Leone" | country == "Trinidad and Tobago" | country == "Tunisia" |
                  country == "Turkey" | country == "Uruguay" | country == "Venezuela" | country == "Zambia")
modelherz <- glm(Gini ~ logODA, data = herz2)
summary(modelherz)

# WIID Gini gave results opposite to Herzer and Nunnekamp. Now trying EHII.

ehii <- read.csv("C:/Users/User/Desktop/MA Thesis/ehii.csv", header = TRUE, fill = TRUE, check.names=F)
ehii2 <- melt(ehii, id=c("Country"))
ehii2$year <- ehii2$variable
ehii2$Giniehii <- ehii2$value
ehii2$variable <- NULL
ehii2$value <- NULL
ehii2$cy <- paste(ehii2$Country, ehii2$year)
ehii2$year <- NULL
TwoGinis <- merge(herz2, ehii2, by = "cy")
modelehii <- glm(Giniehii ~ logODA, data = TwoGinis)
modelehii2 <- glm(Giniehii ~ logODA + logrem + logFDI + logGDPpcap + democracy + country + year,
                  data = TwoGinis)
stargazer(modelherz,modelehii, modelehii2)

# EHII data analysis is in line with Herzer and Nunnekamp (in both bivariate and multivariate 
# analyses). This means that using WIID changes the picture.

# Now integrating EHII with WIID and trying to re-do model 4 using EHII.
ehii2$Country <- NULL
bigdb <- merge(ehii2, db, by = "cy")

model5 <- glm(Giniehii ~ logODA + logrem + logFDI + logGDPpcap + democracy +
                country + year, data = bigdb)
model6 <- glm(Gini ~ logODA + logrem + logFDI + logGDPpcap + democracy + country + year, data = db)
summary(model6)
stargazer(model5, model6)
# There are fewer observations in bigdb as compared to db. The logODA coef is positive.
# Due to the fact that WIID 3.4 is released in January 2017, I decide to stick to it due to
# it being more up-to-date and more comprehensive. Mic drop, dear Herzer and Nunnekamp.
b <- modely$coefficients
vcov <- vcov(modely)
nsim = 1000

set.seed(07052018)
S <- mvrnorm(nsim, b, vcov)


#polity2_sim <- seq(min(MAR$polity22, na.rm=TRUE), max(MAR$polity22, na.rm=TRUE), length=1000)
#GPRO_sim <- seq(min(MAR$GPRO), max(MAR$GPRO), length=1000)
rem_sim <- seq(min(no_dev$logrem, na.rm = TRUE), max(no_dev$logrem, na.rm = TRUE), length=16)

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

# Getting Gini and GDP pcap since 1960 for the plot
wb_dat2 <- wb(indicator = c("NY.GDP.PCAP.CD"))
wb_dat2 <- merge(wb_dat2, y = wb_countries[c("iso2c", "region")], by = "iso2c", all.x = TRUE)
wb_dat2 <- subset(wb_dat2, region != "Aggregates") # this also removes NAs
wb_dat2$indicatorID[wb_dat2$indicatorID == "NY.GDP.PCAP.CD"] <- "GDP_pcap"
wb_dat2 <- dcast(wb_dat2, iso2c + country + date + region ~ indicatorID,  value.var = 'value')
wb_dat2$cy <- paste(wb_dat2$country, wb_dat2$date)
comb2 <- merge(wiidagg, wb_dat2, by = "cy")
combagg <- aggregate(comb2[, 57], list(comb2$date), mean)
ggplot(combagg, aes(Group.1, x)) + geom_line() + geom_point()
# Trying to get aggregate Gini and GDP per capita measures for each year
db[is.na(db)] <- 0
agggini <- aggregate(db[, 9], list(db$year), mean)
db[db == 0] <- NA
aggGDPpcap <- aggregate(db[, 12], list(db$year), mean)
aggGDPpcap <- aggGDPpcap[ -c(1:30), ]
ggplot(agggini, aes(Group.1, x)) + geom_line() + geom_point()
pdf(file = "GDPpcap.pdf")
ggplot(aggGDPpcap, aes(Group.1, x)) + geom_line() + geom_point() + 
  labs(title = "Average annual GDP Per Capita for non-OECD countries, current US$", 
       x = "Year", y = "GDP per capita")
dev.off()

hi10 <- read.csv("C:/Users/User/Desktop/MA Thesis/hi10.csv", fill = TRUE, check.names = F)
hi10$country <- hi10$`Country Name`
hi10$scode <- hi10$`Country Code`
hi10$`Country Name` <- NULL
hi10$`Country Code` <- NULL
hi10$scode <- NULL
hi10 <- melt(hi10, id=c("country"))
hi10$year <- hi10$variable
hi10$hi10 <- hi10$value
hi10$variable <- NULL
hi10$value <- NULL
hi10$cy <- paste(hi10$country, hi10$year)
hi10$country <- NULL
hi10$year <- NULL
hi10 <- hi10[complete.cases(hi10), ]

hi20 <- read.csv("C:/Users/User/Desktop/MA Thesis/hi20.csv", fill = TRUE, check.names = F)
hi20 <- melt(hi20, id=c("country"))
hi20$year <- hi20$variable
hi20$hi20 <- hi20$value
hi20$variable <- NULL
hi20$value <- NULL
hi20$cy <- paste(hi20$country, hi20$year)
hi20$country <- NULL
hi20$year <- NULL
hi20 <- hi20[complete.cases(hi20), ]

sec20 <- read.csv("C:/Users/User/Desktop/MA Thesis/sec20.csv", fill = TRUE, check.names = F)
sec20 <- melt(sec20, id=c("country"))
sec20$year <- sec20$variable
sec20$sec20 <- sec20$value
sec20$variable <- NULL
sec20$value <- NULL
sec20$cy <- paste(sec20$country, sec20$year)
sec20$country <- NULL
sec20$year <- NULL
sec20 <- sec20[complete.cases(sec20), ]

thr20 <- read.csv("C:/Users/User/Desktop/MA Thesis/thr20.csv", fill = TRUE, check.names = F)
thr20 <- melt(thr20, id=c("country"))
thr20$year <- thr20$variable
thr20$thr20 <- thr20$value
thr20$variable <- NULL
thr20$value <- NULL
thr20$cy <- paste(thr20$country, thr20$year)
thr20$country <- NULL
thr20$year <- NULL
thr20 <- thr20[complete.cases(thr20), ]

four20 <- read.csv("C:/Users/User/Desktop/MA Thesis/four20.csv", fill = TRUE, check.names = F)
four20 <- melt(four20, id=c("country"))
four20$year <- four20$variable
four20$four20 <- four20$value
four20$variable <- NULL
four20$value <- NULL
four20$cy <- paste(four20$country, four20$year)
four20$country <- NULL
four20$year <- NULL
four20 <- four20[complete.cases(four20), ]

low20 <- read.csv("C:/Users/User/Desktop/MA Thesis/low20.csv", fill = TRUE, check.names = F)
low20 <- melt(low20, id=c("country"))
low20$year <- low20$variable
low20$low20 <- low20$value
low20$variable <- NULL
low20$value <- NULL
low20$cy <- paste(low20$country, low20$year)
low20$country <- NULL
low20$year <- NULL
low20 <- low20[complete.cases(low20), ]

db <- merge(db, low20, by = "cy")
db <- merge(db, four20, by = "cy")
db <- merge(db, thr20, by = "cy")
db <- merge(db, sec20, by = "cy")
db <- merge(db, hi20, by = "cy")
db <- merge(db, hi10, by = "cy")
db$logGDPpcap <- log(db2$GDP_pcap)
write.csv(wb, file = "wb.csv")

portfolio <- read.csv("C:/Users/User/Desktop/MA Thesis/portfolio.csv", fill = TRUE, check.names = F)
portfolio <- melt(portfolio, id=c("country"))
portfolio$year <- portfolio$variable
portfolio$portfolio <- portfolio$value
portfolio$variable <- NULL
portfolio$value <- NULL
portfolio$cy <- paste(portfolio$country, portfolio$year)
portfolio$country <- NULL
portfolio$year <- NULL
portfolio <- portfolio[complete.cases(portfolio), ]
#decided not to include portfolio due to 50% missing values

#Adding GDP growth
growth <- read.csv("C:/Users/User/Desktop/MA Thesis/growth.csv", fill = TRUE, check.names = F)
growth <- melt(growth, id=c("country"))
growth$year <- growth$variable
growth$growth <- growth$value
growth$variable <- NULL
growth$value <- NULL
growth$cy <- paste(growth$country, growth$year)
growth$country <- NULL
growth$year <- NULL
growth <- growth[complete.cases(growth), ]

no_dev <- merge(no_dev, growth, by = "cy", all.x = TRUE)
dbcpi <- merge(dbcpi, growth, by = "cy")
dbcohort <- merge(dbcohort, growth, by = "cy")
write.csv(dbfin, file = "dbfin.csv")
write.csv(dbcpi, file = "dbcpi.csv")
write.csv(dbcohort, file = "dbcohort.csv")
dbcpi$logGDPpcap <- log(dbcpi$GDP_pcap) 
write.csv(dbcpi, file = "dbcpi.csv")

growup <- read.csv("C:/Users/User/Desktop/MA Thesis/Growup/data.csv", fill = TRUE, check.names=T)
growup <- growup[ -c(1:2, 6:8, 10, 12, 14:16, 18, 20, 22, 24: 26) ]
growagg <- aggregate(growup[, 4:10], list(growup$groupname, growup$countryname), mean)
growagg2 <- growagg[complete.cases(growagg$gdp90_total, growagg$gdp95_total, growagg$gdp00_total, growagg$gdp05_total), ]
growagg2$pop95_total <- (growagg2$pop90_total + growagg2$pop00_total)/2
growagg2$pop05_total <- (growagg2$pop00_total + growagg2$pop10_total)/2
growagg2$gdppcap_90 <- growagg2$gdp90_total/growagg2$pop90_total
growagg2$gdppcap_95 <- growagg2$gdp95_total/growagg2$pop95_total
growagg2$gdppcap_00 <- growagg2$gdp00_total/growagg2$pop00_total
growagg2$gdppcap_05 <- growagg2$gdp05_total/growagg2$pop05_total
growupnew <- growagg2[ -c(3:11)]
growupnew$Group <- growupnew$Group.1
growupnew$country <- growupnew$Group.2
growupnew <- growupnew[ -c(1:2)]
growupnew <- growupnew[,c(5,6,1,2,3,4)]
write.csv(growupnew, file = "growupedit.csv")

growupnew$gdppcap_90

x <- c(1.03, 0.76)
gini(x)

ginicities <- aggregate(gdppcap_90 ~ country,
                        data = growupnew,
                        FUN = "gini")
ginicities$year <- 1990
ginicities$cy <- paste(ginicities$country, ginicities$year)
colnames(ginicities)[2] <- "GiniGU"
giniGU90 <- ginicities[ -c(1,3)]

dbfin <- merge(dbfin, giniGU90, by = "cy", all.x=TRUE)

marketcap <- read.csv("C:/Users/User/Desktop/MA Thesis/marketcap.csv", fill = TRUE, check.names = F)
marketcap <- melt(marketcap, id=c("country"))
marketcap$year <- marketcap$variable
marketcap$marketcap <- marketcap$value
marketcap$variable <- NULL
marketcap$value <- NULL
marketcap$cy <- paste(marketcap$country, marketcap$year)
marketcap$country <- NULL
marketcap$year <- NULL
marketcap <- marketcap[complete.cases(marketcap), ]

gdp <- read.csv("C:/Users/User/Desktop/MA Thesis/gdp.csv", fill = TRUE, check.names = F)
gdp <- melt(gdp, id=c("country"))
gdp$year <- gdp$variable
gdp$gdp <- gdp$value
gdp$variable <- NULL
gdp$value <- NULL
gdp$cy <- paste(gdp$country, gdp$year)
gdp$country <- NULL
gdp$year <- NULL
gdp <- gdp[complete.cases(gdp), ]


edu <- read.csv("C:/Users/User/Desktop/MA Thesis/Data/R/edu.csv", fill = TRUE, check.names = F)
edu <- melt(edu, id=c("country"))
edu$year <- edu$variable
edu$edu <- edu$value
edu$variable <- NULL
edu$value <- NULL
edu$cy <- paste(edu$country, edu$year)
edu$country <- NULL
edu$year <- NULL
edu <- edu[complete.cases(edu), ]

fd <- read.csv("C:/Users/User/Desktop/MA Thesis/Data/R/fd.csv", fill = TRUE, check.names = F)
fd <- melt(fd, id=c("country"))
fd$year <- fd$variable
fd$fd <- fd$value
fd$variable <- NULL
fd$value <- NULL
fd$cy <- paste(fd$country, fd$year)
fd$country <- NULL
fd$year <- NULL
fd <- fd[complete.cases(fd), ]
fd <- aggregate(fd[, 1], list(fd$cy), mean, na.omit = T)
fd$x <- NULL

tax <- read.csv("C:/Users/User/Desktop/MA Thesis/tax.csv", fill = TRUE, check.names = F)
names(tax)[1]<-paste("country")
tax <- melt(tax, id=c("country"))
tax$year <- tax$variable
tax$tax <- tax$value
tax$variable <- NULL
tax$value <- NULL
tax$cy <- paste(tax$country, tax$year)
tax$country <- NULL
tax$year <- NULL
tax <- tax[complete.cases(tax), ]

consinq <- read.csv("C:/Users/User/Desktop/MA Thesis/consinq.csv", fill = TRUE, check.names = F)
consinq$cy <- paste(consinq$country, consinq$year)
consinq$country <- NULL
consinq$year <- NULL
consinq <- consinq[,c(25,1,3,5,24,10:19)]

socsp <- read.csv("C:/Users/User/Desktop/MA Thesis/socsp.csv", fill = TRUE, check.names = F)
govsp <- read.csv("C:/Users/User/Desktop/MA Thesis/govsp.csv", fill = TRUE, check.names = F)
socsp$cy <- paste(socsp$Entity, socsp$Year)
govsp$cy <- paste(govsp$Entity, govsp$Year)
govsp$Entity <- NULL
govsp$govsp <- govsp[, 3]
govsp[, 1:3] <- NULL

pensions <- read.csv("C:/Users/User/Desktop/MA Thesis/pension.csv", fill = TRUE, check.names = F)
pensions$cy <- paste(pensions$country, pensions$year)
pensions$year <- NULL


finaldev <- merge(finaldev, pensions, by = "cy", all.x=TRUE)
summary(dbfin$Poverty)
write.csv(dbfin, file = "dbfin.csv")
dbfin$loggdp <- log(dbfin$gdp)
summary(dbfin$tax)
qplot(dbfin$loggdp)

final <- merge(consinq, wb, by = "cy", all.x = TRUE)
final <- final[ -c(4,6:8,10:20,44,46) ]
final$country <- final$country.y
write.csv(final, file = "final.csv")
final <- read.csv("C:/Users/User/Desktop/MA Thesis/final.csv", fill = TRUE)
?aggregate

blabla <- aggregate(finaldev[, 8], list(finaldev$country), mean, na.omit = T)
bla <- subset(finaldev, year < 1990) 
final<- final[-c(40)]
final$odagdp <- final$ODA
final <- merge(final, wiidagg, by = "cy", all.x = TRUE)
write.csv(wb, file = "wb.csv")
summary(final$Remittances)
#finaldev <- final[-c(306:408, 552:608, 1196:1251, 1459:1465, 1936:1957, 1991:2047, 2422:2428, 
#                     2544:2590, 2591:2647, 2783:2871, 2929:2985, ),] 
finaldev <- final[complete.cases(final[, 33]), ]
finaldev <- finaldev[complete.cases(finaldev[, 48]), ]
finaldev <- finaldev[complete.cases(finaldev[, 44]), ]

summary(finaldev$marketcap)
finaldev <- merge(finaldev, govsp, by = "cy", all.x = TRUE)
summary(finaldev$govsp)
# Lagging financial inflows
finaldev <- ddply(finaldev, .(country), transform, gdplag =
                    c(NA, GDP_gross[-length(GDP_gross)]
                    )
)

finaldev$logODA <- log(finaldev$odalag)
finaldev$logFDI <- log(finaldev$fdilag)
finaldev$logrem <- log(finaldev$remlag)

finaldev[,2] <- NULL

finaldev$country.y <- NULL
finaldev$odagdp <- finaldev$odalag / finaldev$gdplag
finaldev$remgdp <- finaldev$remlag / finaldev$gdplag
finaldev$fdigdp <- finaldev$fdilag / finaldev$gdplag

write.csv(finaldev, file = "finaldev.csv")

final <- merge(final, fd, by = "cy", all.x = TRUE)
finaldev$share1_4 <- finaldev$share1 + finaldev$share2 + finaldev$share3 + finaldev$share4

finaldev2015 <- subset(finaldev, year > 2010)
finaldev2015 <- aggregate(finaldev2015[, 60:62], list(finaldev2015$country), mean, na.omit = T)
finaldev2015$country <- finaldev2015$Group.1
finaldev <- merge(finaldev, edu, by = "cy")
summary(finaldev$Inflation)
diff <- finaldev2015[finaldev2015$year == "2015" | finaldev2015$year == "2014", "remgdp"] - finaldev2015[finaldev2015$year == "2011", "remgdp"] 
auto <- subset(finaldev, autocracy == 1)
demo <- subset(finaldev, democracy == 1)



#### Migration ####
migration <- read.csv("C:/Users/User/Desktop/MA Thesis/migration_2014.csv", fill = TRUE, check.names=T)
agg <- aggregate(migration[, 13:16], list(migration$country_orig), sum, na.omit = T)
write.csv(agg, file = "agg.csv")
agg$country <- agg$Group.1
agg$Group.1 <- NULL
agg <- melt(agg, id=c("country"))
agg$year <- agg$variable
agg$migrants <- agg$value
agg$yearx[agg$year == "countryflow_1990"] <- 1995
agg$yearx[agg$year == "countryflow_1995"] <- 2000
agg$yearx[agg$year == "countryflow_2000"] <- 2005
agg$yearx[agg$year == "countryflow_2005"] <- 2010
agg$cy <- paste(agg$country, agg$year)

sbst <- subset(finaldev, year < 1995)
summary(finaldev$Income)
finaldev <- finaldev[,c(1,46,4:11,16:27,30:34,36,37,40,42:43,47,49:57,60,66,70)]
summary(finaldev$pension)
wb <- merge(wb, income, by = "cy", all.x = TRUE)
test <- subset(wb, Poverty >= 0)
