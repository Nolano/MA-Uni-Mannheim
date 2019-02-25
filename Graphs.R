ggplot(dbfin, aes(Gini, logGDP)) +
  geom_jitter() +
  geom_smooth(method=lm)

dbcohort$logGDP <- log(dbcohort$GDP_gross)
qplot(dbfin$logGDP)

summary(dbfin$Gini)

ggplot(finaldev, aes(x=factor(year)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Number of Gini coefficient observations per year", 
       x = "Year", y = "Count")

ggplot() + geom_jitter(dbcpi, mapping=aes(x=fdigdp, y=cpivalue), size=1)

dbfin$odagdp100 <- dbfin$odagdp * 100

ggplot(final, aes(Giniwiid, gini)) +
  geom_jitter(aes(colour = region)) +
  geom_smooth(method=lm) +
  labs(title = "Polity by Inequality", 
       x = "Polity", 
       y = "Inequality", colour = "Region")

ggplot(no_dev, aes(growth, loggdpcap)) +
  geom_jitter(aes(colour = democracy)) +
  geom_smooth(method=lm)# +
  labs(title = "Correlation between economic growth and income inequality", 
       x = "Economic growth", y = "Gini coefficient")

ggplot(finaldev, aes(polity2, ginicon)) +
  geom_jitter(aes(colour = region)) +
  geom_smooth(method=lm) +
  labs(title = "Effect of Polity Score on Consumption-Based Gini Coefficient", 
       x = "Polity", y = "Gini Coefficient (Consumption)", colour = "Region")



ggplot(finaldev, aes(autocracy, gini)) +
  geom_jitter(aes(colour = region)) +
  geom_smooth(method=lm)# +
  labs(title = "Effect of Polity Score on Consumption-Based Gini", 
       x = "Polity", 
       y = "Gini Coefficient", colour = "Region")# +
  #geom_text(aes(label=cy),hjust=0, vjust=0, check_overlap = FALSE)

DataSlid2 <- slide(dbfin, Var = "logrem", GroupVar = "year", slideBy = 1)

dbfin <- ddply(dbfin, .(country), transform, GiniLag1 =
                c(NA, Gini[-length(Gini)]
                )
)
summary(dbfin3$GiniLag1)

aggrem <- subset(dbfin, remgdp > 0)

dbtemp <- dbfin
dbtemp <- dbfin[complete.cases(dbfin$remgdp, dbfin$odagdp, dbfin$fdigdp), ]
finaldev$fdisameyear <- finaldev$FDI / finaldev$GDP_gross
finaldev$odasameyear <- finaldev$ODA / finaldev$GDP_gross
finaldev$remsameyear <- finaldev$Remittances / finaldev$GDP_gross

agg3 <- aggregate(finaldev[, 67:69], list(finaldev$year), mean, na.omit = T)

ggplot(agg3, aes(Group.1)) + 
  geom_line(aes(y = odasameyear, colour = "ODA")) +
  geom_line(aes(y = fdisameyear, colour = "FDI")) + 
  geom_line(aes(y = remsameyear, colour = "Remittances")) +
  labs(title = "Share of three types of inflows in GDP of developing countries", x = "Year", y = "Share in GDP", colour = "Financial inflows") +
  scale_x_continuous(limits = c(1995, 2015))

agg <- aggregate(dbfin[, 42], list(dbfin$year), mean, na.omit = T)

ggplot(data=agg, aes(x=x, y=Group.1)) +
  geom_bar(stat="identity")

Swaziland <- subset(finaldev, country == "Swaziland")
ggplot(Georgia, aes(year, Gini)) +
  geom_path()+
  geom_point()

modelx <- glm(palmaratio ~ remlag + odalag + fdilag + remgdp + odagdp + fdigdp + GDP_gross + GDP_pcap + year, data = Brazil)
summary(modelx)

var(duration)
summary(finaldev$Inflation)
