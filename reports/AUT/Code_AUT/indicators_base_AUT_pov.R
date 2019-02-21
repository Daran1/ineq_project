# ------------------------------------------------------------------------
#
# Indicators AUT
#
# -------------------------------------------------------------------------


library(readr)
library(dplyr)
library(survey)
library(convey)
library(knitr)
library(ggplot2)
library(svglite)
library(laeken)

# Source connection 
# source("R/_connection.R")

# Source Setup scripts to provide data
#source("reports/AUT/Code_AUT/_setup_AUT.R")


# Subsetting --------------------------------------------------------------

#### To get useful results we may want to subset to only positive 
#### income and people over 20

#### Continue here
# Positive income
silc.inc_1 <- silc.rph 

# Positive income > 20
silc.inc_2 <- silc.rph %>% filter(age >= 20)

# Creating Survey Objects -------------------------------------------------
# Positive income for all 
silc.inc_1.svy <- svydesign(ids =  ~ id_h,
                         strata = ~rb020,
                         weights = ~rb050,
                         data = silc.inc_1) %>% convey_prep()

# Positive income > 20 (only for people over 20 years of age)
silc.inc_2.svy <- svydesign(ids = ~id_h,
                         strata = ~rb020,
                         weights = ~rb050,
                         data = silc.inc_2) %>% convey_prep()

#########-------------------------------------------------------------------------
###### For each variable calculate indicators (Mean, Median, Gini, Top10%, 80/20)

#### Pre-tax factor income (Canberra Income):Can_inc

# Mean for whole period
svymean(~Can_inc, silc.inc_1.svy)

#Mean yearly
mean_p1_1 <- svyby(~Can_inc, by=~rb010, design=silc.inc_1.svy, FUN=svymean)

# Medianfor whole period
svyquantile(~Can_inc, silc.inc_1.svy, quantiles = c(0.5))

#Median yearly
median_p1_1 <- svyby(~Can_inc, by=~rb010, design=silc.inc_1.svy, 
                     FUN=svyquantile, c(0.5), ci=TRUE)


# Top 10% Share 
svytotal(~Can_inc, subset(silc.inc_1.svy, pb020 == "AT" & Can_inc >= 
                              as.numeric(
                                svyquantile(~Can_inc, silc.inc_1.svy, quantile = 0.9)))) / 
  svytotal(~Can_inc, subset(silc.inc_1.svy, pb020 == "AT"))

# Top 10% Share yearly
share <- vector("numeric", length(2005:2017))
j <- 1

for(year in 2005:2017) {
  svy_subset <- subset(silc.inc_1.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, Can_inc >= as.numeric(svyquantile(~Can_inc, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~Can_inc, svy_top_10) / svytotal(~Can_inc, svy_subset)
  j <- j + 1
}

years_top10_p1_1 <- data.frame(top_p1_1 = share)

years_top10_p1_1

# 80/20 Ratio for whole period
svyqsr(~Can_inc, silc.inc_1.svy, 0.2, 0.8)

# 80/20 Ratio yearly
p8020_p1_1 <- svyby(~Can_inc, by=~rb010, design=silc.inc_1.svy, FUN=svyqsr)

# Gini for whole period
svygini(~Can_inc, silc.inc_1.svy)

#Gini yearly
Gini_p1_1 <- svyby(~Can_inc, by=~rb010, design=silc.inc_1.svy, FUN=svygini, 
                   c(0.5), ci=TRUE)

# Theil Index for whole period
svygei(~Can_inc, silc.inc_1.svy, epsilon = 1)

# Theil Index yearly
Theil_p1_1 <- svyby(~Can_inc, by=~rb010, design=silc.inc_1.svy, FUN=svygei,
                    epsilon = 1)


#########-------------------------------------------------------------------------
#########
#### Pre-tax national income:prenatincom

svymean(~prenatincom, silc.inc_1.svy)

#Mean yearly
mean_p1_2 <- svyby(~prenatincom, by=~rb010, design=silc.inc_1.svy, FUN=svymean)

# Medianfor whole period
svyquantile(~prenatincom, silc.inc_1.svy, quantiles = c(0.5))

#Median yearly
median_p1_2 <- svyby(~prenatincom, by=~rb010, design=silc.inc_1.svy, FUN=svyquantile,
                     c(0.5), ci=TRUE)

# Top 10% Share 
svytotal(~prenatincom, subset(silc.inc_1.svy, pb020 == "AT" & prenatincom >= 
                                as.numeric(svyquantile(~prenatincom, silc.inc_1.svy, 
                                                       quantile = 0.9)))) / 
  svytotal(~prenatincom, subset(silc.inc_1.svy, pb020 == "AT"))

# Top 10% Share yearly
share <- vector("numeric", length(2005:2017))
j <- 1

for(year in 2005:2017) {
  svy_subset <- subset(silc.inc_1.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, prenatincom >= as.numeric(svyquantile(~prenatincom, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~prenatincom, svy_top_10) / svytotal(~prenatincom, svy_subset)
  j <- j + 1
}

years_top10_p1_2 <- data.frame(top_p1_2 = share)

years_top10_p1_2 

# 80/20 Ratio for whole period
svyqsr(~prenatincom, silc.inc_1.svy, 0.2, 0.8)

# 80/20 Ratio yearly
p8020_p1_2 <- svyby(~prenatincom, by=~rb010, design=silc.inc_1.svy, FUN=svyqsr)

# Gini for whole period
svygini(~Prenatincom, silc.inc_1.svy)

#Gini yearly
Gini_p1_2 <- svyby(~prenatincom, by=~rb010, design=silc.inc_1.svy, FUN=svygini, 
                   c(0.5), ci=TRUE)

# Theil Index for whole period
svygei(~prenatincom, silc.inc_1.svy, epsilon = 1)

# Theil Index yearly
Theil_p1_2 <- svyby(~prenatincom, by=~rb010, design=silc.inc_1.svy, FUN=svygei,
                    epsilon = 1)



#########-------------------------------------------------------------------------
#### Post-tax disposable Income: posttax

svymean(~posttax, silc.inc_1.svy)

#Mean yearly
mean_p1_3 <- svyby(~posttax, by=~rb010, design=silc.inc_1.svy, FUN=svymean)

# Medianfor whole period
svyquantile(~posttax, silc.inc_1.svy, quantiles = c(0.5))

#Median yearly
median_p1_3 <- svyby(~posttax, by=~rb010, design=silc.inc_1.svy, FUN=svyquantile,
                     c(0.5), ci=TRUE)


# Top 10% Share 
svytotal(~posttax, subset(silc.inc_1.svy, pb020 == "AT" & posttax >= 
                            as.numeric(svyquantile(~posttax, silc.inc_1.svy, 
                                                   quantile = 0.9)))) / 
  svytotal(~posttax, subset(silc.inc_1.svy, pb020 == "AT"))

# Top 10% Share yearly
share <- vector("numeric", length(2005:2017))
j <- 1

for(year in 2005:2017) {
  svy_subset <- subset(silc.inc_1.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, posttax >= as.numeric(svyquantile(~posttax, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~posttax, svy_top_10) / svytotal(~posttax, svy_subset)
  j <- j + 1
}

years_top10_p1_3 <- data.frame(top_p1_3 = share)

years_top10_p1_3 


# 80/20 Ratio for whole period
svyqsr(~posttax, silc.inc_1.svy, 0.2, 0.8)

# 80/20 Ratioyearly
p8020_p1_3 <- svyby(~posttax, by=~rb010, design=silc.inc_1.svy, FUN=svyqsr)

# Gini for whole period
svygini(~posttax, silc.inc_1.svy)

#Gini yearly
Gini_p1_3 <- svyby(~posttax, by=~rb010, design=silc.inc_1.svy, FUN=svygini,
                   c(0.5), ci=TRUE)

# Theil Index for whole period
svygei(~posttax, silc.inc_1.svy, epsilon = 1)

# Theil Index yearly
Theil_p1_3 <- svyby(~posttax, by=~rb010, design=silc.inc_1.svy, FUN=svygei, 
                    epsilon = 1)


#-------------------------------------------------------------------------
#
# P2 (Wid. World)
#
#-------------------------------------------------------------------------
#
#For each variable calculate indicators (Mean, Median, Gini, Top10%, 80/20)
#
# Pre-tax factor income (Canberra Income):income_wid_1

# Mean for whole period
svymean(~income_wid_1, silc.inc_2.svy)

#Mean yearly
mean_p2_1 <- svyby(~income_wid_1, by=~rb010, design=silc.inc_2.svy, FUN=svymean)

# Medianfor whole period
svyquantile(~income_wid_1, silc.inc_2.svy, quantiles = c(0.5))

#Median yearly
median_p2_1 <- svyby(~income_wid_1, by=~rb010, design=silc.inc_2.svy, FUN=svyquantile, c(0.5), ci=TRUE)


# Top 10% Share 
svytotal(~income_wid_1, subset(silc.inc_2.svy, pb020 == "AT" & income_wid_1 >= 
                            as.numeric(svyquantile(~income_wid_1, silc.inc_2.svy, quantile = 0.9)))) / 
  svytotal(~income_wid_1, subset(silc.inc_2.svy, pb020 == "AT"))

# Top 10% Share yearly

share <- vector("numeric", length(2005:2017))
j <- 1

for(year in 2005:2017) {
  svy_subset <- subset(silc.inc_2.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, income_wid_1 >= as.numeric(svyquantile(~income_wid_1, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~income_wid_1, svy_top_10) / svytotal(~income_wid_1, svy_subset)
  j <- j + 1
}

years_top10_p2_1 <- data.frame(top_p2_1 = share)

years_top10_p2_1


# 80/20 Ratio for whole period
svyqsr(~income_wid_1, silc.inc_2.svy, 0.2, 0.8)

# 80/20 Ratio yearly
p8020_p2_1 <- svyby(~income_wid_1, by=~rb010, design=silc.inc_2.svy, FUN=svyqsr)

# Gini for whole period
svygini(~income_wid_1, silc.inc_2.svy)

#Gini yearly
Gini_p2_1 <- svyby(~income_wid_1, by=~rb010, design=silc.inc_2.svy, FUN=svygini, c(0.5), ci=TRUE)

# Theil Index for whole period
svygei(~income_wid_1, silc.inc_2.svy, epsilon = 1)

# Theil Index yearly
Theil_p2_1 <- svyby(~income_wid_1, by=~rb010, design=silc.inc_2.svy, FUN=svygei, epsilon = 1)


#########-------------------------------------------------------------------------
#########
#### Pre-tax national income:income_wid_2

# Mean for whole period
svymean(~income_wid_2, silc.inc_2.svy)

#Mean yearly
mean_p2_2 <- svyby(~income_wid_2, by=~rb010, design=silc.inc_2.svy, FUN=svymean)

# Medianfor whole period
svyquantile(~income_wid_2, silc.inc_2.svy, quantiles = c(0.5))

#Median yearly
median_p2_2 <- svyby(~income_wid_2, by=~rb010, design=silc.inc_2.svy, FUN=svyquantile, c(0.5), ci=TRUE)


# Top 10% Share 
svytotal(~income_wid_2, subset(silc.inc_2.svy, pb020 == "AT" & income_wid_2 >= 
                                 as.numeric(svyquantile(~income_wid_2, silc.inc_2.svy, quantile = 0.9)))) / 
  svytotal(~income_wid_2, subset(silc.inc_2.svy, pb020 == "AT"))

# Top 10% Share yearly
share <- vector("numeric", length(2005:2017))
j <- 1

for(year in 2005:2017) {
  svy_subset <- subset(silc.inc_2.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, income_wid_2 >= as.numeric(svyquantile(~income_wid_2, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~income_wid_2, svy_top_10) / svytotal(~income_wid_2, svy_subset)
  j <- j + 1
}

years_top10_p2_2 <- data.frame(top_p2_2 = share)

years_top10_p2_2

# 80/20 Ratio for whole period
svyqsr(~income_wid_2, silc.inc_2.svy, 0.2, 0.8)

# 80/20 Ratio yearly
p8020_p2_2 <- svyby(~income_wid_2, by=~rb010, design=silc.inc_2.svy, FUN=svyqsr)

# Gini for whole period
svygini(~income_wid_2, silc.inc_2.svy)

#Gini yearly
Gini_p2_2 <- svyby(~income_wid_2, by=~rb010, design=silc.inc_2.svy, FUN=svygini, c(0.5), ci=TRUE)

# Theil Index for whole period
svygei(~income_wid_2, silc.inc_2.svy, epsilon = 1)

# Theil Index yearly
Theil_p2_2 <- svyby(~income_wid_2, by=~rb010, design=silc.inc_2.svy, FUN=svygei, epsilon = 1)




#########-------------------------------------------------------------------------
#### Post-tax disposable Income: income_wid_3

# Mean for whole period
svymean(~income_wid_3, silc.inc_2.svy)

#Mean yearly
mean_p2_3 <- svyby(~income_wid_3, by=~rb010, design=silc.inc_2.svy, FUN=svymean)

# Medianfor whole period
svyquantile(~income_wid_3, silc.inc_2.svy, quantiles = c(0.5))

#Median yearly
median_p2_3 <- svyby(~income_wid_3, by=~rb010, design=silc.inc_2.svy, FUN=svyquantile, c(0.5), ci=TRUE)

# Top 10% Share 
svytotal(~income_wid_3, subset(silc.inc_2.svy, pb020 == "AT" & income_wid_3 >= 
                                 as.numeric(svyquantile(~income_wid_3, silc.inc_2.svy, quantile = 0.9)))) / 
  svytotal(~income_wid_3, subset(silc.inc_2.svy, pb020 == "AT"))

# Top 10% Share yearly
share <- vector("numeric", length(2005:2017))
j <- 1

for(year in 2005:2017) {
  svy_subset <- subset(silc.inc_2.svy, rb010 == year)
  svy_top_10 <- subset(svy_subset, income_wid_3 >= as.numeric(svyquantile(~income_wid_3, svy_subset, quantile = c(0.9))))
  share[j] <- svytotal(~income_wid_3, svy_top_10) / svytotal(~income_wid_3, svy_subset)
  j <- j + 1
}

years_top10_p2_3 <- data.frame(top_p2_3 = share)

years_top10_p2_3


# 80/20 Ratio for whole period
svyqsr(~income_wid_3, silc.inc_2.svy, 0.2, 0.8)

# 80/20 Ratio yearly
p8020_p2_3 <- svyby(~income_wid_3, by=~rb010, design=silc.inc_2.svy, FUN=svyqsr)

# Gini for whole period
svygini(~income_wid_3, silc.inc_2.svy)

#Gini yearly
Gini_p2_3 <- svyby(~income_wid_3, by=~rb010, design=silc.inc_2.svy, FUN=svygini, c(0.5), ci=TRUE)

# Theil Index for whole period
svygei(~income_wid_3, silc.inc_2.svy, epsilon = 1)

# Theil Index yearly
Theil_p2_3 <- svyby(~income_wid_3, by=~rb010, design=silc.inc_2.svy, FUN=svygei, epsilon = 1)


#----------Poverty rate-------------------------------------------------------------------

years <- (2005:2017)
dim(years) <- c(13, 1)
colnames(years) <- c("Jahr")
years <- as.data.frame(years)
years

silc.pov <- silc.rph %>% filter(rb010 > 2004)

arop1 <- laeken::arpr(silc.pov$posttax, weights=silc.pov$rb050, 
                      years=silc.pov$rb010)
arop1$value
arop1

# gender
# age >=18 

silc.pov18 <- silc.pov %>% filter(age>=18)

arop2 <- laeken::arpr(silc.pov$posttax, weights=silc.pov$rb050, breakdown = silc.pov$rb090, years=silc.pov$rb010)
arop2


povgender <- arop2$valueByStratum
povgender <- c(years$Jahr, povgender$value)
povgender
dim(povgender) <-  c(13,3)
colnames(povgender) <- c("Jahr", "Männer", "Frauen")
class(povgender)
povgender <- as.data.frame(povgender)
povgender

povtotal <- c(years$Jahr, arop2$value)
dim(povtotal) <-  c(13,2)
colnames(povtotal) <- c("Jahr", "At Risk of Poverty")
class(povtotal)
povtotal <- as.data.frame(povtotal)
povtotal

#age: <18, 17-65, >64

silc.povage <- silc.pov %>% mutate(agecl1=cut(age,c(0,17,65,120)))

arop3 <- arpr(silc.povage$posttax, weights=silc.povage$rb050, breakdown = silc.povage$agecl1, years=silc.povage$rb010)

arop3

povage <- arop3$valueByStratum
povage <- c(years$Jahr, povage$value)
povage
dim(povage) <-  c(13,4)
colnames(povage) <- c("Jahr", "Unter 18", "18-65", "Über 65")
class(povage)
povage <- as.data.frame(povage)
povage

table_pov <- dplyr::left_join(povtotal, povgender, by="Jahr")
table_pov <- dplyr::left_join(table_pov, povage, by="Jahr")
colnames(table_pov) <- c("Jahr", "Insgesamt", "Männer", "Frauen", "Unter 18", "18-65", "Über 65")
table_pov <- round(table_pov, 2)
table_pov

write.csv(table_pov, "./reports/AUT/tables/_tables_AUT_pov.csv")

write_rds(table_pov, "./reports/AUT/tables/_tables_AUT_pov.rds")

#------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
#
#Cretaing tables containig all indicators over the years
#
#------------------------------------------------------------------------------------------

# P1 Eurostat Income
# Pre-tax factor income (Canberra: primary income): Can_inc

table_p1_1 <- data.frame(mean_p1_1$rb010, mean_p1_1$Can_inc, median_p1_1$Can_inc, 
                         Gini_p1_1$Can_inc, p8020_p1_1$Can_inc, years_top10_p1_1$top_p1_1)

colnames(table_p1_1)<- c("Year", "Mean" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_1 

write.csv(table_p1_1, "./reports/AUT/tables/_tables_AUT_p1_1.csv")

write_rds(table_p1_1, "./reports/AUT/tables/_tables_AUT_p1_1.rds")

# Pre-tax national income: prenatincom

table_p1_2 <- data.frame(mean_p1_2$rb010, mean_p1_2$prenatincom, median_p1_2$prenatincom, 
                         Gini_p1_2$prenatincom, p8020_p1_2$prenatincom, 
                         years_top10_p1_2$top_p1_2)

colnames(table_p1_2)<- c("Year", "Mean" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_2 

write.csv(table_p1_2, "./reports/AUT/tables/_tables_AUT_p1_2.csv")

write_rds(table_p1_2, "./reports/AUT/tables/_tables_AUT_p1_2.rds")

# Post-tax disposable income: posttax

table_p1_3 <- data.frame(mean_p1_3$rb010, mean_p1_3$posttax, median_p1_3$posttax, 
                         Gini_p1_3$posttax, p8020_p1_3$posttax, years_top10_p1_3$top_p1_3)

colnames(table_p1_3)<- c("Year", "Mean" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_3 

write.csv(table_p1_3, "./reports/AUT/tables/_tables_AUT_p1_3.csv")

write_rds(table_p1_3, "./reports/AUT/tables/_tables_AUT_p1_3.rds")

#P2 wid.world

# Pre-tax factor income: Canberra income

table_p2_1 <- data.frame(mean_p2_1$rb010, mean_p2_1$income_wid_1, median_p2_1$income_wid_1, 
                         Gini_p2_1$income_wid_1, p8020_p2_1$income_wid_1, 
                         years_top10_p2_1$top_p2_1)

colnames(table_p2_1)<- c("Year", "Mean" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p2_1 

write.csv(table_p2_1, "./reports/AUT/tables/_tables_AUT_p2_1.csv")

write_rds(table_p2_1, "./reports/AUT/tables/_tables_AUT_p2_1.rds")

# Pre-tax income

table_p2_2 <- data.frame(mean_p2_2$rb010, mean_p2_2$income_wid_2, median_p2_2$income_wid_2, 
                         Gini_p2_2$income_wid_2, p8020_p2_2$income_wid_2, 
                         years_top10_p2_2$top_p2_2)

colnames(table_p2_2)<- c("Year", "Mean" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p2_2

write.csv(table_p2_2, "./reports/AUT/tables/_tables_AUT_p2_2.csv")

write_rds(table_p2_2, "./reports/AUT/tables/_tables_AUT_p2_2.rds")



# Post-tax (disposable) income

table_p2_3 <- data.frame(mean_p2_3$rb010, mean_p2_3$income_wid_3, median_p2_3$income_wid_3, 
                         Gini_p2_3$income_wid_3, p8020_p2_3$income_wid_3, 
                         years_top10_p2_3$top_p2_3)

colnames(table_p2_3)<- c("Year", "Mean" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p2_3

write.csv(table_p2_3, "./reports/AUT/tables/_tables_AUT_p2_3.csv")

write_rds(table_p2_3, "./reports/AUT/tables/_tables_AUT_p2_3.rds")



#------------------------------------------------------------------------------------------
#
#Cretaing plots for certain indicators over the years
#
#------------------------------------------------------------------------------------------

## Gini plot für P1
gini_p1 <- ggplot() +
  geom_line(mapping = aes(y = table_p1_1$Gini,x = table_p1_1$Year,
                          color = "Gini vor Steuern (Pre-tax factor income)"), size = 1 ) +
  geom_line(mapping = aes(y = table_p1_2$Gini,x = table_p1_2$Year,
                          color = "Gini vor Steuern (Pre-tax national income)"), size = 1) +
  geom_line(mapping = aes(y = table_p1_3$Gini,x = table_p1_3$Year,
                          color = "Gini nach Steuern (Post-tax disposable income)"), size = 1) +
  scale_color_manual(values = c('Gini vor Steuern (Pre-tax factor income)' = 'darkred',
                                'Gini vor Steuern (Pre-tax national income)' = 'darkblue', 
                                'Gini nach Steuern (Post-tax disposable income)' = 'darkgreen'))+
  labs(color = '', x = "Jahr", y = "Gini", 
       title = "Gini-Koeffizient des Einkommens der gesamten Bevölkerung") + 
  ylim(0, 0.6) +
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1))

gini_p1

ggsave(file='./reports/AUT/img/Gini1.svg',height=4,width=7)

### Gini plot für p2
gini_p2 <- ggplot() +
  geom_line(mapping = aes(y = table_p2_1$Gini,x = table_p2_1$Year,
                          color = "Gini vor Steuern (Pre-tax factor income)"), size = 1 ) +
  geom_line(mapping = aes(y = table_p2_2$Gini,x = table_p2_2$Year,
                          color = "Gini vor Steuern (Pre-tax national income)"), size = 1) +
  geom_line(mapping = aes(y = table_p2_3$Gini,x = table_p2_3$Year,
                          color = "Gini nach Steuern (Post-tax disposable income)"), size = 1) +
  scale_color_manual(values = c('Gini vor Steuern (Pre-tax factor income)' = 'darkred',
                                'Gini vor Steuern (Pre-tax national income)' = 'darkblue', 
                                'Gini nach Steuern (Post-tax disposable income)' = 'darkgreen'))+
  scale_x_discrete(limits=2005:2017) + 
  ylim(0, 0.6) +
  labs(color = '', x = "Jahr", y = "Gini", 
       title = "Gini-Koeffizient des Einkommens von Personen über 20 Jahren") +
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction="vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1))
        
gini_p2
ggsave(file='./reports/AUT/img/Gini2.svg',height=4,width=7)


###Top 10% Share (P1)

Top10_p1 <- ggplot() +
  geom_line(mapping = aes(y = table_p1_1$"Top10%", x = table_p1_1$Year,
                          color = "Top10% vor Steuern (Pre-tax factor income)"), size = 1 ) +
  geom_line(mapping = aes(y = table_p1_2$"Top10%", x = table_p1_2$Year,
                          color = "Top10% vor Steuern (Pre-tax national income)"), size = 1) +
  geom_line(mapping = aes(y = table_p1_3$"Top10%", x = table_p1_3$Year,
                          color = "Top10% nach Steuern (Post-tax disposable income)"), size = 1) +
  scale_color_manual(values = c('Top10% vor Steuern (Pre-tax factor income)' = 'darkred',
                                'Top10% vor Steuern (Pre-tax national income)' = 'darkblue', 
                                'Top10% nach Steuern (Post-tax disposable income)' = 'darkgreen'))+
  labs(color = '', x = "Jahr", y = "Top10%", 
       title = "Top10%  des Einkommens der gesamten Bevölkerung") + 
  scale_x_discrete(limits=2005:2017) + 
  ylim(0,0.5) +
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Top10_p1
ggsave(file='./reports/AUT/img/Top101.svg',height=4,width=7)

###Top 10% Share (P2)

Top10_p2 <- ggplot() +
  geom_line(mapping = aes(y = table_p2_1$"Top10%", x = table_p2_1$Year,
                          color = "Top10% vor Steuern (Pre-tax factor income)"), size = 1) +
  geom_line(mapping = aes(y = table_p2_2$"Top10%", x = table_p2_2$Year,
                          color = "Top10% vor Steuern (Pre-tax national income)"), size = 1) +
  geom_line(mapping = aes(y = table_p2_3$"Top10%", x = table_p2_3$Year,
                          color = "Top10% nach Steuern (Post-tax disposable income)"), size = 1) +
  scale_color_manual(values = c('Top10% vor Steuern (Pre-tax factor income)' = 'darkred',
                                'Top10% vor Steuern (Pre-tax national income)' = 'darkblue', 
                                'Top10% nach Steuern (Post-tax disposable income)' = 'darkgreen'))+
  labs(color = '', x = "Jahr", y = "Top10%", 
       title = "Top10%  des Einkommens der Bevölkerung über 20 Jahre") + 
  scale_x_discrete(limits=2005:2017) + 
  ylim(0,0.5) +
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Top10_p2

ggsave(file='./reports/AUT/img/Top102.svg',height=4,width=7)

###Mean (P1)

Mean_p1 <- ggplot() +
  geom_line(mapping = aes(y = table_p1_1$"Mean", x = table_p2_1$Year,
                          color = "Mittelwert vor Steuern (Pre-tax factor income)"), size = 1) +
  geom_line(mapping = aes(y = table_p1_2$"Mean", x = table_p2_2$Year,
                          color = "Mittelwert vor Steuern (Pre-tax national income)"), size = 1) +
  geom_line(mapping = aes(y = table_p1_3$"Mean", x = table_p2_3$Year,
                          color = "Mittelwert nach Steuern (Post-tax disposable income)"), size = 1) +
  scale_color_manual(values = c('Mittelwert vor Steuern (Pre-tax factor income)' = 'darkred',
                                'Mittelwert vor Steuern (Pre-tax national income)' = 'darkblue', 
                                'Mittelwert nach Steuern (Post-tax disposable income)' = 'darkgreen'))+
  labs(color = '', x = "Jahr", y = "Mittelwert", 
       title = "Mittelwert  der Einkommen der gesamten Bevölkerung") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Mean_p1

ggsave(file='./reports/AUT/img/Mean1.svg',height=4,width=7)

###Mean (P2)

Mean_p2 <- ggplot() +
  geom_line(mapping = aes(y = table_p2_1$"Mean", x = table_p2_1$Year,
                          color = "Mittelwert vor Steuern (Pre-tax factor income)"), size = 1) +
  geom_line(mapping = aes(y = table_p2_2$"Mean", x = table_p2_2$Year,
                          color = "Mittelwert vor Steuern (Pre-tax national income)"), size = 1) +
  geom_line(mapping = aes(y = table_p2_3$"Mean", x = table_p2_3$Year,
                          color = "Mittelwert nach Steuern (Post-tax disposable income)"), size = 1) +
  scale_color_manual(values = c('Mittelwert vor Steuern (Pre-tax factor income)' = 'darkred',
                                'Mittelwert vor Steuern (Pre-tax national income)' = 'darkblue', 
                                'Mittelwert nach Steuern (Post-tax disposable income)' = 'darkgreen'))+
  labs(color = '', x = "Jahr", y = "Mittelwert", 
       title = "Mittelwert  der Einkommen der Bevölkerung über 20 Jahre") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Mean_p2
ggsave(file='./reports/AUT/img/Mean2.svg',height=4,width=7)

###Median (P1)

Median_p1 <- ggplot() +
  geom_line(mapping = aes(y = table_p1_1$"Median", x = table_p2_1$Year,
                          color = "Median vor Steuern (Pre-tax factor income)"), size = 1) +
  geom_line(mapping = aes(y = table_p1_2$"Median", x = table_p2_2$Year,
                          color = "Median vor Steuern (Pre-tax national income)"), size = 1) +
  geom_line(mapping = aes(y = table_p1_3$"Median", x = table_p2_3$Year,
                          color = "Median nach Steuern (Post-tax disposable income)"), size = 1) +
  scale_color_manual(values = c('Median vor Steuern (Pre-tax factor income)' = 'darkred',
                                'Median vor Steuern (Pre-tax national income)' = 'darkblue', 
                                'Median nach Steuern (Post-tax disposable income)' = 'darkgreen'))+
  labs(color = '', x = "Jahr", y = "Median", 
       title = "Median  der Einkommen der gesamten Bevölkerung") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Median_p1

ggsave(file='./reports/AUT/img/Median1.svg',height=4,width=7)

###Mean (P2)

Median_p2 <- ggplot() +
  geom_line(mapping = aes(y = table_p2_1$"Median", x = table_p2_1$Year,
                          color = "Median vor Steuern (Pre-tax factor income)"), size = 1) +
  geom_line(mapping = aes(y = table_p2_2$"Median", x = table_p2_2$Year,
                          color = "Median vor Steuern (Pre-tax national income)"), size = 1) +
  geom_line(mapping = aes(y = table_p2_3$"Median", x = table_p2_3$Year,
                          color = "Median nach Steuern (Post-tax disposable income)"), size = 1) +
  scale_color_manual(values = c('Median vor Steuern (Pre-tax factor income)' = 'darkred',
                                'Median vor Steuern (Pre-tax national income)' = 'darkblue', 
                                'Median nach Steuern (Post-tax disposable income)' = 'darkgreen'))+
  labs(color = '', x = "Jahr", y = "Median", 
       title = "Median  der Einkommen der Bevölkerung über 20 Jahre") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Median_p2

ggsave(file='./reports/AUT/img/Median2.svg',height=4,width=7)

#######
#Mean vs Median nach Steuern (P1)#

MedianMean_p1 <- ggplot() +
  geom_line(mapping = aes(y = table_p1_3$"Mean", x = table_p1_3$Year,
                          color = "Mittelwert nach Steuern (Post-tax disposable income)"), size = 1) +
  geom_line(mapping = aes(y = table_p1_3$"Median", x = table_p1_3$Year,
                          color = "Median nach Steuern (Post-tax disposable income)"), size = 1) +
  scale_color_manual(values = c('Mittelwert nach Steuern (Post-tax disposable income)' = 'darkblue',
                                'Median nach Steuern (Post-tax disposable income)' = 'lightblue'))+
  labs(color = '', x = "Jahr", y = "Median/Mittelwert", 
       title = "Median und Mittelwert der Einkommen der gesamten Bevölkerung") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

MedianMean_p1

ggsave(file='./reports/AUT/img/MedianMean1.svg',height=4,width=7)


#Mean vs Median nach Steuern (P2)#

MedianMean_p2 <- ggplot() +
  geom_line(mapping = aes(y = table_p2_3$"Mean", x = table_p2_3$Year,
                          color = "Mittelwert nach Steuern (Post-tax disposable income)"), size = 1) +
  geom_line(mapping = aes(y = table_p2_3$"Median", x = table_p2_3$Year,
                          color = "Median nach Steuern (Post-tax disposable income)"), size = 1) +
  scale_color_manual(values = c('Mittelwert nach Steuern (Post-tax disposable income)' = 'darkblue',
                                'Median nach Steuern (Post-tax disposable income)' = 'lightblue'))+
  labs(color = '', x = "Jahr", y = "Median/Mittelwert", 
       title = "Median und Mittelwert der Einkommen der Bevölkerung über 20 Jahre") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

MedianMean_p2

ggsave(file='./reports/AUT/img/MedianMean2.svg',height=4,width=7)


#POV Age Plot
povage_plot <- ggplot() +
  geom_line(mapping = aes(y = povage$"Unter 18", x = povage$Jahr,
                          color = "Armutsgefährdungsquote der unter 18 Jährigen"), size = 1) +
  geom_line(mapping = aes(y = povage$"18-65", x = povage$Jahr,
                          color = "Armutsgefährdungsquote der 18-65 Jährigen"), size = 1) +
  geom_line(mapping = aes(y = povage$"Über 65", x = povage$Jahr,
                          color = "Armutsgefährdungsquote der über 65 Jährigen"), size = 1) +
  geom_point(mapping = aes(y = povage$"Unter 18", x = povage$Jahr,
                           color = "Armutsgefährdungsquote der unter 18 Jährigen"), size = 2) +
  geom_point(mapping = aes(y = povage$"18-65", x = povage$Jahr,
                           color = "Armutsgefährdungsquote der 18-65 Jährigen"), size = 2) +
  geom_point(mapping = aes(y  = povage$"Über 65", x = povage$Jahr,
                           color = "Armutsgefährdungsquote der über 65 Jährigen"), size = 2) +
  scale_color_manual(values = c('Armutsgefährdungsquote der unter 18 Jährigen' = 'darkred',
                                'Armutsgefährdungsquote der 18-65 Jährigen' = 'darkblue',
                                'Armutsgefährdungsquote der über 65 Jährigen' = 'darkgreen')) +
  labs(color = '', x = "Jahr", y = "Armutsgefährdungsquote", 
       title = "Armutsgefährdungsquoten in Österreich nach Altersgruppen") + 
  scale_x_discrete(limits=2005:2017) +
  ylim(10,20) +
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position="bottom", legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

povage_plot



#POV Gender Plot
povgender_plot <- ggplot() +
  geom_line(mapping = aes(y = povtotal$"At Risk of Poverty", x = povtotal$Jahr,
                          color = "Armutsgefährdungsquote Gesamt"), size = 1) +
  geom_line(mapping = aes(y = povgender$"Frauen", x = povgender$Jahr,
                          color = "Armutsgefährdungsqoute Frauen"), size = 1) +
  geom_line(mapping = aes(y = povgender$"Männer", x = povgender$Jahr,
                          color = "Armutsgefährdungsquote Männer"), size = 1) +
  geom_point(mapping = aes(y =povtotal$"At Risk of Poverty", x = povtotal$Jahr, 
                           color = "Armutsgefährdungsquote Gesamt"), size =  2) +
  geom_point(mapping = aes(y = povgender$"Frauen", x = povgender$Jahr,
                           color = "Armutsgefährdungsqoute Frauen"), size = 2) +
  geom_point(mapping = aes(y = povgender$"Männer", x = povgender$Jahr,
                           color = "Armutsgefährdungsquote Männer"), size = 2) +
  scale_color_manual(values = c('Armutsgefährdungsquote Gesamt' = 'darkred',
                                'Armutsgefährdungsqoute Frauen' = 'darkblue', 
                                'Armutsgefährdungsquote Männer' = 'darkgreen'))+
  labs(color = '', x = "Jahr", y = "Armutsgefährdungsquote", 
       title = "Armutsgefährdungsquoten in Österreich") + 
  scale_x_discrete(limits=2005:2017) +
  ylim(10,20) +
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

povgender_plot




###### Le Fin ############################################################################# 

