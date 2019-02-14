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

# Source connection 
# source("R/_connection.R")

# Source Setup scripts to provide data
source("reports/AUT/Code_AUT/_setup_AUT.R")


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
top10_p1_1 <- subset(silc.inc_1.svy, Can_inc >= as.numeric(
  svyquantile(~Can_inc, silc.inc_1.svy, quantile=c(0.9))))

top10num_p1_1 <- svyby(~Can_inc, ~rb010, top10_p1_1, svytotal)

top10den_p1_1 <- svyby(~Can_inc, ~rb010, silc.inc_1.svy, svytotal)

years_top10_p1_1 <- top10num_p1_1 / top10den_p1_1

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
top10_p1_2 <- subset(silc.inc_1.svy, prenatincom >= as.numeric(
  svyquantile(~prenatincom, silc.inc_1.svy, quantile=c(0.9))))

top10num_p1_2 <- svyby(~prenatincom, ~rb010, top10_p1_2, svytotal)

top10den_p1_2 <- svyby(~prenatincom, ~rb010, silc.inc_1.svy, svytotal)

years_top10_p1_2 <- top10num_p1_2 / top10den_p1_2

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
top10_p1_3 <- subset(silc.inc_1.svy, posttax >= as.numeric(
  svyquantile(~posttax, silc.inc_1.svy, quantile=c(0.9))))

top10num_p1_3 <- svyby(~posttax, ~rb010, top10_p1_3, svytotal)

top10den_p1_3 <- svyby(~posttax, ~rb010, silc.inc_1.svy, svytotal)

years_top10_p1_3 <- top10num_p1_3 / top10den_p1_3

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
top10_p2_1 <- subset(silc.inc_2.svy, income_wid_1 >= as.numeric(
  svyquantile(~income_wid_1, silc.inc_2.svy, quantile=c(0.9))))

top10num_p2_1 <- svyby(~income_wid_1, ~rb010, top10_p2_1, svytotal)

top10den_p2_1 <- svyby(~income_wid_1, ~rb010, silc.inc_2.svy, svytotal)

years_top10_p2_1 <- top10num_p2_1 / top10den_p2_1

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
top10_p2_2 <- subset(silc.inc_2.svy, income_wid_1 >= as.numeric(
  svyquantile(~income_wid_2, silc.inc_2.svy, quantile=c(0.9))))

top10num_p2_2 <- svyby(~income_wid_2, ~rb010, top10_p2_2, svytotal)

top10den_p2_2 <- svyby(~income_wid_2, ~rb010, silc.inc_2.svy, svytotal)

years_top10_p2_2 <- top10num_p2_2 / top10den_p2_2

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
top10_p2_3 <- subset(silc.inc_2.svy, income_wid_3 >= as.numeric(
  svyquantile(~income_wid_3, silc.inc_2.svy, quantile=c(0.9))))

top10num_p2_3 <- svyby(~income_wid_3, ~rb010, top10_p2_3, svytotal)

top10den_p2_3 <- svyby(~income_wid_3, ~rb010, silc.inc_2.svy, svytotal)

years_top10_p2_3 <- top10num_p2_3 / top10den_p2_3

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




#------------------------------------------------------------------------------------------
#
#including Top 10 shares as calculated in file indicators_base_AUT_top10test.R
#
#------------------------------------------------------------------------------------------


#Can_inc

testtop10Can_inc <- c(0.31382, 0.31524, 0.32488, 0.32979, 0.33647, 0.34559, 0.33004, 0.33174, 0.33372, 0.33882, 0.33943, 0.33948, 0.34348)

dim(testtop10Can_inc) <- c(13,1)
colnames(testtop10Can_inc) <- c("Can_inc")
class(testtop10Can_inc)
testtop10Can_inc <- as.data.frame(testtop10Can_inc)

testtop10Can_inc

#prenatincom


testtop10prenatincom <- c(0.26932, 0.25895, 0.26934, 0.27479, 0.27911, 0.28489, 0.27504, 0.27982, 0.27993, 0.28257, 0.27824, 0.27948, 0.28391)

dim(testtop10prenatincom) <- c(13,1)
colnames(testtop10prenatincom) <- c("prenatincom")
class(testtop10prenatincom)
testtop10prenatincom <- as.data.frame(testtop10prenatincom)

testtop10prenatincom

#posttax

testtop10posttax <- c(0.23741, 0.22731, 0.23249, 0.23776, 0.23726, 0.24316, 0.23451, 0.23646, 0.23388, 0.24363, 0.23903, 0.23585, 0.23703)

dim(testtop10posttax) <- c(13,1)
colnames(testtop10posttax) <- c("posttax")
class(testtop10posttax)
testtop10posttax <- as.data.frame(testtop10posttax)

testtop10posttax

#########################P2 Berechnung
###############Top10

#income_wid_1

testtop10income_wid_1 <- c(0.31388, 0.30532, 0.31369, 0.32371, 0.32293, 0.33445, 0.32136, 0.3269, 0.32672, 0.32594, 0.31927, 0.31925, 0.32892)

dim(testtop10income_wid_1) <- c(13,1)
colnames(testtop10income_wid_1) <- c("income_wid_1")
class(testtop10income_wid_1)
testtop10income_wid_1 <- as.data.frame(testtop10income_wid_1)

testtop10income_wid_1

#income_wid_2

testtop10income_wid_2 <- c(0.28684, 0.27328, 0.28238, 0.29415, 0.29307, 0.30238, 0.29064, 0.29593, 0.29449, 0.2936, 0.28739, 0.28821, 0.29676)

dim(testtop10income_wid_2) <- c(13,1)
colnames(testtop10income_wid_2) <- c("income_wid_2")
class(testtop10income_wid_2)
testtop10income_wid_2 <- as.data.frame(testtop10income_wid_2)

testtop10income_wid_2

#income_wid_3
testtop10income_wid_3 <- c(0.28679, 0.27351, 0.27826, 0.29178, 0.2894, 0.29907, 0.28433, 0.28895, 0.28979, 0.28985, 0.28142, 0.28283, 0.28844)

dim(testtop10income_wid_3) <- c(13,1)
colnames(testtop10income_wid_3) <- c("income_wid_3")
class(testtop10income_wid_3)
testtop10income_wid_3 <- as.data.frame(testtop10income_wid_3)

testtop10income_wid_3

#------------------------------------------------------------------------------------------
#
#Cretaing tables containig all indicators over the years
#
#------------------------------------------------------------------------------------------

# P1 Eurostat Income
# Pre-tax factor income (Canberra: primary income): Can_inc

table_p1_1 <- data.frame(mean_p1_1$rb010, mean_p1_1$Can_inc, median_p1_1$Can_inc, 
                         Gini_p1_1$Can_inc, p8020_p1_1$Can_inc, testtop10Can_inc$Can_inc)

colnames(table_p1_1)<- c("Year", "Mean" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_1 

write.csv(table_p1_1, "./reports/AUT/tables/_tables_AUT_p1_1.csv")

write_rds(table_p1_1, "./reports/AUT/tables/_tables_AUT_p1_1.rds")

# Pre-tax national income: prenatincom

table_p1_2 <- data.frame(mean_p1_2$rb010, mean_p1_2$prenatincom, median_p1_2$prenatincom, 
                         Gini_p1_2$prenatincom, p8020_p1_2$prenatincom, 
                         testtop10prenatincom$prenatincom)

colnames(table_p1_2)<- c("Year", "Mean" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_2 

write.csv(table_p1_2, "./reports/AUT/tables/_tables_AUT_p1_2.csv")

write_rds(table_p1_2, "./reports/AUT/tables/_tables_AUT_p1_2.rds")

# Post-tax disposable income: posttax

table_p1_3 <- data.frame(mean_p1_3$rb010, mean_p1_3$posttax, median_p1_3$posttax, 
                         Gini_p1_3$posttax, p8020_p1_3$posttax, testtop10posttax$posttax)

colnames(table_p1_3)<- c("Year", "Mean" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p1_3 

write.csv(table_p1_3, "./reports/AUT/tables/_tables_AUT_p1_3.csv")

write_rds(table_p1_3, "./reports/AUT/tables/_tables_AUT_p1_3.rds")

#P2 wid.world

# Pre-tax factor income: Canberra income

table_p2_1 <- data.frame(mean_p2_1$rb010, mean_p2_1$income_wid_1, median_p2_1$income_wid_1, 
                         Gini_p2_1$income_wid_1, p8020_p2_1$income_wid_1, 
                         testtop10income_wid_1$income_wid_1)

colnames(table_p2_1)<- c("Year", "Mean" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p2_1 

write.csv(table_p2_1, "./reports/AUT/tables/_tables_AUT_p2_1.csv")

write_rds(table_p2_1, "./reports/AUT/tables/_tables_AUT_p2_1.rds")

# Pre-tax income

table_p2_2 <- data.frame(mean_p2_2$rb010, mean_p2_2$income_wid_2, median_p2_2$income_wid_2, 
                         Gini_p2_2$income_wid_2, p8020_p2_2$income_wid_2, 
                        testtop10income_wid_2$income_wid_2)

colnames(table_p2_2)<- c("Year", "Mean" ,"Median", "Gini", "P80/P20", 
                         "Top10%")

table_p2_2

write.csv(table_p2_2, "./reports/AUT/tables/_tables_AUT_p2_2.csv")

write_rds(table_p2_2, "./reports/AUT/tables/_tables_AUT_p2_2.rds")



# Post-tax (disposable) income

table_p2_3 <- data.frame(mean_p2_3$rb010, mean_p2_3$income_wid_3, median_p2_3$income_wid_3, 
                         Gini_p2_3$income_wid_3, p8020_p2_3$income_wid_3, 
                         testtop10income_wid_3$income_wid_3)

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
       title = "Gini-Koeffizient des Einkommens der \n gesamten Bevölkerung") + 
  ylim(0, 0.6) +
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1))

gini_p1


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
       title = "Gini-Koeffizient des Einkommens von \n Personen über 20 Jahren") +
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction="vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1))
        
gini_p2



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
       title = "Top10%  des Einkommens der \n gesamten Bevölkerung") + 
  scale_x_discrete(limits=2005:2017) + 
  ylim(0,0.5) +
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Top10_p1


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
       title = "Top10%  des Einkommens der \n Bevölkerung über 20 Jahre") + 
  scale_x_discrete(limits=2005:2017) + 
  ylim(0,0.5) +
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Top10_p2

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
       title = "Mittelwert  der Einkommen der \n gesamten Bevölkerung") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Mean_p1

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
       title = "Mittelwert  der Einkommen der \n Bevölkerung über 20 Jahre") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Mean_p2


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
       title = "Median  der Einkommen der \n gesamten Bevölkerung") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Median_p1

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
       title = "Median  der Einkommen der \n Bevölkerung über 20 Jahre") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

Median_p2

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
       title = "Median und Mittelwert der Einkommen der \n gesamten Bevölkerung") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

MedianMean_p1
#Mean vs Median nach Steuern (P2)#

MedianMean_p2 <- ggplot() +
  geom_line(mapping = aes(y = table_p2_3$"Mean", x = table_p2_3$Year,
                          color = "Mittelwert nach Steuern (Post-tax disposable income)"), size = 1) +
  geom_line(mapping = aes(y = table_p2_3$"Median", x = table_p2_3$Year,
                          color = "Median nach Steuern (Post-tax disposable income)"), size = 1) +
  scale_color_manual(values = c('Mittelwert nach Steuern (Post-tax disposable income)' = 'darkblue',
                                'Median nach Steuern (Post-tax disposable income)' = 'lightblue'))+
  labs(color = '', x = "Jahr", y = "Median/Mittelwert", 
       title = "Median und Mittelwert der Einkommen der \n Bevölkerung über 20 Jahre") + 
  scale_x_discrete(limits=2005:2017) + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE)) +
  theme_light()+ 
  theme(legend.position=c(0.7, 0.2), legend.direction = "vertical", 
        panel.grid.major = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1)) 

MedianMean_p2



####alternative line for the legend position theme(legend.position=c(0.25, 0.5), legend.direction = "vertical", 

