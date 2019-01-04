# ------------------------------------------------------------------------
#
# Indicators AUT
#
# -------------------------------------------------------------------------



library(dplyr)
library(survey)
library(convey)


# Source connection 
source("R/_connection.R")

# Source Setup scripts to provide data
source("reports/AUT/Code_AUT/_setup_AUT.R")


# Subsetting --------------------------------------------------------------

#### To get useful results we may want to subset to only positive 
#### income and people over 20

#### Continue here
# Positive income
silc.inc_1 <- silc.rph %>% filter(Can_inc > 0, prenatincom > 0, 
                                   posttax > 0)
# Positive income > 20
silc.inc_2 <- silc.rph %>% filter(Can_inc > 0, prenatincom > 0, 
                                   posttax > 0, age > 20)

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
median_p1_1 <- svyby(~Can_inc, by=~rb010, design=silc.inc_1.svy, FUN=svyquantile, c(0.5), ci=TRUE)


# Top 10% Share 
svytotal(~Can_inc, subset(silc.inc_1.svy, pb020 == "AT" & Can_inc >= 
                              as.numeric(svyquantile(~Can_inc, silc.inc_1.svy, quantile = 0.9)))) / 
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
Gini_p1_1 <- svyby(~Can_inc, by=~rb010, design=silc.inc_1.svy, FUN=svygini, c(0.5), ci=TRUE)

# Theil Index for whole period
svygei(~Can_inc, silc.inc_1.svy, epsilon = 1)

# Theil Index yearly
Theil_p1_1 <- svyby(~Can_inc, by=~rb010, design=silc.inc_1.svy, FUN=svygei, epsilon = 1)


#########-------------------------------------------------------------------------
#########
#### Pre-tax national income:prenatincom

svymean(~prenatincom, silc.inc_1.svy)

#Mean yearly
mean_p1_2 <- svyby(~prenatincom, by=~rb010, design=silc.inc_1.svy, FUN=svymean)

# Medianfor whole period
svyquantile(~prenatincom, silc.inc_1.svy, quantiles = c(0.5))

#Median yearly
median_p1_2 <- svyby(~prenatincom, by=~rb010, design=silc.inc_1.svy, FUN=svyquantile, c(0.5), ci=TRUE)

# Top 10% Share 
svytotal(~prenatincom, subset(silc.inc_1.svy, pb020 == "AT" & prenatincom >= 
                                as.numeric(svyquantile(~prenatincom, silc.inc_1.svy, quantile = 0.9)))) / 
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
Gini_p1_2 <- svyby(~prenatincom, by=~rb010, design=silc.inc_1.svy, FUN=svygini, c(0.5), ci=TRUE)

# Theil Index for whole period
svygei(~prenatincom, silc.inc_1.svy, epsilon = 1)

# Theil Index yearly
Theil_p1_2 <- svyby(~prenatincom, by=~rb010, design=silc.inc_1.svy, FUN=svygei, epsilon = 1)



#########-------------------------------------------------------------------------
#### Post-tax disposable Income: posttax

svymean(~posttax, silc.inc_1.svy)

#Mean yearly
mean_p1_3 <- svyby(~posttax, by=~rb010, design=silc.inc_1.svy, FUN=svymean)

# Medianfor whole period
svyquantile(~posttax, silc.inc_1.svy, quantiles = c(0.5))

#Median yearly
median_p1_3 <- svyby(~posttax, by=~rb010, design=silc.inc_1.svy, FUN=svyquantile, c(0.5), ci=TRUE)


# Top 10% Share 
svytotal(~posttax, subset(silc.inc_1.svy, pb020 == "AT" & posttax >= 
                            as.numeric(svyquantile(~posttax, silc.inc_1.svy, quantile = 0.9)))) / 
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
Gini_p1_3 <- svyby(~posttax, by=~rb010, design=silc.inc_1.svy, FUN=svygini, c(0.5), ci=TRUE)

# Theil Index for whole period
svygei(~posttax, silc.inc_1.svy, epsilon = 1)

# Theil Index yearly
Theil_p1_3 <- svyby(~posttax, by=~rb010, design=silc.inc_1.svy, FUN=svygei, epsilon = 1)



########################################################################################
#########################################################################################
#########################################################################################

#########-------------------------------------------------------------------------
#########
#######P2 (Wid. World)


#########-------------------------------------------------------------------------
###### For each variable calculate indicators (Mean, Median, Gini, Top10%, 80/20)

#### Pre-tax factor income (Canberra Income):income_wid_1

# Mean for whole period
svymean(~income_wid_1, silc.inc_2.svy)

#Mean yearly
mean_p2_1 <- svyby(~income_wid_1, by=~rb010, design=silc.inc_2.svy, FUN=svymean)

# Medianfor whole period
svyquantile(~income_wid_1, silc.inc_2.svy, quantiles = c(0.5))

#Median yearly
median_p2_1 <- svyby(~income_wid_1, by=~rb010, design=silc.inc_1.svy, FUN=svyquantile, c(0.5), ci=TRUE)


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
median_p2_2 <- svyby(~income_wid_2, by=~rb010, design=silc.inc_1.svy, FUN=svyquantile, c(0.5), ci=TRUE)


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
median_p2_3 <- svyby(~income_wid_3, by=~rb010, design=silc.inc_1.svy, FUN=svyquantile, c(0.5), ci=TRUE)

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

















######################################################################################
###############################Technic-Team foundation################################
######################################################################################


# Indicators --------------------------------------------------------------

# Mean Income
#
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svymean)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy, svymean)

# Median Income
#
svyquantile(~total.inc, silc.pd.svy, quantiles = c(0.5))
svyquantile(~hy010, silc.hd.svy, quantiles = c(0.5))

# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy,
#       svyquantile, ~total.inc, quantiles = c(0.5), keep.var = FALSE)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy,
#       svyquantile, ~hy010, quantiles = c(0.5), keep.var = FALSE)

# Decile Points
#
svyquantile(~total.inc, silc.pd.svy, quantiles = seq(0, 1, 0.1))
svyquantile(~hy010, silc.hd.svy, quantiles = seq(0, 1, 0.1))
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, 
#       svyquantile, ~total.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)
# svyby(~hy010, ~as.factor(hb020), silc.pd.svy, 
#       svyquantile, ~total.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)

# Quantile Share Ratio
#
svyqsr(~total.inc, silc.pd.svy, 0.2, 0.8)
svyqsr(~hy010, silc.hd.svy, 0.2, 0.8)
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svyqsr, 0.2, 0.8)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy, svyqsr, 0.2, 0.8)

# Top 10% Income Share
#
svytotal(~total.inc, subset(silc.pd.svy, pb020 == country & total.inc >= 
                           as.numeric(svyquantile(~total.inc, silc.pd.svy, quantile = 0.9)))) / 
  svytotal(~total.inc, subset(silc.pd.svy, pb020 == country))
svytotal(~hy010, subset(silc.hd.svy, db020 == country & hy010 >= 
                          as.numeric(svyquantile(~hy010, silc.hd.svy, quantile = 0.9)))) /
  svytotal(~hy010,subset(silc.hd.svy, db020 == country))

# Gini Coefficient
#
svygini(~total.inc, silc.pd.svy)
svygini(~hy010, silc.hd.svy)
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svygini)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy, svygini)

# Theil Index
#
svygei(~total.inc, silc.pd.svy, epsilon = 1)
svygei(~hy010, silc.hd.svy, epsilon = 1)
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy,
#      svygei, epsilon = 1)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy,
#      svygei, epsilon = 1)
