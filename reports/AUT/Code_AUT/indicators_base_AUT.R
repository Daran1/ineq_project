# ------------------------------------------------------------------------
#
# Indicators AUT
#
# -------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)

#country <- "AT"
#year <- 2005-2017

# Source the Setup scripts to provide merged household and personal data
source("R/_connection.R")
source("reports/AUT/Code_AUT/_setup_AUT.R")


# Subsetting --------------------------------------------------------------

#### To get useful results we may want to subset to only positive 
#### income and people over 20

#### Continue here
# Positive income
silc.inc_1 <- silc.rph %>% filter(Can_inc > 0, prenatincom > 0, 
                                   posttax > 0 )
# Positive income > 20
silc.inc_2 <- silc.rph %>% filter(Can_inc > 0, prenatincom > 0, 
                                   posttax > 0, age > 20)

# Creating Survey Objects -------------------------------------------------
# Positive income
silc.inc_1.svy <- svydesign(ids =  ~ id_h,
                         strata = ~rb020,
                         weights = ~rb050,
                         data = silc.inc_1) %>% convey_prep()

# Positive income > 20
silc.inc_2.svy <- svydesign(ids = ~id_h,
                         strata = ~rb020,
                         weights = ~rb050,
                         data = silc.inc_2) %>% convey_prep()


###### For each variable calculate indicators (Mean, Median, Gini, Top10%, 80/20)

#### Pre-tax factor income (Canberra Income):Can_inc
# Mean 
# Median
# Gini
# Top 10% Share
# 80/20 Ratio

#### Pre-tax national income:prenatincom
# Mean 
# Median
# Gini
# Top 10% Share
# 80/20 Ratio

#### Post-tax disposable Income: posttax
# Mean 
# Median
# Gini
# Top 10% Share
# 80/20 Ratio

# P2 (Wid. World)

# Indicators --------------------------------------------------------------

# Mean Income
#
svymean(~Can_inc, silc.inc_1.svy)
svymean(~hy010, silc.hd.svy)
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
