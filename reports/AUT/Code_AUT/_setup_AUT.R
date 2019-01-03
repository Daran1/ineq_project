# Setup data for Austria -----------------------------------------------------------------

library(dplyr)
if(!exists(c("country", "year"))) {
  stop("Please specify country and year.")
}

source('./R/_connection.R')

# Download Data using required variables -------------------------------------------------
## For the years 2005-2013 because of the variable for non cash income
##Seperate dowload for this timeframe

#Personal Data  
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 == "AT" & pb010 %in% c(2005:2013)) %>%
  select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, 
         py080g, py090g, py100g, py110g, py120g, px010,px030, py130g, py140g) %>%
  collect(n = Inf)

#Household data
silc.h <- tbl(pg, "hh") %>%
  filter(hb020 == "AT" & hb010 %in% c(2005:2013)) %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, hy050g, hy060g, hy070g, 
         hy080g, hy090g, hy110g, hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

#Household register
silc.d <- tbl(pg, "dd") %>%
  filter(db020 == "AT" & db010 %in% c(2005:2013)) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

#Personal register
silc.r <- tbl(pg, "rr") %>% 
  filter(rb020 == "AT" & rb010 %in% c(2005:2013)) %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx010, rx030) %>%
  collect(n = Inf)

#### Include variable for cars and other non cash benefits py021g for entire timeframe
### @Lasserro script #Creditsgiven!

# Download c[YY]p tables from 2007 - 2013
c07p <- tbl(pg, "c07p") %>% filter(pb020 %in% "AT") %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c08p <- tbl(pg, "c08p") %>% filter(pb020 %in% "AT") %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c09p <- tbl(pg, "c09p") %>% filter(pb020 %in% "AT") %>% 
  select( pb010, pb030,py021g) %>% collect(n = Inf)

c10p <- tbl(pg, "c10p") %>% filter(pb020 %in% "AT") %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c11p <- tbl(pg, "c11p") %>% filter(pb020 %in% "AT") %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c12p <- tbl(pg, "c12p") %>% filter(pb020 %in% "AT") %>% 
  select(pb010, pb030, py021g) %>% collect(n = Inf)

c13p <- tbl(pg, "c13p") %>% filter(pb020 %in% "AT") %>% 
  select(pb010, pb030,py021g) %>% collect(n = Inf)

cxxp <- bind_rows(c07p, c08p, c09p, c10p, c11p, c12p, c13p)
rm(c07p, c08p, c09p, c10p, c11p, c12p, c13p)
# Merge cxxp with silc.p to include the py021g variable for 2007-2013

silc.p <- left_join(silc.p, cxxp %>% select(py021g, pb010, pb030))

rm(cxxp)
 
##### DOWNLOAD remaining DATA 2014-2017!!!!
##### Personal Data (p)

c14p <- tbl(pg, "c14p") %>% filter(pb020 == 'AT') %>% 
  select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, 
         py080g, py090g, py100g, py110g, py120g, py130g, py140g, px010, 
         px030, py021g) %>% collect(n = Inf)

c15p <- tbl(pg, "c15p") %>% filter(pb020 == 'AT') %>% 
  select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, 
         py080g, py090g, py100g, py110g, py120g, py130g, py140g, px010, 
         px030, py021g) %>% collect(n = Inf)

c16p <- tbl(pg, "c16p") %>% filter(pb020 == 'AT') %>% 
  select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, 
         py080g, py090g, py100g, py110g, py120g, py130g, py140g, px010, 
         px030, py021g) %>% collect(n = Inf)

c17p <- tbl(pg, "c17p") %>% filter(pb020 == 'AT') %>% 
  select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, 
         py080g, py090g, py100g, py110g, py120g, py130g, py140g, px010, 
         px030, py021g) %>% collect(n = Inf)

cxxp <- bind_rows(c14p, c15p, c16p, c17p)

#include the data in silc.p
silc.p <- bind_rows(silc.p, cxxp) 

#removing unnecessary variables 
rm(cxxp)
rm(c14p, c15p, c16p, c17p)

####Same procedure for Household data (h)
c14h <- tbl(pg, "c14h") %>%
  filter(hb020 == 'AT') %>%
  select(hb010, hb020, hb030, hy010, hy020, hy040g, hy050g, hy060g, hy070g, hy080g, 
         hy090g, hy110g, hy120g, hy130g, hy140g, hx010, hx050) %>%
  collect(n = Inf)

c15h <- tbl(pg, "c15h") %>%
  filter(hb020 == 'AT') %>%
  select(hb010, hb020, hb030, hy010, hy020, hy040g, hy050g, hy060g, hy070g, hy080g, 
         hy090g, hy110g, hy120g, hy130g, hy140g, hx010, hx050) %>%
  collect(n = Inf)

c16h <- tbl(pg, "c16h") %>%
  filter(hb020 == 'AT') %>%
  select(hb010, hb020, hb030, hy010, hy020, hy040g, hy050g, hy060g, hy070g, hy080g, 
         hy090g, hy110g, hy120g, hy130g, hy140g, hx010, hx050) %>%
  collect(n = Inf)

c17h <- tbl(pg, "c17h") %>%
  filter(hb020 == 'AT') %>%
  select(hb010, hb020, hb030, hy010, hy020, hy040g, hy050g, hy060g, hy070g, hy080g, 
         hy090g, hy110g, hy120g, hy130g, hy140g, hx010, hx050) %>%
  collect(n = Inf)


cxxh <- bind_rows(c14h, c15h, c16h, c17h)

#include the data in silc.p
silc.h <- bind_rows(silc.h, cxxh) 

#removing variables
rm(c14h, c15h, c16h, c17h)
rm(cxxh)

####Same procedure for Household register (d)
c14d <- tbl(pg, "c14d") %>%
  filter(db020 == 'AT') %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c15d <- tbl(pg, "c15d") %>%
  filter(db020 == 'AT') %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c16d <- tbl(pg, "c16d") %>%
  filter(db020 == 'AT') %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c17d <- tbl(pg, "c16d") %>%
  filter(db020 == 'AT') %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

cxxd <- bind_rows(c14d, c15d, c16d, c17d)

#merging with silc.d
silc.d <- bind_rows(silc.d, cxxd)

rm(c14d, c15d, c16d, c17d)
rm(cxxd)


# for personal register (r)
c14r <- tbl(pg, "c14r") %>% 
  filter(rb020 == 'AT') %>%
  select(rb010, rb020, rb030, rb050, rb090, rx010, rx030) %>%
  collect(n = Inf)

c15r <- tbl(pg, "c15r") %>% 
  filter(rb020 == 'AT') %>%
  select(rb010, rb020, rb030, rb050, rb090, rx010, rx030) %>%
  collect(n = Inf)

c16r <- tbl(pg, "c16r") %>% 
  filter(rb020 == 'AT') %>%
  select(rb010, rb020, rb030, rb050, rb090, rx010, rx030) %>%
  collect(n = Inf)

c17r <- tbl(pg, "c17r") %>% 
  filter(rb020 == 'AT') %>%
  select(rb010, rb020, rb030, rb050, rb090, rx010, rx030) %>%
  collect(n = Inf)

cxxr <- bind_rows(c14r, c15r, c16r, c17r)

# merge data to silc.r
silc.r <- bind_rows(silc.r, cxxr)

rm(c14r, c15r, c16r, c17r)
rm(cxxr)



#Making Data look better (hopefully) 

#renaming variables in the dataset for clarity

#silc.r <- silc.r %>% rename(id = rb030)

#silc.p <- silc.p %>% rename(id = pb030)


#Putting the datasets together creating one data set containing personal data and register
#
#Better to merge beforehand using unique IDs for merging ?
# Create unique IDs for merging
silc.p <- silc.p %>% mutate(id_h = paste0(pb020, px030))

silc.h <- silc.h %>% mutate(id_h = paste0(hb020, hb030))

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030))

silc.r <- silc.r %>% mutate(id_h = paste0(rb020, rb030))

#Merging datasets
silc.rp <- left_join(silc.r, silc.p)
silc.pd <- left_join(silc.p, silc.d)

###Create new variable combining py020g and py021g:car 

time1 <- seq(2004,2006,1)
time2 <- seq(2007,2017,1)
set1 <- silc.pd %>% filter(pb010 %in% time1)
set2 <- silc.pd %>% filter(pb010 %in% time2)
set1$car <- set1$py020g
set2$car <- set2$py021g

silc.pd <- bind_rows(set1,set2)

set3 <- silc.rp %>% filter(rb010 %in% time1)
set4 <- silc.rp %>% filter(rb010 %in% time2)
set3$car <- set3$py020g
set4$car <- set4$py020g

silc.rp <- bind_rows(set3,set4)

rm(time1,time2,set1,set2, set3, set4)


#Create new variables (age, gender and household ID)

silc.rp <- silc.rp %>% 
  mutate(age = rb010 - rb080,
         gender = factor(rb090, labels = c('Male','Female')),
         id_h = paste0(rb020, rx030)) 


#Merge datasets using unique IDs

silc.rph <- left_join(silc.rp, silc.h)

#silc.rphd <- left_join(silc.rph, silc.d)
#cannot allocate vector of size 220.9Mb (Error)

#Replace not answered NA
silc.rph[is.na(silc.rph)] <- 0

#### Create different income versions

############
#Income 1: Pre-tax factor income (Canberra: primary income)
#personal:pers_inc
silc.rph <- silc.rph %>% mutate(pers_inc = py010 + py050g + py080g + car)

#household:house_inc
silc.rph <- silc.rph%>% mutate(house_inc = hy040g, hy090g, hy110g)

#sum pers_inc
silc.rph <- silc.rph %>% group_by(id_h, rb010) %>%
  mutate(sum_pers_inc = sum(pers_inc))

#Canberra pre tax factor income: Combining
silc.rph <- silc.rph %>% mutate(Can_inc = (sum_pers_inc + house_inc) /hx050)


##########
#Income 2: Pre-tax national income
#Pensions and benefits: pensben
silc.rph <- silc.rph %>% mutate(pensben = py090 + py100g)
silc.rph <- silc.rph %>% group_by(id_h) %>% mutate(sum_pensben = sum(pensben))

#national:
silc.rph <- silc.rph %>% mutate(prenatincom = (Can_inc + sum_pensben/hx050))

#########
#Income 3: Post-tax disposable income
#All transfers
silc.rph <- silc.rph %>% mutate(perstransf = 
                                  py110g + py120g + py130g + py140g)
silc.rph <- silc.rph %>% mutate(houstransf = 
                                  hy050g + hy060g + hy070g + hy080g)

silc.rph <- silc.rph %>% group_by(id_h) %>% mutate( 
                                  sum_perstransf = sum(perstransf))

# Tax transfers
silc.rph <- silc.rph %>% mutate(tax = hy120g + hy130g + hy140g)

#Post tax income 
silc.rph <- silc.rph %>% mutate(posttax = prenatincome + 
                                  (perstransf + housetransf - tax)/hx050)




# Find string "py" (i.e. income variables) for summing up total personal income. 
#silc.pd <- silc.pd %>% 
#  mutate(total.inc = rowSums(silc.pd[, grep("py", colnames(silc.pd))], na.rm = TRUE)) 

# Fin ---------------------------------------------------------------------

