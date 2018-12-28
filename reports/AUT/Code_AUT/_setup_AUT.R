# Setup data for Austria -------------------------------------------------------------------

library(dplyr)
if(!exists(c("country", "year"))) {
  stop("Please specify country and year.")
}

# Download Data using required variables ---------------------------------------------------

#Personal Data  
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 == "AT" & pb010 %in% c(2005:2017)) %>%
  select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, 
         py080g, py090g, py100g, py110g, py120g, px010,px030 py130g, py140g) %>%
  collect(n = Inf)

#Household data
silc.h <- tbl(pg, "hh") %>%
  filter(hb020 == "AT" & hb010 %in% c(2005:2017)) %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, hy050g, hy060g, hy070g, 
         hy080g, hy090g, hy110g, hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

#Household register
silc.d <- tbl(pg, "dd") %>%
  filter(db020 == "AT" & db010 %in% c(2005:2017)) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

#Personal register
silc.r <- tbl(pg, "rr") %>% 
  filter(rb020 == "AT" & rb010 %in% c(2005:2017)) %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx010, rx030) %>%
  collect(n = Inf)

#### Include variable for cars and other non cash benefits py021g for entire timeframe


# Download c[YY]p tables from 2007 - 2013
c07p <- tbl(pg, "c07p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
                                                                  py021g) %>% collect(n = Inf)

c08p <- tbl(pg, "c08p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
                                                                  py021g) %>% collect(n = Inf)

c09p <- tbl(pg, "c09p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
                                                                  py021g) %>% collect(n = Inf)

c10p <- tbl(pg, "c10p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
                                                                  py021g) %>% collect(n = Inf)

c11p <- tbl(pg, "c11p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
                                                                  py021g) %>% collect(n = Inf)

c12p <- tbl(pg, "c12p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
                                                                  py021g) %>% collect(n = Inf)

c13p <- tbl(pg, "c13p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
                                                                  py021g) %>% collect(n = Inf)

cxxp <- bind_rows(c07p, c08p, c09p, c10p, c11p, c12p, c13p)
rm(c07p, c08p, c09p, c10p, c11p, c12p, c13p)
# Merge cxxp with silc.p to include the py021g variable for 2007-2013

silc.p <- left_join(silc.p, cxxp %>% select(py021g, pb010, pb030))
rm(cxxp)

#renaming the variable rb030 and pb 030 to personal_id 

silc.r <- silc.r %>% rename(id = rb030)
silc.p <- silc.p %>% rename(id = pb030)

#Putting the datasets together creating one data set containing personal data and register
silc.rp <- left_join(silc.r, silc.p)

#renaming variables in the dataset for clarity

silc.rp <- silc.rp %>% rename(country = rb020)
silc.rp <- silc.rp %>% rename(survey_year = rb010)

#Create new variables (age, gender and household ID)

silc.rp <- silc.rp %>% 
  mutate(age = rb010 - rb080,
         gender = factor(rb090, labels = c('Male','Female')),
         id_h = paste0(rb020, rx030)) 

# Create unique IDs for merging
#silc.p <- silc.p %>% mutate(id_h = paste0(pb020, px030))

silc.h <- silc.h %>% mutate(id_h = paste0(hb020, hb030 #,hb010?
))

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030 #,db010
))
#Merge datasets using unique IDs

silc.rph <- left_join(silc.rp, silc.h)

#silc.rphd <- left_join(silc.rph, silc.d)
#cannot allocate vector of size 220.9Mb (Error)

#Replace not answered NA
silc.rph[is.na(silc.rph)] <- 0

#####Continue here !!!!

# Create total personal income --------------------------------------------

# Find string "py" (i.e. income variables) for summing up total personal income. 
silc.pd <- silc.pd %>% 
  mutate(total.inc = rowSums(silc.pd[, grep("py", colnames(silc.pd))], 
                             na.rm = TRUE)) 

# Fin ---------------------------------------------------------------------

message("Prepared data for ", country, " in ", year, ".")
