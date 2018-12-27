# Setup data for Austria -------------------------------------------------------------------

library(dplyr)
if(!exists(c("country", "year"))) {
  stop("Please specify country and year.")
}

# Download Data using required variables ---------------------------------------------------

#Personal Data  
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 == "AT" & pb010 %in% c(2005:2017)) %>%
  select(pb010, pb030, py010g, py050g, py080g, py090g, py100g, py110g, py120g, 
         py130g, py140g) %>%
  collect(n = Inf)

#Household data
silc.h <- tbl(pg, "hh") %>%
  filter(hb020 == "AT" & hb010 %in% c(2005:2017)) %>%
  select(hb010, hb020, hb030, hy020, hy030g, hy040g, hy050g, hy060g, hy070g, 
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
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

#### Include variable for cars and other non cash benefits py021g for entire timeframe

#######not done
#######

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
