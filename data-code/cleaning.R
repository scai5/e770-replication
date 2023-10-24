# About ------------------------------------------------------------------------

# Data merging and cleaning 
# ECON 770 HE replication project based on Markowitz et al. (2017) 
# Replication by  Shirley Cai 
# Date created:   10/04/2023 
# Last edited:    10/23/2023 
# Last run:       10/23/2023

# Preliminary ------------------------------------------------------------------

library(tidyverse)
library(haven)

# Import and clean birth certificate data --------------------------------------

source("./data-code/clean_natl2001.R")
source("./data-code/clean_natl2002.R")
source("./data-code/clean_natl2003.R")

## Merge data ------------------------------------------------------------------

natl2001 <- read_csv("./data/generated/clean_2001.csv")
natl2002 <- read_csv("./data/generated/clean_2002.csv")
natl2003 <- read_csv("./data/generated/clean_2003.csv")

merged <- bind_rows(list(natl2001, natl2002, natl2003))

rm(natl2001)
rm(natl2002)
rm(natl2003)

# Create conception year variable
b_centurym <- (merged$biryr*12 - (12*2001)) + merged$birmon
merged$gestmon <- round(merged$gestat*84/365)
c_centurym <- b_centurym - merged$gestmon
merged$cyear <- case_when(
  c_centurym < 1 ~ 2000, 
  c_centurym <= 12 ~ 2001, 
  c_centurym <= 24 ~ 2002, 
  c_centurym <= 36 ~ 2003, 
  TRUE ~ NA
)

# Filter sample 
merged <- merged %>% 
  filter(!is.na(gestat) & cyear >= 2001)

rm(b_centurym)
rm(c_centurym)

# Merge in county level data ---------------------------------------------------

# Import county level data 
county_vars <- read_dta("./data/raw/county_vars.dta")
ruralcounty_vars <- read_dta("./data/raw/ruralcounty_vars.dta")
eitc <- read_dta("./data/raw/eitc.dta")

ruralcounty_vars <- ruralcounty_vars %>% 
  mutate(unemp = unemp * 100)

# Merge EITC vars 
merged <- merged %>%
  mutate(
    eitc_cat = case_when(
      nlbnl <= 3 ~ nlbnl, 
      TRUE ~ 3
    )
  ) %>% 
  left_join(eitc, by = c('stresfip' = 'stfips', 
                         'cyear' = 'year', 
                         'eitc_cat' = 'numchildren'))

# Merge county vars
ruralcounty_vars$cofips <- 999
allcounty_vars <- bind_rows(list(county_vars, ruralcounty_vars))
allcounty_vars <- allcounty_vars %>% 
  select(!(pop_lt_100k))

merged <- merged %>% 
  left_join(allcounty_vars,
         by = c('stresfip' = 'stfips',
                'cntyrfip' = 'cofips',
                'cyear' = 'year'),
         keep=FALSE)

rm(county_vars)
rm(eitc)
rm(ruralcounty_vars)
rm(allcounty_vars)

# Delete observations missing county level data, transform eitc to conditional
merged <- merged %>% 
  filter(!is.na(eitc_pct) & !is.na(unemp)) %>% 
  mutate(
    Deitc = as.numeric(eitc_pct > 0), 
    eitc_pct = case_when(
      Deitc == 0 ~ NA, 
      TRUE ~ eitc_pct
    ), 
    refund = case_when(
      Deitc == 0 ~ NA, 
      TRUE ~ refund
    ),
    Dcntrspop500t1000 = as.numeric(cntrspop == 1), 
    Dcntrspop250t500 = as.numeric(cntrspop == 2),
    Dcntrspop100t250 = as.numeric(cntrspop == 3), 
    Dcntrspople100 = as.numeric(cntrspop == 9)
  )

# Export cleaned data ----------------------------------------------------------

write_csv(merged, "./data/generated/clean_merged.csv")
rm(list = ls())
