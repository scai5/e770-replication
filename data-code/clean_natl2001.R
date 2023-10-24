# About ------------------------------------------------------------------------

# Importing and cleaning 2001 birth certificate data 
# ECON 770 HE replication project based on Markowitz et al. (2017) 
# Replication by  Shirley Cai 
# Date created:   10/05/2023 
# Last edited:    10/16/2023 

# Import data ------------------------------------------------------------------

natl2001 <- read.delim("./data/raw/Natl2001.dat", header=FALSE)
colnames(natl2001) <- c("raw")

# Labeling variables from codebook ---------------------------------------------

# Birth year 
natl2001 <- natl2001 %>% 
  mutate(
    biryr = substr(raw, 176, 179), 
    birmon = substr(raw, 172, 173)
  )

# Maternal health behaviors
natl2001 <- natl2001 %>% 
  mutate(
    mpre5 = substr(raw, 109, 109), 
    tobacco = substr(raw, 242, 242)
  )

# Infant health 
natl2001 <- natl2001 %>% 
  mutate(
    dbirwt = substr(raw, 193, 196),
    dgestat = substr(raw, 183, 184)
  )

# Maternal characteristics
natl2001 <- natl2001 %>% 
  mutate(
    dmage = substr(raw, 70, 71),
    dmar = substr(raw, 87, 87),
    csex = substr(raw, 189, 189), 
    dmeduc = substr(raw, 83, 84), 
    mrace = substr(raw, 80, 81),
    ormoth = substr(raw, 77, 77),
    stresfip = substr(raw, 42, 43), 
    cntyrfip = substr(raw, 44, 46), 
    cntrspop = substr(raw, 58, 58), 
    nlbnl = substr(raw, 94, 95), 
    dplural = substr(raw, 201, 201)
  ) 

# Recoding and cleaning --------------------------------------------------------

# Convert to numeric, drop foreign res
natl2001 <- natl2001 %>% 
  select(!(raw)) %>%
  filter(cntyrfip != "000" & cntrspop != "Z") %>% 
  lapply(as.numeric) %>% 
  as.data.frame()

# Drop non-singleton births, teen moms, >HS, missing # children 
natl2001 <- natl2001 %>% 
  filter(dplural == 1 & dmage >= 18) %>% 
  filter(dmeduc != 99 & dmeduc <= 12) %>% 
  filter(nlbnl != 99)

# Recode variables 
natl2001 <- natl2001 %>% 
  mutate(
    Dfirsttri = case_when(
      mpre5 == 1 ~ 1, 
      mpre5 == 5 ~ NA, 
      TRUE ~ 0
    ),
    Dsmoke = case_when(
      tobacco == 1 ~ 1, 
      tobacco == 9 ~ NA, 
      TRUE ~ 0
    ),
    Dmar = case_when(
      dmar == 1 ~ 1, 
      dmar == 9 ~ NA, 
      TRUE ~ 0
    ), 
    Dcfemale = as.numeric(csex == 2),
    Dlehs = as.numeric(dmeduc <= 11),
    Dblack = as.numeric(mrace == 2), 
    Dnativeam = as.numeric(mrace == 3), 
    Dasian = as.numeric(mrace >= 4), 
    Dhisp = case_when(
      ormoth == 0 ~ 0, 
      ormoth == 9 ~ 0, 
      TRUE ~ 1
    ), 
    Dhispunknown = as.numeric(ormoth == 9), 
    birwt = case_when(
      dbirwt == 9999 ~ NA, 
      TRUE ~ dbirwt
    ),
    lowbirwt = as.numeric(birwt < 2500), 
    gestat = case_when(
      dgestat == 99 ~ NA, 
      TRUE ~ dgestat
    )
  )

# Remove variables unneeded for analysis 
natl2001 <- natl2001 %>% 
  select(!c(mpre5, tobacco, dmar, csex, dmeduc, mrace, 
            ormoth, dplural, dbirwt, dgestat))

# Export as .csv ---------------------------------------------------------------

write_csv(natl2001, "./data/generated/clean_2001.csv")
rm(list = ls())