# About ------------------------------------------------------------------------

# Importing and cleaning 2003 birth certificate data 
# ECON 770 HE replication project based on Markowitz et al. (2017) 
# Replication by  Shirley Cai 
# Date created:   10/09/2023 
# Last edited:    10/16/2023

# Import data ------------------------------------------------------------------

natl2003 <- read.delim("./data/raw/Nat03us.dat", header=FALSE)
colnames(natl2003) <- c("raw")

# Labeling variables from codebook ---------------------------------------------

# Birth month and year 
natl2003 <- natl2003 %>% 
  mutate(
    revision = substr(raw, 7, 7),
    biryr = substr(raw, 15, 18), 
    birmon = substr(raw, 19, 20)
  ) 

# Maternal health behaviors
natl2003 <- natl2003 %>% 
  mutate(
    prenatal_r = substr(raw, 247, 247), 
    prenatal_u = substr(raw, 259, 259), 
    smoke_r = substr(raw, 294, 294),
    smoke_u = substr(raw, 290, 290)
  )

# Infant health 
natl2003 <- natl2003 %>% 
  mutate(
    dbirwt = substr(raw, 463, 466), 
    dgestat = substr(raw, 451, 452)
  )

# Maternal characteristics
natl2003 <- natl2003 %>% 
  mutate(
    mage = substr(raw, 89, 90), 
    dmar = substr(raw, 153, 153), 
    csex = substr(raw, 436, 436), 
    meduc_r = substr(raw, 155, 155), 
    meduc_u = substr(raw, 156, 157), 
    mrace = substr(raw, 143, 143), 
    ormoth = substr(raw, 148, 148), 
    stres = substr(raw, 109, 110), 
    cntyrfip = substr(raw, 114, 116), 
    cntrspop = substr(raw, 132, 132), 
    nlbnl = substr(raw, 204, 205), 
    dplural = substr(raw, 423, 423)
  )

# Recoding and cleaning --------------------------------------------------------

natl2003 <- natl2003 %>% 
  select(!(raw)) 

# Map state to stresfip 
stfips_crosswalk <- read_csv("./data/raw/stfips_crosswalk.csv")
natl2003 <- merge(x = natl2003, y = stfips_crosswalk, by.x = "stres", by.y = "stabbr",
      all.x = TRUE)

# Drop foreign res, convert to numeric 
natl2003 <- natl2003 %>% 
  mutate(
    Dcfemale = as.numeric(csex == "F"), 
    Drevised = as.numeric(revision == "A"), 
    smoke_r = case_when(
      smoke_r == "Y" ~ 1, 
      smoke_r == "N" ~ 0, 
      TRUE ~ NA
    )
  ) %>%
  select(!c(stres, state, csex, revision)) %>%
  filter(cntrspop != "Z") %>% 
  lapply(as.numeric) %>% 
  as.data.frame()

# Combine R/U variables
natl2003 <- natl2003 %>% 
  mutate(
    dmage = mage + 13, 
    Dfirsttri = case_when(
      Drevised == 1 & prenatal_r == 1 ~ 1, 
      Drevised == 1 & prenatal_r >= 2 & prenatal_r <= 4 ~ 0, 
      Drevised == 0 & prenatal_u == 1 ~ 1, 
      Drevised == 0 & prenatal_u >= 2 & prenatal_u <= 4 ~ 0, 
      TRUE ~ NA
    ), 
    Dsmoke = case_when(
      Drevised == 1 & smoke_r == 1 ~ 1, 
      Drevised == 1 & smoke_r == 0 ~ 0, 
      Drevised == 0 & smoke_u == 1 ~ 1, 
      Drevised == 0 & smoke_u == 2 ~ 0, 
      TRUE ~ NA
    ), 
    Dcollegeplus = case_when(
      Drevised == 1 & meduc_r >= 4 & meduc_r <= 8 ~ 1, 
      Drevised == 1 & meduc_r <= 3 ~ 0, 
      Drevised == 0 & meduc_u == 99 ~ NA, 
      Drevised == 0 & meduc_u >= 13 ~ 1, 
      Drevised == 0 & meduc_u < 13 ~ 0, 
      TRUE ~ NA
    ), 
    Dlehs = case_when(
      Drevised == 1 & meduc_r <= 2 ~ 1, 
      Drevised == 1 & meduc_r == 9 ~ NA, 
      Drevised == 0 & meduc_u <= 11 ~ 1, 
      Drevised == 0 & meduc_u == 99 ~ NA,
      TRUE ~ 0
    )
  )

# Drop non-singleton births, teen moms, >HS, missing # children
natl2003 <- natl2003 %>% 
  filter(dplural == 1 & dmage >= 18) %>% 
  filter(Dcollegeplus == 0) %>% 
  filter(nlbnl != 99)

# Recode variables
natl2003 <- natl2003 %>% 
  mutate(
    Dmar = case_when(
      dmar == 1 ~ 1, 
      dmar == 9 ~ NA, 
      TRUE ~ 0
    ), 
    Dblack = as.numeric(mrace == 2), 
    Dnativeam = as.numeric(mrace == 3),
    Dasian = as.numeric(mrace == 4),
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
natl2003 <- natl2003 %>%
  select(!c(prenatal_r, prenatal_u, smoke_r, smoke_u, dbirwt,
            dgestat, mage, dmar, meduc_r, meduc_u, mrace, 
            ormoth, dplural, Drevised, Dcollegeplus)) %>%
  rename(stresfip = stfips)
  
# Export as .csv ---------------------------------------------------------------

write_csv(natl2003, "./data/generated/clean_2003.csv")
rm(list = ls())