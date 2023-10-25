# About ------------------------------------------------------------------------

# Master script
# ECON 770 HE replication project based on Markowitz et al. (2017) 
# Replication by  Shirley Cai 
# Date created:   10/10/2023 
# Last edited:    10/20/2023 
# Last run:       10/25/2023

# Preliminary ------------------------------------------------------------------

library(tidyverse)
library(haven)
library(xtable)
library(vtable)
library(gt)
library(gtsummary)
library(plm)
library(fixest)
library(modelsummary)
library(car)
library(marginaleffects)
library(corrplot)
library(Cairo)

# Clean data -------------------------------------------------------------------

source("./data-code/cleaning.R")
df <- read_csv("./data/generated/clean_merged.csv")

# Replicate tables -------------------------------------------------------------

source("./analysis/table1.R")
source("./analysis/table2.R")
source("./analysis/table3.R")

# Additional analyses ----------------------------------------------------------

source("./analysis/q2b-refund.R")
source("./analysis/q2c-probit.R")
source("./analysis/q2d-multicollinearity.R")
