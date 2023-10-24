# About ------------------------------------------------------------------------

# Replicating Table 1: Summary Statistics
# ECON 770 HE replication project based on Markowitz et al. (2017) 
# Replication by  Shirley Cai 
# Date created:   10/10/2023 
# Last edited:    10/20/2023 

# Replication ------------------------------------------------------------------

# Function that returns summary stats
describe <- function(variable){
  summ <- summary(variable)
  summ <- c("Mean" = mean(variable, na.rm=TRUE),
            "SD" = sd(variable, na.rm=TRUE),
            "Min" = min(variable, na.rm=TRUE),
            "Max" = max(variable, na.rm=TRUE),
            "N" = sum(!is.na(variable)))
  return(tibble(summ))
}

# Summarize for all obs
summ <- df %>% 
  reframe(across(c(Dfirsttri, Dsmoke,
                   birwt, lowbirwt, gestat, 
                   Deitc, eitc_pct, refund, dmage, Dmar, Dcfemale, 
                   Dblack, Dnativeam, Dasian, Dhisp, 
                   Dlehs, Dhispunknown, 
                   Dcntrspop500t1000, Dcntrspop250t500, 
                   Dcntrspop100t250, Dcntrspople100,
                   unemp, rpcinc, pctpoverty, supplyMD_pc),
                 describe))

summ <- as.data.frame(t(summ))
colnames(summ) <- c("Mean", "Std. Dev", "Min", "Max", "N")

# Format table -----------------------------------------------------------------

varnames <- c("Dfirsttri" = "1st trimester prenatal care", 
              "Dsmoke" = "Smoked during pregnancy", 
              "birwt" = "Birth weight", 
              "lowbirwt" = "Birth weight < 2500 grams", 
              "gestat" = "Gestation weeks", 
              "Deitc" = "State has EITC", 
              "eitc_pct" = "EITC percent of federal (among states with EITC)", 
              "refund" = "State has a refund (among states with EITC)", 
              "dmage" = "Maternal age", 
              "Dmar" = "Married", 
              "Dcfemale" = "Female baby", 
              "Dblack" = "Black", 
              "Dnativeam" = "Native American", 
              "Dasian" = "Asian", 
              "Dhisp" = "Hispanic", 
              "Dlehs" = "Less than high school", 
              "Dhispunknown" = "Hispanic ethnicity missing", 
              "Dcntrspop500t1000" = "County pop. 500,000 - 1,000,000", 
              "Dcntrspop250t500" = "County pop. 250,000 - 500,000", 
              "Dcntrspop100t250" = "County pop. 100,000 - 250,000",
              "Dcntrspople100" = "County pop. < 100,000", 
              "unemp" = "Unemployment", 
              "rpcinc" = "Real income per capita (in $1000s)", 
              "pctpoverty" = "Percent poverty", 
              "supplyMD_pc" = "Primary care physicians per 1000 females age 15-44"
)

summ <- summ %>% 
  add_column(Variable = rownames(summ), .before="Mean") %>% 
  mutate(
    Variable = dplyr::recode(Variable, !!!varnames)
  )
rownames(summ) <- NULL

tab_1 <- gt(summ, rowname_col = "Variable") %>%
  tab_stubhead(label = "Variable") %>% 
  tab_header(
    title = md("**Table 1: Summary Statistics**")
  ) %>%
  tab_row_group(
    label = "County-Level Covariates", 
    rows = 22:25
  ) %>%
  tab_row_group(
    label = "Individual-Level Covariates", 
    rows = 9:21
  ) %>%
  tab_row_group(
    label = "State EITC Variables", 
    rows = 6:8
  ) %>% 
  tab_row_group(
    label = "Dependent Variables: Infant Health", 
    rows = 3:5
  ) %>% 
  tab_row_group(
    label = "Dependent Variables: Maternal Health Behaviors", 
    rows = 1:2
  ) %>% 
  fmt_number( 
    decimals = 3, 
    drop_trailing_zeros = TRUE
  ) %>% 
  tab_style(
    style = cell_text(align = "left", indent = px(20)),
    locations = cells_stub()
  ) %>% 
  opt_horizontal_padding(scale = 3)

# Export -----------------------------------------------------------------------

gtsave(tab_1, "./results/table1.html")
gtsave(tab_1, "./results/table1.docx")
rm(list=setdiff(ls(), "df"))
