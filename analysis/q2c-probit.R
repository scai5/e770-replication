# About ------------------------------------------------------------------------

# Rerunning LPM models with probit
# ECON 770 HE replication project based on Markowitz et al. (2017) 
# Replication by  Shirley Cai 
# Date created:   10/19/2023 
# Last edited:    10/23/2023

# Rerunning regressions --------------------------------------------------------

varlist <- c("Deitc", "dmage", "Dmar", "Dcfemale", 
             "Dblack", "Dnativeam", "Dasian", "Dhisp", "Dlehs", 
             "Dhispunknown", "Dcntrspop500t1000", "Dcntrspop250t500", 
             "Dcntrspop100t250", "Dcntrspople100", "unemp", 
             "rpcinc", "pctpoverty", "supplyMD_pc")
varnames <- c("Has state EITC", "Maternal age", "Married", "Female baby", 
              "Black", "Native American", "Asian", "Hispanic", "Less than high school", 
              "Hispanic ethnicity unknown", "County pop.500,000 - 1,000,000",
              "County pop. 250,000 - 500,000", "County pop. 100,000 - 250,000", 
              "County pop. < 100,000", "Unemployment", "Real income per capita", 
              "Percent poverty", "Primary care physicians per cap.")
felist <- c("stresfip", "cyear")

getFormula <- function(outcome){
  fml_string <- paste(outcome, 
                      paste(varlist, collapse = " + "), 
                      sep = " ~ ")
  fml_string <- paste(fml_string, 
                      paste(felist, collapse = " + "), 
                      sep = " | ")
  return(as.formula(fml_string))
}

# 1st trimester prenatal care
firsttri_fe <- feglm(getFormula("Dfirsttri"), 
                     data = df, 
                     vcov = cluster ~ stresfip, 
                     family = 'probit')

# Low birth weight 
lowbirwt_fe <- feglm(getFormula("lowbirwt"), 
                     data = df, 
                     vcov = cluster ~ stresfip, 
                     family = 'probit')

# Get marginal effects at the mean ---------------------------------------------

firsttri_me <- slopes(firsttri_fe, newdata = "mean")
lowbirwt_me <- slopes(lowbirwt_fe, newdata = "mean")

# LPM estimates ----------------------------------------------------------------

# 1st trimester prenatal care
firsttri_lpm <- feols(getFormula("Dfirsttri"), 
                     data = df, 
                     vcov = cluster ~ stresfip)

# Low birth weight 
lowbirwt_lpm <- feols(getFormula("lowbirwt"), 
                     data = df, 
                     vcov = cluster ~ stresfip)

rm(felist)
rm(varlist)
rm(getFormula)

# Format table -----------------------------------------------------------------

models <- list("Probit 1" = firsttri_fe,  "Marginal effects 1" = firsttri_me,
               "LPM 1" = firsttri_lpm,
               "Probit 2" = lowbirwt_fe, "Marginal effects 2" = lowbirwt_me, 
               "LPM 2" = lowbirwt_lpm)

gof_map <- tribble(
  ~raw,      ~clean,          ~fmt,  ~omit,
  "nobs",      "Observations",     0,  FALSE
)

# Format table 
tab_5 <- modelsummary(models, 
                      stars = c('*' = .1, '**' = .05, '***' = 0.01),
                      coef_rename = varnames,
                      gof_map = gof_map,
                      output = "gt")

tab_5 <- tab_5 %>% 
  tab_header(
    title = md("**Table 5: Presence of EITC Models Using Probit Models Instead of LPM**")
  ) %>%
  tab_source_note(
    source_note = "State-level clustered standard errors in parentheses. 
                   Sample restricted to singleton births by mothers age 18 or older with high school education or less.  
                   All models include state and conception year fixed effects.
                   Probit marginal effects are calculated at the mean."
  ) %>% 
  tab_spanner(label = '1st trimester prenatal care', columns = 2:4) %>% 
  tab_spanner(label = 'Birth weight < 2500 g', columns = 5:7) %>%
  opt_horizontal_padding(scale = 3)

# Export -----------------------------------------------------------------------

gtsave(tab_5, "./results/q2c-probit.html")
gtsave(tab_5, "./results/q2c-probit.docx")
rm(list=setdiff(ls(), "df"))
