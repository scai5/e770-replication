# About ------------------------------------------------------------------------

# Replicating Table 3: Effect of generosity of EITC laws
# ECON 770 HE replication project based on Markowitz et al. (2017) 
# Replication by  Shirley Cai 
# Date created:   10/18/2023 
# Last edited:    10/23/2023 

# Replication ------------------------------------------------------------------

varlist <- c("eitc_pct", "dmage", "Dmar", "Dcfemale", 
             "Dblack", "Dnativeam", "Dasian", "Dhisp", "Dlehs", 
             "Dhispunknown", "Dcntrspop500t1000", "Dcntrspop250t500", 
             "Dcntrspop100t250", "Dcntrspople100", "unemp", 
             "rpcinc", "pctpoverty", "supplyMD_pc")
varnames <- c("EITC percent of federal", "Maternal age", "Married", "Female baby", 
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
firsttri_fe <- feols(getFormula("Dfirsttri"), 
                     data = df, 
                     vcov = cluster ~ stresfip)

# Smoked during pregnancy 
smoke_fe <- feols(getFormula("Dsmoke"), 
                  data = df, 
                  vcov = cluster ~ stresfip)

# Birth weight 
birwt_fe <- feols(getFormula("birwt"), 
                  data = df, 
                  vcov = cluster ~ stresfip)

# Low birth weight 
lowbirwt_fe <- feols(getFormula("lowbirwt"), 
                     data = df, 
                     vcov = cluster ~ stresfip)

# Gestation weeks
gestat_fe <- feols(getFormula("gestat"), 
                   data = df, 
                   vcov = cluster ~ stresfip)

rm(felist)
rm(varlist)
rm(getFormula)

# Format table -----------------------------------------------------------------

models <- list("1st trimester prenatal care" = firsttri_fe, 
               "Smoked during pregnancy" = smoke_fe, 
               "Birth weight" = birwt_fe, 
               "Birth weight < 2500 g" = lowbirwt_fe, 
               "Gestation weeks" = gestat_fe)

gof_map <- tribble(
  ~raw,      ~clean,          ~fmt,  ~omit,
  "nobs",      "Observations",     0,  FALSE
)

# Format table 
tab_3 <- modelsummary(models, 
                      stars = c('*' = .1, '**' = .05, '***' = 0.01),
                      coef_rename = varnames,
                      gof_map = gof_map,
                      fmt = 4,
                      output = "gt")

tab_3 <- tab_3 %>% 
  tab_header(
    title = md("**Table 3: Effects of Generosity of EITC Laws, Low Educated Mothers**")
  ) %>%
  tab_source_note(
    source_note = "State-level clustered standard errors in parentheses. 
                   Sample restricted to singleton births by mothers age 18 or older with high school education or less. 
                   All models include state and conception year fixed effects."
  ) %>% 
  opt_horizontal_padding(scale = 3)

# Export -----------------------------------------------------------------------

gtsave(tab_3, "./results/table3.html")
gtsave(tab_3, "./results/table3.docx")
rm(list=setdiff(ls(), "df"))
