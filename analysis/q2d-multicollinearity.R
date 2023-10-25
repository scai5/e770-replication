# About ------------------------------------------------------------------------

# Checking if multicollinearity is an issue 
# ECON 770 HE replication project based on Markowitz et al. (2017) 
# Replication by  Shirley Cai 
# Date created:   10/20/2023 
# Last edited:    10/25/2023

# Covariate correlation matrix -------------------------------------------------

varlist <- c("dmage", "Dmar", "Dcfemale", 
             "Dblack", "Dnativeam", "Dasian", "Dhisp", "Dlehs", 
             "Dhispunknown", "Dcntrspop500t1000", "Dcntrspop250t500", 
             "Dcntrspop100t250", "Dcntrspople100", "unemp", 
             "rpcinc", "pctpoverty", "supplyMD_pc")
varnames <- c("Maternal age", "Married", "Female baby", 
              "Black", "Native American", "Asian", "Hispanic", "Less than high school", 
              "Hispanic ethnicity unknown", "County pop.500,000 - 1,000,000",
              "County pop. 250,000 - 500,000", "County pop. 100,000 - 250,000", 
              "County pop. < 100,000", "Unemployment", "Real income per capita", 
              "Percent poverty", "Primary care physicians per cap.")

X_presence <- df %>% 
  select(all_of(c("Deitc", varlist)))
cor_presence <- cor(X_presence)
rownames(cor_presence) <- c("Has state EITC", varnames)
colnames(cor_presence) <- c("Has state EITC", varnames)

X_generosity <- df %>% 
  select(all_of(c("eitc_pct", varlist)))
cor_generosity <- cor(X_generosity, use = 'pairwise')
rownames(cor_generosity) <- c("EITC percent of federal", varnames)
colnames(cor_generosity) <- c("EITC percent of federal", varnames)

# Correlation graphs -----------------------------------------------------------

Cairo(file="./results/q2d-presence.png",
      type="png",
      units="px", 
      width=1200, 
      height=1200,
      pointsize = 12,
      dpi="auto")

corrplot(cor_presence, 
         method="circle", type = "lower", 
         diag = FALSE, tl.col = "black", tl.srt = 45, tl.cex = 1)

dev.off()

Cairo(file="./results/q2d-generosity.png",
      type="png",
      units="px", 
      width=1200, 
      height=1200,
      pointsize = 12,
      dpi="auto")

corrplot(cor_generosity, 
         method="circle", type = "lower", 
         diag = FALSE, tl.col = "black", tl.srt = 45, tl.cex = 1)
dev.off()

# Robustness checks ------------------------------------------------------------

rm(list=setdiff(ls(), "df"))

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
birwt_mod1 <- feols(getFormula("birwt"), 
                    data = df, 
                    vcov = cluster ~ stresfip)

varlist <- c("Deitc", "dmage", "Dmar", "Dcfemale", 
             "Dblack", "Dnativeam", "Dasian", "Dhisp", "Dlehs", 
             "Dhispunknown", "Dcntrspop500t1000", "Dcntrspop250t500", 
             "Dcntrspop100t250", "unemp", 
             "rpcinc", "pctpoverty", "supplyMD_pc")
varnames <- c("Has state EITC", "Maternal age", "Married", "Female baby", 
              "Black", "Native American", "Asian", "Hispanic", "Less than high school", 
              "Hispanic ethnicity unknown", "County pop.500,000 - 1,000,000",
              "County pop. 250,000 - 500,000", "County pop. 100,000 - 250,000", 
              "Unemployment", "Real income per capita", 
              "Percent poverty", "Primary care physicians per cap.")
birwt_mod2 <- feols(getFormula("birwt"), 
                    data = df, 
                    vcov = cluster ~ stresfip)

varlist <- c("Deitc", "dmage", "Dmar", "Dcfemale", 
             "Dblack", "Dnativeam", "Dasian", "Dhisp", "Dlehs", 
             "Dhispunknown", "Dcntrspop500t1000", "Dcntrspop250t500", 
             "Dcntrspop100t250", "Dcntrspople100", 
             "rpcinc", "pctpoverty", "supplyMD_pc")
varnames <- c("Has state EITC", "Maternal age", "Married", "Female baby", 
              "Black", "Native American", "Asian", "Hispanic", "Less than high school", 
              "Hispanic ethnicity unknown", "County pop.500,000 - 1,000,000",
              "County pop. 250,000 - 500,000", "County pop. 100,000 - 250,000", 
              "County pop. < 100,000", "Real income per capita", 
              "Percent poverty", "Primary care physicians per cap.")
birwt_mod3 <- feols(getFormula("birwt"), 
                    data = df, 
                    vcov = cluster ~ stresfip)

# Format table -----------------------------------------------------------------

models <- list(birwt_mod1, birwt_mod2, birwt_mod3)

gof_map <- tribble(
  ~raw,      ~clean,          ~fmt,  ~omit,
  "nobs",      "Observations",     0,  FALSE
)

# Format table 
varnames <- c("Has state EITC", "Maternal age", "Married", "Female baby", 
              "Black", "Native American", "Asian", "Hispanic", "Less than high school", 
              "Hispanic ethnicity unknown", "County pop.500,000 - 1,000,000",
              "County pop. 250,000 - 500,000", "County pop. 100,000 - 250,000", 
              "County pop. < 100,000", "Unemployment", "Real income per capita", 
              "Percent poverty", "Primary care physicians per cap.")
tab_6 <- modelsummary(models, 
                      stars = c('*' = .1, '**' = .05, '***' = 0.01),
                      coef_rename = varnames,
                      gof_map = gof_map,
                      output = "gt")

tab_6 <- tab_6 %>% 
  tab_header(
    title = md("**Table 6: Robustness Check**")
  ) %>%
  tab_source_note(
    source_note = "State-level clustered standard errors in parentheses. 
                   Sample restricted to singleton births by mothers age 18 or older with high school education or less.  
                   All models include state and conception year fixed effects."
  ) %>% 
  tab_spanner(label = 'Birth weight', columns = 2:4) %>% 
  opt_horizontal_padding(scale = 3)

# Getting vif ------------------------------------------------------------------

varlist <- c("Deitc", "dmage", "Dmar", "Dcfemale", 
             "Dblack", "Dnativeam", "Dasian", "Dhisp", "Dlehs", 
             "Dhispunknown", "Dcntrspop500t1000", "Dcntrspop250t500", 
             "Dcntrspop100t250", "unemp", 
             "rpcinc", "pctpoverty", "supplyMD_pc")
varnames <- c("Has state EITC", "Maternal age", "Married", "Female baby", 
              "Black", "Native American", "Asian", "Hispanic", "Less than high school", 
              "Hispanic ethnicity unknown", "County pop. 500,000 - 1,000,000",
              "County pop. 250,000 - 500,000", "County pop. 100,000 - 250,000", 
              "Unemployment", "Real income per capita", 
              "Percent poverty", "Primary care physicians per cap.")

getFormula <- function(outcome){
  fml_string <- paste(outcome, 
                      paste(varlist, collapse = " + "), 
                      sep = " ~ ")
  return(as.formula(fml_string))
}

vif_tbl <- data.frame(Variable = varnames) 

Dfirsttri_ols <- lm(getFormula("Dfirsttri"), data = df)
vif_tbl$Dfirsttri <- vif(Dfirsttri_ols)

Dsmoke_ols <- lm(getFormula("Dsmoke"), data = df)
vif_tbl$Dsmoke <- vif(Dsmoke_ols)

birwt_ols <- lm(getFormula("birwt"), data = df)
vif_tbl$birwt <- vif(birwt_ols)

lowbirwt_ols <- lm(getFormula("lowbirwt"), data = df)
vif_tbl$lowbirwt <- vif(lowbirwt_ols)

gestat_ols <- lm(getFormula("gestat"), data = df)
vif_tbl$gestat <- vif(gestat_ols)

colnames(vif_tbl) <- c("Variable", 
                       "1st trimester prenatal care", "Smoked during pregnancy", 
                       "Birth weight", "Birth weight < 2500 g", "Gestation weeks")

tab_vif <- gt(vif_tbl) %>% 
  tab_header(
    title = md("**Table 7: Variance Inflation Factors**")
  ) %>%
  tab_source_note(
    source_note = "VIFs computed from a pooled OLS regression with an intercept.  
                   Sample restricted to singleton births by mothers age 18 or older with high school education or less."
  ) %>% 
  opt_horizontal_padding(scale = 3)

# Export -----------------------------------------------------------------------

gtsave(tab_6, "./results/q2d-robustness.html")
gtsave(tab_6, "./results/q2d-robustness.docx")
gtsave(tab_vif, "./results/q2d-vif.html")
gtsave(tab_vif, "./results/q2d-vif.docx")
rm(list=setdiff(ls(), "df"))
