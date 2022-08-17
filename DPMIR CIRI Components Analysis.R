## MARTIN STAVRO
## 6.23.22
## DNR DISAGGREGATION BY CIRI COMPONENT
rm(list=ls())
packages <- c("jmv", "readr", "MASS", "dplyr", "brant", "ordinal", "dotwhisker", "stargazer", "ggthemes", "googleVis", "DAMisc", "lattice", "DescTools")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())), repos="https://cran.rstudio.com/")
}

lapply(packages, library, character.only = T)
options(stringsAsFactors=FALSE)
options(scipen=999)
set.seed(33603)

## import full dataset with CIRI components flipped
data <- read_csv(file.choose())

## run ordered logits on each component
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Disappearance t-1", "Political imprisonment t-1", "Extrajudicial killing t-1", "Torture t-1")
olrK <- polr(as.factor(kill) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + kill_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data, method = "logistic")
olrT <- polr(as.factor(tort) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + tort_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data, method = "logistic")
olrD <- polr(as.factor(disap) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + disap_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data, method = "logistic")
olrP <- polr(as.factor(polpris) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + polpris_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data, method = "logistic")
output <- stargazer(olrD, olrP, olrK, olrT, type = "html", order = c(1, 3, 9, 11, 4, 2, 10, 5, 6, 7, 8), covariate.labels = colabs, dep.var.labels = c("Disappearance", "Political Imprisonment", "Extrajudicial Killing", "Torture"))
output <- str_replace(output, "Disappearance t-1", "Disappearance<sub> t-1</sub>")
output <- str_replace(output, "Political imprisonment t-1", "Political imprisonment<sub> t-1</sub>")
output <- str_replace(output, "Extrajudicial killing t-1", "Extrajudicial killing<sub> t-1</sub>")
output <- str_replace(output, "Torture t-1", "Torture<sub> t-1</sub>")

write_lines(output, "DPMIR CIRI Components OLR.html")
## brant tests
brant(olrK)
brant(olrT)
brant(olrD)
brant(olrP)

## CLM
clmK <- clm(as.factor(kill) ~ police + cameo_protests + hasNHRI + polity2_P4 + pop_WDI_log10 + lji_LS, nominal = ~ gdp_WDI_log10 + kill_lag, data = data)
## torture passes the proportional odds assumption
clmD <- clm(as.factor(disap) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10, nominal = ~ disap_lag + lji_LS + polity2_P4, data = data)
clmP <- clm(as.factor(polpris) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + polpris_lag + polity2_P4 + pop_WDI_log10, nominal = ~ lji_LS, data = data)
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Political imprisonment t-1")
output <- stargazer(clmD, clmP, clmK, type = "html", order = c(1, 3, 6, 8, 4, 2, 7, 5), covariate.labels = colabs, dep.var.labels = c("Disappearance", "Political Imprisonment", "Extrajudicial Killing"))
output <- str_replace(output, "Political imprisonment t-1", "Political imprisonment<sub> t-1</sub>")
write_lines(output, "DPMIR CIRI Components CLM.html")

## full plot
stargazer(olrD, clmD, olrP, clmP, olrK, clmK, olrT, type = "html", out = "DPMIR CIRI Components OLR and CLM.html")

PseudoR2(olrK)
PseudoR2(olrT)
PseudoR2(olrP)
PseudoR2(olrD)
