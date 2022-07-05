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
olrK <- polr(as.factor(kill) ~ police + gdp_WDI_log10 + cameo_protests + trade_WDI + kill_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data, method = "logistic")
olrT <- polr(as.factor(tort) ~ police + gdp_WDI_log10 + cameo_protests + trade_WDI + tort_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data, method = "logistic")
olrD <- polr(as.factor(disap) ~ police + gdp_WDI_log10 + cameo_protests + trade_WDI + disap_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data, method = "logistic")
olrP <- polr(as.factor(polpris) ~ police + gdp_WDI_log10 + cameo_protests + trade_WDI + polpris_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data, method = "logistic")
stargazer(olrD, olrP, olrK, olrT, type = "html", out = "DNR CIRI Components OLR.html")

## brant tests
brant(olrK)
brant(olrT)
brant(olrD)
brant(olrP)

## CLM
clmK <- clm(as.factor(kill) ~ police + cameo_protests + trade_WDI + polity2_P4 + pop_WDI_log10 + lji_LS, nominal = ~ kill_lag + gdp_WDI_log10, data = data)
## torture passes the proportional odds assumption
clmD <- clm(as.factor(disap) ~ police + gdp_WDI_log10 + cameo_protests + trade_WDI + pop_WDI_log10, nominal = ~ disap_lag + polity2_P4 + lji_LS, data = data)
clmP <- clm(as.factor(polpris) ~ police + cameo_protests + polity2_P4 + pop_WDI_log10, nominal = ~ gdp_WDI_log10 + trade_WDI + polpris_lag + lji_LS, data = data)
stargazer(clmD, clmP, clmK, type = "html", out = "DNR CIRI Components CLM.html")

## full plot
stargazer(olrD, clmD, olrP, clmP, olrK, clmK, olrT, type = "html", out = "DNR CIRI Components OLR and CLM.html")

PseudoR2(olrK)
PseudoR2(olrT)
PseudoR2(olrP)
PseudoR2(olrD)
