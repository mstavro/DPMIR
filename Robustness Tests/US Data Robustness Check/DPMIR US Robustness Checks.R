## MARTIN STAVRO
## 8.16.22
## DPMIR Main Analyses
rm(list=ls())
packages <- c("jmv", "readr", "MASS", "dplyr", "brant", "ordinal", "dotwhisker", "stargazer", "ggthemes", "googleVis", "DAMisc", "lattice", "stringr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())), repos="https://cran.rstudio.com/")
}

lapply(packages, library, character.only = T)
options(stringsAsFactors=FALSE)
options(scipen=999)
set.seed(33603)

## select DPMIR US removed
data_USremoved <- read_csv(file.choose())
## select DPMIR US recoded
data_USrecoded <- read_csv(file.choose())

## choose csv called "geocoding"
## geocoding <- read_csv(file.choose())
## G1 <- gvisGeoChart(geocoding, "geocode", "police")
## plot(G1)

## ordered logit
olrRM <- polr(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + repress_index_lagged + polity2_P4 + pop_WDI_log10 + lji_LS, data = data_USremoved, method = "logistic")
olrRE <- polr(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + repress_index_lagged + polity2_P4 + pop_WDI_log10 + lji_LS, data = data_USrecoded, method = "logistic")


## assumption test: parallel regression (proportional odds) Brant Test
brant(olrRM)
brant(olrRE)

## parallel regression / proportional odds assumption violated
## test on clm() package as a generalized ordinal logit / partial
## proportional odds model
clmRM <- clm(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + repress_index_lagged + polity2_P4 + pop_WDI_log10, nominal = ~ lji_LS, data = data_USremoved)
clmRM$convergence
clmRE <- clm(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + repress_index_lagged + polity2_P4 + pop_WDI_log10, nominal = ~ lji_LS, data = data_USrecoded)
clmRE$convergence

## linear regression
linear_modelRM <- lm(repress_index ~ police + polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
                      lji_LS, data = data_USremoved)
linear_modelRE <- lm(repress_index ~ police + polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
                       lji_LS, data = data_USrecoded)

## output all the main models as html
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Repression t-1")
output <- stargazer(olrRM, olrRE, clmRM, clmRE, linear_modelRM, linear_modelRE, type = "html", order = c(1, 3, 6, 8, 4, 2, 7, 5), covariate.labels = colabs, dep.var.labels = c("Repression Index (Inverted CIRI)", ""), column.labels = c("U.S. Removed", "U.S. Recoded to 1", "U.S. Removed", "U.S. Recoded to 1", "U.S. Removed", "U.S. Recoded to 1"))
output <- str_replace(output, "Repression t-1", "Repression<sub> t-1</sub>")
write_lines(output, "DPMIR US Robustness Check.html")


