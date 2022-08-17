## MARTIN STAVRO
## 3.19.22

rm(list=ls())
packages <- c("dplyr", "readr", "jmv", "stargazer")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())), repos="https://cran.rstudio.com/")
}

lapply(packages, library, character.only = T)
options(stringsAsFactors=FALSE)
options(scipen=999)
set.seed(33603)

##choose the data file
data <- read_csv(file.choose())
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Latent physical integrity rights t-1")

lm1 <- lm(latentmean_FA ~ police, data)
lm2 <- lm(latentmean_FA ~ police + polity2_P4 + gdp_WDI_log10 + pop_WDI_log10 + cameo_protests + hasNHRI + lji_LS, data)
lm3 <- lm(latentmean_FA ~ police + polity2_P4 + gdp_WDI_log10 + pop_WDI_log10 + cameo_protests + hasNHRI + lji_LS + latentmean_FA_lagged, data)
output <- stargazer(lm1, lm2, lm3, type = "html", order = c(1, 5, 2, 7, 6, 3, 4, 8), covariate.labels = colabs, dep.var.labels = "Latent Physical Integrity Rights")
output <- str_replace(output, "Latent physical integrity rights t-1", "Latent physical integrity rights<sub> t-1</sub>")
write_lines(output, "DPMIR Fariss Results.html")