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

lm1 <- lm(latentmean_FA ~ police, data)
lm2 <- lm(latentmean_FA ~ police + polity2_P4 + gdp_WDI_log10 + pop_WDI_log10 + cameo_protests + usaidoblige_log + hasNHRI + lji_LS, data)
lm3 <- lm(latentmean_FA ~ police + polity2_P4 + gdp_WDI_log10 + pop_WDI_log10 + cameo_protests + usaidoblige_log + hasNHRI + lji_LS + latentmean_FA_lagged, data)
write(stargazer(lm1, lm2, lm3, type = "html"), "fariss_table.html")