## MARTIN STAVRO
## 6.26.22
## DNR Descriptive Data
## Obtain descriptive data from full dataset

rm(list = ls())
packages <- c("jmv", "readr", "dplyr")
if(length(setdiff(packages, rownames(installed.packages()))) > 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "https://cran.rstudio.com/")
}

lapply(packages, library, character.only = T)
options(stringsAsFactors = F)
options(scipen = 999)
set.seed(33603)

## choose the DNR full data from the github repo
data <- read_csv(file.choose())

## obtain descriptives
descriptives(
  data = data,
  vars = vars(police, polity2_P4, gdp_WDI_log10, pop_WDI_log10, cameo_protests, trade_WDI, lji_LS, repress_index),
  desc = "rows",
  bar = TRUE,
  dotType = "stack",
  boxMean = TRUE)
