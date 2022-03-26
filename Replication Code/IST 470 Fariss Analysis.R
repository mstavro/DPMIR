## MARTIN STAVRO
## 3.19.22

rm(list=ls())
packages <- c("dplyr", "readr", "jmv")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())), repos="https://cran.rstudio.com/")
}

lapply(packages, library, character.only = T)
options(stringsAsFactors=FALSE)
options(scipen=999)
set.seed(33603)

##choose the original IST data file
IST470 <- read_csv(file.choose())

IST470 <- IST470 %>% select(cowcode, year, country.x, police, polity2_P4, latentmean_FA, latentmean_FA_lagged, gdp_WDI_log10, pop_WDI_log10, cameo_protests, trade_WDI, lji_LS)

IST470 <- IST470 %>% distinct(cowcode, year, .keep_all = TRUE)

IST470 <- na.omit(IST470)
##output new file
linReg(
  data = data,
  dep = latentmean_FA,
  covs = vars(polity2_P4, latentmean_FA_lagged, gdp_WDI_log10, pop_WDI_log10, cameo_protests, trade_WDI, lji_LS),
  factors = police,
  blocks = list(
    list(
      "police"),
    list(
      "polity2_P4",
      "gdp_WDI_log10",
      "pop_WDI_log10",
      "cameo_protests",
      "trade_WDI",
      "lji_LS"),
    list(
      "latentmean_FA_lagged")),
  refLevels = list(
    list(
      var="police",
      ref="0")),
  r2Adj = TRUE)