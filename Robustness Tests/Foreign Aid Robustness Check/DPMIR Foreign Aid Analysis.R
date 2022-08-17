## MARTIN STAVRO
## 8.16.22
## DPMIR Foreign Aid Analysis
rm(list=ls())
packages <- c("jmv", "readr", "MASS", "dplyr", "brant", "ordinal", "dotwhisker", "stargazer", "ggthemes", "googleVis", "DAMisc", "lattice")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())), repos="https://cran.rstudio.com/")
}

lapply(packages, library, character.only = T)
options(stringsAsFactors=FALSE)
options(scipen=999)
set.seed(33603)

## select the DPMIR Data USAID file from 
## GitHub repo/zip folder
data <- read_csv(file.choose())

## ordered logit
olr1 <- polr(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + usaidoblige_log + hasNHRI + repress_index_lagged + polity2_P4 + pop_WDI_log10 + lji_LS, data = data, method = "logistic")
summary(olr1)

## assumption test: parallel regression (proportional odds) Brant Test
brant(olr1)

## parallel regression / proportional odds assumption violated
## test on clm() package as a generalized ordinal logit / partial
## proportional odds model
clm1 <- clm(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + usaidoblige_log + repress_index_lagged + polity2_P4, nominal = ~ lji_LS + pop_WDI_log10, data = data)
clm1$convergence

## invalid fit, mean center
data <- data %>% mutate(pop_mc = pop_WDI_log10 - mean(pop_WDI_log10, na.rm = TRUE))
data <- data %>% mutate(gdp_mc = gdp_WDI_log10 - mean(gdp_WDI_log10, na.rm = TRUE))
clm1 <- clm(as.factor(repress_index) ~ police + gdp_mc + cameo_protests + usaidoblige_log + repress_index_lagged + polity2_P4, nominal = ~ lji_LS + pop_mc, data = data)

## obtain McFadden's R for clm
clm0 <- clm(as.factor(repress_index) ~ 1, data = data)
McF.pr2 <- 1 - (clm1$logLik / clm0$logLik)

## linear regression
linear_model1 <- lm(repress_index ~ police + polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + usaidoblige_log + hasNHRI + pop_WDI_log10 + 
                      lji_LS, data = data)
summary(linear_model1)

## output all the main models as html
stargazer(olr1, clm1, linear_model1, type = "html", out = "DPMIR Foreign Aid")