## MARTIN STAVRO
## 8.16.22
## DPMIR PTS Robustness Check

rm(list=ls())
packages <- c("jmv", "readr", "MASS", "dplyr", "brant", "car", "ordinal", "dotwhisker", "stargazer", "ggthemes", "googleVis", "DAMisc", "lattice")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())), repos="https://cran.rstudio.com/")
}

lapply(packages, library, character.only = T)
options(stringsAsFactors=FALSE)
options(scipen=999)
set.seed(33603)

## select the main data file
## GitHub repo/zip folder
data <- read_csv(file.choose())
data <- data %>% mutate(apts_lag = lag(politterr_a_PTS))
data <- data %>% mutate(spts_lag = lag(politterr_s_PTS))

## ordered logit with amnesty PTS
olrA <- polr(as.factor(politterr_a_PTS) ~ police + gdp_WDI_log10 + cameo_protests + usaidoblige_log + apts_lag + polity2_P4 + pop_WDI_log10 + lji_LS + hasNHRI, data = data, method = "logistic")
summary(olrA)

## ordered logit state PTS
olrS <- polr(as.factor(politterr_s_PTS) ~ police + gdp_WDI_log10 + cameo_protests + trade_WDI + spts_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data, method = "logistic")
summary(olrS)

## assumption test: parallel regression (proportional odds) Brant Test
brant(olrA)
brant(olrS)

## parallel regression / proportional odds assumption violated
## test on clm() package as a generalized ordinal logit / partial
## proportional odds model

## results in nonconvergence without mean centering of GDP and pop
data <- data %>% mutate(pop_mc = pop_WDI_log10 - mean(pop_WDI_log10, na.rm = TRUE))
data <- data %>% mutate(gdp_mc = gdp_WDI_log10 - mean(gdp_WDI_log10, na.rm = TRUE))

clmA <-
  clm(
    as.factor(politterr_a_PTS) ~ police + gdp_mc + usaidoblige_log + polity2_P4
    + lji_LS + hasNHRI, nominal = ~ cameo_protests + pop_mc + apts_lag,
    data = data
  )
summary(clmA)

clmS <-
  clm(
    as.factor(politterr_s_PTS) ~ police + cameo_protests + polity2_P4 + 
      lji_LS, nominal = ~ gdp_mc + usaidoblige_log + spts_lag + pop_mc + hasNHRI,
    data = data
  )
summary(clmS)

## obtain McFadden's R for clm
clm0a <- clm(as.factor(politterr_a_PTS) ~ 1, data = data)
clm0s <- clm(as.factor(politterr_s_PTS) ~ 1, data = data)
McF.pr2_a <- 1 - (clmA$logLik / clm0a$logLik)
McF.pr2_s <- 1 - (clmS$logLik / clm0s$logLik)

## OLS
olsA <- lm(politterr_a_PTS ~ police + polity2_P4 + apts_lag + gdp_WDI_log10 + cameo_protests + trade_WDI + pop_WDI_log10 + 
             lji_LS, data = data)
olsS <- lm(politterr_s_PTS ~ police + polity2_P4 + spts_lag + gdp_WDI_log10 + cameo_protests + trade_WDI + pop_WDI_log10 + 
                      lji_LS, data = data)
summary(olsA)
summary(olsS)

## output all the main models as html
stargazer(olsA, olsS, olrA, olrS, clmA, clmS, type = "html", out = "DNR_PTSModels.html")

maximaldiffsA <- ordChange(olrA, data = data)
maximaldiffsA

maximaldiffsS <- ordChange(olrS, data = data)
maximaldiffsS

oc2plot(maximaldiffsA)
oc2plot(maximaldiffsS)
