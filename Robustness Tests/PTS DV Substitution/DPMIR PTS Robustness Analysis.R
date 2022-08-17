## MARTIN STAVRO
## 8.16.22
## DPMIR PTS Robustness Check

rm(list=ls())
packages <- c("jmv", "readr", "MASS", "dplyr", "brant", "car", "ordinal", "dotwhisker", "stargazer", "ggthemes", "googleVis", "DAMisc", "lattice", "stringr")
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

## results in nonconvergence without mean centering of GDP and pop
data <- data %>% mutate(pop_mc = pop_WDI_log10 - mean(pop_WDI_log10, na.rm = TRUE))
data <- data %>% mutate(gdp_mc = gdp_WDI_log10 - mean(gdp_WDI_log10, na.rm = TRUE))

## ordered logit with amnesty PTS
olrA <- polr(as.factor(politterr_a_PTS) ~ police + gdp_mc + cameo_protests + apts_lag + polity2_P4 + pop_mc + lji_LS + hasNHRI, data = data, method = "logistic")
summary(olrA)

## ordered logit state PTS
olrS <- polr(as.factor(politterr_s_PTS) ~ police + gdp_mc + cameo_protests + hasNHRI + spts_lag + polity2_P4 + pop_mc + lji_LS, data = data, method = "logistic")
summary(olrS)

## assumption test: parallel regression (proportional odds) Brant Test
brant(olrA)
brant(olrS)

## parallel regression / proportional odds assumption violated
## test on clm() package as a generalized ordinal logit / partial
## proportional odds model

clmA <-
  clm(
    as.factor(politterr_a_PTS) ~ police + gdp_mc + polity2_P4
    + lji_LS + hasNHRI, nominal = ~ cameo_protests + pop_mc + apts_lag,
    data = data
  )
summary(clmA)

clmS <-
  clm(
    as.factor(politterr_s_PTS) ~ police + cameo_protests + polity2_P4 + 
      lji_LS, nominal = ~ gdp_mc + spts_lag + pop_mc + hasNHRI,
    data = data
  )
summary(clmS)

## obtain McFadden's R for clm
clm0a <- clm(as.factor(politterr_a_PTS) ~ 1, data = data)
clm0s <- clm(as.factor(politterr_s_PTS) ~ 1, data = data)
McF.pr2_a <- 1 - (clmA$logLik / clm0a$logLik)
McF.pr2_s <- 1 - (clmS$logLik / clm0s$logLik)

## OLS
olsA <- lm(politterr_a_PTS ~ police + polity2_P4 + apts_lag + gdp_mc + cameo_protests + hasNHRI + pop_mc + 
             lji_LS, data = data)
olsS <- lm(politterr_s_PTS ~ police + polity2_P4 + spts_lag + gdp_mc + cameo_protests + hasNHRI + pop_mc + 
                      lji_LS, data = data)
summary(olsA)
summary(olsS)

## output all the main models as html
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Amnesty t-1", "State t-1")
output <- stargazer(olrA, olrS, clmA, clmS, olsA, olsS, type = "html", order = c(1, 3, 5, 7, 8, 2, 6, 4, 9), covariate.labels = colabs, dep.var.labels = c("Amnesty", "State", "Amnesty", "State", "Amnesty", "State"))
output <- str_replace(output, "Amnesty t-1", "Amnesty<sub> t-1</sub>")
output <- str_replace(output, "State t-1", "State<sub> t-1</sub>")
write_lines(output, "DPMIR PTS.html")

maximaldiffsA <- ordChange(olrA, data = data)
maximaldiffsA

maximaldiffsS <- ordChange(olrS, data = data)
maximaldiffsS

oc2plot(maximaldiffsA)
oc2plot(maximaldiffsS)
