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

## grab ITT
ITT <- read_tsv(file.choose())
## filter by agency of control such that police are involved; aocpolice == 1
ITT <- filter(ITT, aocpolice == 1)
## ITT lists cowcode as "cowccode"; correct this for merger
ITT <- rename(ITT, cowcode = cowccode)

## obtain dataset for merger (ex. full data with CIRI components)
DNR <- read_csv(file.choose())
data <- merge(ITT, DNR, by = c("cowcode", "year"))

## remove duplicate columns
data <- data %>% select(unique(colnames(.)))

## ensure distinct rows
data <- distinct(data)

## lag variables of interest to prepare for logits
data <- data %>% mutate(scarring_lagged = lag(scarring))
data <- data %>% mutate(stealth_lagged = lag(stealth))
data <- data %>% mutate(illtreatment_lagged = lag(illtreatment))
data <- data %>% mutate(unknown_lagged = lag(unknown))

## logits; glm family binomial
logitStealth <- glm(stealth ~ police + polity2_P4 + stealth_lagged + gdp_WDI_log10 + cameo_protests + usaidoblige_log + hasNHRI + pop_WDI_log10 + lji_LS, family = "binomial", data = data)
logitScar <- glm(scarring ~ police + polity2_P4 + scarring_lagged + gdp_WDI_log10 + cameo_protests + usaidoblige_log + hasNHRI + pop_WDI_log10 + lji_LS, family = "binomial", data = data)
logitIll <- glm(illtreatment ~ police + polity2_P4 + illtreatment_lagged + gdp_WDI_log10 + cameo_protests + usaidoblige_log + hasNHRI + pop_WDI_log10 + lji_LS, family = "binomial", data = data)
logitUnstated <- glm(unknown ~ police + polity2_P4 + unknown_lagged + gdp_WDI_log10 + cameo_protests + usaidoblige_log + hasNHRI + pop_WDI_log10 + lji_LS, family = "binomial", data = data)
stargazer(logitStealth, logitScar, logitIll, logitUnstated, type = "text")
stargazer(logitStealth, logitScar, logitIll, logitUnstated, type = "html", out = "DPMIR ITT Logits.html")

write_csv(data, "DPMIR ITT Merged.csv")

