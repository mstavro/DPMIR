## MARTIN STAVRO
## 6.23.22
## DNR DISAGGREGATION BY CIRI COMPONENT
rm(list=ls())
packages <- c("jmv", "readr", "MASS", "dplyr", "brant", "ordinal", "dotwhisker", "stargazer", "ggthemes", "googleVis", "DAMisc", "lattice", "DescTools", "stringr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())), repos="https://cran.rstudio.com/")
}

lapply(packages, library, character.only = T)
options(stringsAsFactors=FALSE)
options(scipen=999)
set.seed(33603)

## select DPMIR ITT data
data <- read_csv(file.choose())

## commented out below is the merger procedure
## ITT <- read_tsv(file.choose())
## filter by agency of control such that police are involved; aocpolice == 1
## ITT <- filter(ITT, aocpolice == 1)
## ITT lists cowcode as "cowccode"; correct this for merger
## ITT <- rename(ITT, cowcode = cowccode)

## obtain dataset for merger (ex. full data with CIRI components)
## DNR <- read_csv(file.choose())
## data <- merge(ITT, DNR, by = c("cowcode", "year"))

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
logitStealth <- glm(stealth ~ police + polity2_P4 + stealth_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + lji_LS, family = "binomial", data = data)
logitScar <- glm(scarring ~ police + polity2_P4 + scarring_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + lji_LS, family = "binomial", data = data)
logitIll <- glm(illtreatment ~ police + polity2_P4 + illtreatment_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + lji_LS, family = "binomial", data = data)
logitUnstated <- glm(unknown ~ police + polity2_P4 + unknown_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + lji_LS, family = "binomial", data = data)
stargazer(logitStealth, logitScar, logitIll, logitUnstated, type = "text")
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Stealth torture t-1", "Scarring torture t-1", "Ill-treatment t-1", "Unstated torture t-1")
output <- stargazer(logitStealth, logitScar, logitIll, logitUnstated, type = "html", order = c(1,8,2,11,9,7,10,3,4,5,6), covariate.labels = colabs, dep.var.labels = c("Stealth Torture", "Scarring Torture", "Ill-Treatment", "Unstated Torture"))
output <- str_replace(output, "Stealth torture t-1", "Stealth torture<sub> t-1</sub>")
output <- str_replace(output, "Scarring torture t-1", "Scarring torture<sub> t-1</sub>")
output <- str_replace(output, "Ill-treatment t-1", "Ill-treatment<sub> t-1</sub>")
output <- str_replace(output, "Unstated torture t-1", "Unstated torture<sub> t-1</sub>")

write_lines(output, "DPMIR ITT Logits.html")

