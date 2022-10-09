rm(list=ls())
packages <- c("readr", "dplyr", "stargazer", "stringr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())), repos="https://cran.rstudio.com/")
}

lapply(packages, library, character.only = T)
options(stringsAsFactors=FALSE)
options(scipen=999)
set.seed(33603)

## main data file from github
data <- read_csv(file.choose())

glm1 <- glm(newpolice ~ repress_index + polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
              lji_LS, data = data, family = "binomial")
glm2 <- glm(newpolice ~ repress_index, data = data, family = "binomial")
glm3 <- glm(newpolice ~ repress_index + repress_index_lagged, data = data, family = "binomial")
glm4 <- glm(newpolice ~ repress_index_lagged, data = data, family = "binomial")
glm5 <- glm(newpolice ~ polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
              lji_LS, data = data, family = "binomial")
glm6 <- glm(newpolice ~ repress_index + polity2_P4 + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
              lji_LS, data = data, family = "binomial")
colabs2 <- c("Repression", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Repression t-1")
output2 <- stargazer(glm1, glm6, glm3, glm2, glm4, glm5, order = c(1,5,2,8,6,4,7,3), covariate.labels = colabs2, dep.var.labels = "Newly Militarized Police", type = "html")
output2 <- str_replace(output2, "Repression t-1", "Repression<sub> t-1</sub>")
write_lines(output2, "DPMIR_NewMilitariz_EndoTest.html")
