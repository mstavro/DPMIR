#### MARTIN STAVRO, DR. RYAN WELCH ####
## Replication Code for "Does Police Militarization Increase Repression?"
## Submitted to the Journal of Conflict Resolution
## 6/9/2023

#### LOAD REQUIRED PACKAGES AND SET CONFIG. OPTIONS ####
if(!require(pacman)) install.packages("pacman")
pacman::p_load("jmv", "readr", "MASS", "dplyr", "brant", "ordinal", "dotwhisker", "stargazer", "ggthemes", "googleVis", "DAMisc", "lattice", "stringr", "Hmisc")

options(stringsAsFactors = FALSE)
options(scipen = 999)
set.seed(33603)

#### LOAD AN AESTHETIC THEME FOR PLOTTING ####
theme_lightmode <- function (base_size = 12, color = "white", base_family = "sans", 
                             title_family = "sans") 
{
  colorhex <- theme_foundation(base_size = base_size, base_family = base_family) + 
    theme(line = element_line(linetype = 1, colour = "#0a0a0a"), 
          rect = element_rect(fill = "#ffffff", linetype = 0, 
                              colour = "#0a0a0a"), text = element_text(colour = "#0a0a0a"), 
          title = element_text(family = title_family, size = rel(1.03), color = "#0a0a0a"), 
          axis.line = element_line(), axis.line.y = element_blank(), 
          legend.background = element_rect(), legend.position = "top", 
          legend.direction = "horizontal", legend.box = "vertical", 
          panel.grid = element_line(colour = NULL, linetype = 3), 
          panel.grid.major = element_line(colour = "#0a0a0a"), 
          panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), 
          plot.title = element_text(hjust = 0, face = "bold"), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"), 
          strip.background = element_rect())
}
theme_set(theme_lightmode())

#### LOAD MAIN DATASET BY PULLING FROM GITHUB REPO ####
data_main <- read_csv(file.choose())

##### NO REPLICATION FOR FIGURE 1 - JUST A GRAPHIC, CONTAINS NO DATA #####

#### REPLICATE FIGURE 2: "GLOBAL POLICE MILITARIZATION OVER TIME, 1965-2010" ####
## Take the main data and get the mean of PM in each year
global_mean <- data_main %>% group_by(year) %>% summarize(mean_PM = mean(police))
outputF2 <- ggplot(global_mean, aes(year, global_PM_mean)) + geom_smooth(se = FALSE, color = "#78a9ff") + ylab("Proportion of Countries with Militarized Police\n") + xlab("Year") + theme_lightmode() + theme(axis.text.x = element_text(size = rel(1.3)), axis.text.y = element_text(size = rel(1.3)), axis.title.y = element_text(size = rel(1.3)), axis.title.x = element_text(size = rel(1.3)))
outputF2

#### REPLICATE FIGURE 3: "VIOLIN PLOT OF REPRESSION INDEX BY POLICE MILITARIZATION" ####
outputF3 <- ggplot(data = data_main, aes(x = police, y = repress_index, group = police)) + geom_violin(orientation = "x") + scale_x_continuous(breaks = c(0,1), labels = c("No Police Militarization", "Police Militarization")) + scale_y_continuous(breaks = c(0:8)) + xlab("Police Militarization") + ylab("Repression Index (Inverted CIRI Index)") + stat_summary(fun = "median", geom = "point") + theme_custom() + theme(axis.title.y = element_text(vjust = 3))
outputF3

#### REPLICATE TABLE 1: "ANALYSIS OF REPRESSION OUTCOMES" ####
## Develop a linear regression and store the results
linear_model1 <- lm(repress_index ~ police + polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
                      lji_LS, data = data_main)
## Develop an ordered logit and store the results
olr1 <- polr(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + repress_index_lagged + polity2_P4 + pop_WDI_log10 + lji_LS, data = data_main, method = "logistic")
## Check the ordered logit for robustness using the Brant test
brant(olr1)
## Develop a partial proportional odds model for robustness informed by Brant results
clm1 <- clm(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + repress_index_lagged + polity2_P4 + pop_WDI_log10 + hasNHRI, nominal = ~ lji_LS, data = data_main)
## Replicate the table's presentation using stargazer
## "colabs" is a temporary variable to set the labels - it will be overwritten, but tables will be saved
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Repression t-1")
outputT1 <- stargazer(olr1, clm1, linear_model1, order = c(1, 3, 6, 8, 4, 2, 7, 5), type = "text", covariate.labels = colabs, dep.var.labels = c("Repression Index (Inverted CIRI Physical Integrity Index)", ""))
### If you want an HTML table (exactly how the table appears in the paper) saved to your working directory:
#### 1) Replace type = "text" with type = "html" in the stargazer command above
#### 2) Uncomment and run the following 2 lines by deleting "##" and using CTRL + ENTER
## outputT1 <- str_replace(outputT1, "Repression t-1", "Repression<sub> t-1</sub>")
## write_lines(outputT1, "DPMIR_MainModels.html")

#### REPLICATE TABLE 2: "ODDS RATIOS OF ORDERED LOGIT ANALYSIS" ####
outputT2 <- stargazer(olr1, clm1, type = "text", apply.coef = exp, report = "vc", omit.table.layout = "n", order = c(1, 3, 6, 8, 4, 2, 7, 5), covariate.labels = colabs, dep.var.labels = "Repression Index (Inverted CIRI)")
### If you want an HTML table (exactly how the table appears in the paper) saved to your working directory:
#### 1) Replace type = "text" with type = "html" in the stargazer command above
#### 2) Uncomment and run the following 2 lines by deleting "##" and using CTRL + ENTER
## outputT2 <- str_replace(outputT2, "Repression t-1", "Repression<sub> t-1</sub>")
## write_lines(outputT2, "DPMIRMainModels_Odds.html")

#### REPLICATE TABLE 3: "FIRST DIFFERENCES IN PREDICTED PROBABILITIES FOR REPRESSION LEVEL DUE TO POLICE MILITARIZATION" ####
## Compute the first differences
maximaldiffs <- ordChange(olr1, data = data_main)
## Construct table using as_tibble
outputT3 <- as_tibble(oc2plot(maximaldiffs, plot = FALSE))
outputT3 <- outputT3 %>% filter(var == "police")
outputT3

#### REPLICATE TABLE 4: "ANALYSIS OF CIRI TACTIC OUTCOMES" ####
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Disappearance t-1", "Political imprisonment t-1", "Extrajudicial killing t-1", "Torture t-1")
## Run an ordered logit on each CIRI tactic
olrK <- polr(as.factor(kill) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + kill_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data_main, method = "logistic")
olrT <- polr(as.factor(tort) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + tort_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data_main, method = "logistic")
olrD <- polr(as.factor(disap) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + disap_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data_main, method = "logistic")
olrP <- polr(as.factor(polpris) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + polpris_lag + polity2_P4 + pop_WDI_log10 + lji_LS, data = data_main, method = "logistic")
## Create table
outputT4 <- stargazer(olrD, olrP, olrK, olrT, type = "text", order = c(1, 3, 9, 11, 4, 2, 10, 5, 6, 7, 8), covariate.labels = colabs, dep.var.labels = c("Disappearance", "Political Imprisonment", "Extrajudicial Killing", "Torture"))
### If you want an HTML table (exactly how the table appears in the paper) saved to your working directory:
#### 1) Replace type = "text" with type = "html" in the stargazer command above
#### 2) Uncomment and run the following 5 lines by deleting "##" and using CTRL + ENTER
## outputT4 <- str_replace(outputT4, "Disappearance t-1", "Disappearance<sub> t-1</sub>")
## outputT4 <- str_replace(outputT4, "Political imprisonment t-1", "Political imprisonment<sub> t-1</sub>")
## outputT4 <- str_replace(outputT4, "Extrajudicial killing t-1", "Extrajudicial killing<sub> t-1</sub>")
## outputT4 <- str_replace(outputT4, "Torture t-1", "Torture<sub> t-1</sub>")
## write_lines(outputT4, "DPMIR CIRI Components OLR.html")

#### REPLICATE TABLE 5: "REPRESSION'S INFLUENCE ON NEW POLICE MILITARIZATION" ####
## Compute six binomial models to control for presence of repression, previous repression, and controls 
## (2!3 take away the 2 scenarios where explanatory vars are not tested)
glm1 <- glm(newpolice ~ repress_index + polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
              lji_LS, data = data, family = "binomial")
glm2 <- glm(newpolice ~ repress_index, data = data, family = "binomial")
glm3 <- glm(newpolice ~ repress_index + repress_index_lagged, data = data, family = "binomial")
glm4 <- glm(newpolice ~ repress_index_lagged, data = data, family = "binomial")
glm5 <- glm(newpolice ~ polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
              lji_LS, data = data, family = "binomial")
glm6 <- glm(newpolice ~ repress_index + polity2_P4 + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
              lji_LS, data = data, family = "binomial")
colabs2 <- c("Repression", "Repression t-1", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population")
outputT5 <- stargazer(glm1, glm6, glm3, glm2, glm4, glm5, order = c(1,3,5,2,8,6,4,7), covariate.labels = colabs2, dep.var.labels = "Newly Militarized Police", type = "html")
### If you want an HTML table (exactly how the table appears in the paper) saved to your working directory:
#### 1) Replace type = "text" with type = "html" in the stargazer command above
#### 2) Uncomment and run the following 2 lines by deleting "##" and using CTRL + ENTER
## output2 <- str_replace(outputT5, "Repression t-1", "Repression<sub> t-1</sub>")
## write_lines(outputT5, "DPMIR_NewMilitariz_EndoTest.html")

#### REPLICATE TABLE A1: "DESCRIPTIVE STATISTICS" ####
outputTA1 <- descriptives(
  data = data_main,
  vars = vars(repress_index, police, cameo_protests, polity2_P4, lji_LS, hasNHRI, gdp_WDI_log10, pop_WDI_log10),
  desc = "rows")
outputTA1

#### REPLICATE TABLE A2: "FREQUENCY COUNT OF POLICE MILITARIZATION" ####
outputTA2 <- descriptives(
  data = data_main,
  vars = police,
  freq = T)
outputTA2

#### REPLICATE FIGURE A1: "BAR PLOT OF POLICE MILITARIZATION" ####
outputFA1 <- ggplot(data_main, aes(x = police)) + geom_bar(fill = "#BBDAA5", color = "black") + scale_x_continuous(breaks = c(0,1), labels = c(0,1)) + xlab("police") + ylab("counts") + theme_lightmode()
outputFA1

#### REPLICATE TABLE A3: "FREQUENCY COUNT OF REPRESSION INDEX" ####
outputTA3 <- descriptives(
  data = data_main,
  vars = repress_index,
  freq = T)
outputTA3

#### REPLICATE FIGURE A2: "BAR PLOT OF REPRESSION INDEX" ####
outputFA2 <- ggplot(data_main, aes(x = repress_index)) + geom_bar(fill = "#BBDAA5", color = "black") + scale_x_continuous(breaks = 0:8, labels = 0:8) + xlab("repress_index") + ylab("counts") + theme_lightmode()
outputFA2

#### REPLICATE FIGURE A3: "MAP OF COUNTRIES REPRESENTED IN DATA ANALYSIS" ####
## Load a modified version of the data from GitHub repo with geocoded countries readable by googleVis
## The geocoded file is the same as the main file, with an additional variable using 
## Microsoft Excel's "Geography" field to standardize location names.
geocoded <- read_csv(file.choose())
outputFA3 <- gvisGeoChart(geocoding, "geocode", "police")
plot(outputFA3)

#### REPLICATE FIGURE A4: "TIME SERIES PLOT FOR TEMPORAL BIAS" ####
outputFA4 <- data_main %>% ggplot(aes(x = year, y = repress_index)) + stat_summary(fun.data = "mean_cl_normal", geom = "smooth", color = "black") + xlab("Year") + ylab("Average Repression Level") + theme_clean() + geom_vline(xintercept = 1994)
outputFA4

#### REPLICATE FIGURE A5: "PLOT OF FIRST DIFFERENCES IN PREDICTED PROBABILITIES FOR REPRESSION LEVEL" ####
## Modify the oc2plot function to correct for scaling starting at 1 instead of 0
oc2plotadjusted <- function (ordc, plot = TRUE) 
{
  tmpdat <- data.frame(var = rep(rownames(ordc$diffs$mean), 
                                 ncol(ordc$diffs$mean)), lev = rep(colnames(ordc$diffs$mean), 
                                                                   each = nrow(ordc$diffs$mean)), mean = c(ordc$diffs$mean), 
                       lower = c(ordc$diffs$lower), upper = c(ordc$diffs$upper), 
                       stringsAsFactors = TRUE)
  p1 <- xyplot(mean ~ lev | var, data = tmpdat, xlab = "", 
               ylab = "Predicted Change in Pr(y=m)", lower = tmpdat$lower, 
               upper = tmpdat$upper, panel = function(x, y, subscripts, 
                                                      lower, upper, ...) {
                 panel.abline(h = 0, lty = 2)
                 panel.arrows(x, lower[subscripts], x, upper[subscripts], 
                              angle = 90, length = 0.05, code = 3)
                 panel.points(x, y, pch = 16, cex = 0.75, col = "black")
               }, prepanel = prepanel.ci, scales=list(x=list(labels=c(1,1,3,5,7))))
  if (plot) {
    return(p1)
  }
  else {
    return(tmpdat)
  }
}
outputFA5 <- oc2plotadjusted(maximaldiffs)
outputFA5

#### REPLICATE TABLE A4: "UNCONTROLLED ORDERED LOGIT AND LINEAR REGRESSION OF POLICE MILITARIZATION AND REPRESSION" ####
outputTA4 <- stargazer(olr2, linear_model0, type = "text", covariate.labels = "Police militarization")

#### REPLICATE TABLE A5: "POLICE MILITARIZATION AND REPRESSION UNDER CENTRALIZED CONTROL" ####
## Create a copy of the data that filters for only centralized control of police (no federalism)
data_TA5 <- data_main %>% filter(subpolice_IDC == 0)
## Mean center population and GDP for analysis
data_TA5 <- data_TA5 %>% mutate(pop_mc = pop_WDI_log10 - mean(pop_WDI_log10, na.rm = T))
data_TA5 <- data_TA5 %>% mutate(gdp_mc = gdp_WDI_log10 - mean(gdp_WDI_log10, na.rm = T))
## Compute ordered logit, linear regression
olrTA5 <- polr(as.factor(repress_index) ~ police + gdp_mc + cameo_protests + hasNHRI + repress_index_lagged + polity2_P4 + pop_mc + lji_LS, data = data_TA5, method = "logistic")
lmTA5 <- lm(repress_index ~ police + polity2_P4 + repress_index_lagged + gdp_mc + cameo_protests + hasNHRI + pop_mc +
            lji_LS, data = data_TA5)
## Brant test ordered logit
brant(olrTA5)
## Develop partial proportional odds models using Brant results
clmTA5 <- clm(as.factor(repress_index) ~ police + gdp_mc + cameo_protests + hasNHRI + repress_index_lagged + polity2_P4, nominal = ~ pop_mc + lji_LS, data = data_TA5)
## Plot table
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Repression t-1")
outputTA5 <- stargazer(olrTA5, clmTA5, lmTA5, type = "text", order = c(1, 3, 6, 8, 4, 2, 7, 5), covariate.labels = colabs, dep.var.labels = c("Repression Index (Inverted CIRI)", ""))
### If you want an HTML table (exactly how the table appears in the paper) saved to your working directory:
#### 1) Replace type = "text" with type = "html" in the stargazer command above
#### 2) Uncomment and run the following 2 lines by deleting "##" and using CTRL + ENTER
## outputTA5 <- str_replace(outputTA5, "Repression t-1", "Repression<sub> t-1</sub>")
## write_lines(outputTA5, "Robustness Test Using Centralized Police Only.html")

#### REPLICATE TABLE A6: "ANALYSIS OF REPRESSION OUTCOMES WITH POLITICAL TERROR SCALE SUBSTITUTION" ####
## Lag the Amnesty and State Department PTS Variables
data_TA6 <- data_main %>% group_by(cowcode) %>% mutate(apts_lag = lag(politterr_a_PTS))
data_TA6 <- data_TA6 %>% group_by(cowcode) %>% mutate(spts_lag = lag(politterr_s_PTS))
## Mean center population and GDP for analysis
data_TA6 <- data_TA6 %>% mutate(pop_mc = pop_WDI_log10 - mean(pop_WDI_log10, na.rm = TRUE))
data_TA6 <- data_TA6 %>% mutate(gdp_mc = gdp_WDI_log10 - mean(gdp_WDI_log10, na.rm = TRUE))
## Compute ordered logit for Amnesty PTS
olrA <- polr(as.factor(politterr_a_PTS) ~ police + gdp_mc + cameo_protests + apts_lag + polity2_P4 + pop_mc + lji_LS + hasNHRI, data = data_TA6, method = "logistic")
## Compute ordered logit for State PTS
olrS <- polr(as.factor(politterr_s_PTS) ~ police + gdp_mc + cameo_protests + hasNHRI + spts_lag + polity2_P4 + pop_mc + lji_LS, data = data_TA6, method = "logistic")
## Brant tests
brant(olrA)
brant(olrS)
## Develop partial proportional odds models based on Brant test results
clmA <-
  clm(
    as.factor(politterr_a_PTS) ~ police + gdp_mc + polity2_P4
    + hasNHRI, nominal = ~ cameo_protests + pop_mc + apts_lag + lji_LS,
    data = data_TA6
  )
clmS <-
  clm(
    as.factor(politterr_s_PTS) ~ police + polity2_P4 + gdp_mc + spts_lag + pop_mc, nominal = ~ lji_LS + cameo_protests + hasNHRI,
    data = data_TA6
  )
## Compute linear regression models
olsA <- lm(politterr_a_PTS ~ police + polity2_P4 + apts_lag + gdp_mc + cameo_protests + hasNHRI + pop_mc + 
             lji_LS, data = data_TA6)
olsS <- lm(politterr_s_PTS ~ police + polity2_P4 + spts_lag + gdp_mc + cameo_protests + hasNHRI + pop_mc + 
             lji_LS, data = data_TA6)
## Create table using stargazer
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Amnesty t-1", "State t-1")
outputTA6 <- stargazer(olrA, olrS, clmA, clmS, olsA, olsS, type = "text", order = c(1, 3, 5, 7, 8, 2, 6, 4, 9), covariate.labels = colabs, dep.var.labels = c("Amnesty", "State", "Amnesty", "State", "Amnesty", "State"))
### If you want an HTML table (exactly how the table appears in the paper) saved to your working directory:
#### 1) Replace type = "text" with type = "html" in the stargazer command above
#### 2) Uncomment and run the following 3 lines by deleting "##" and using CTRL + ENTER
## outputTA6 <- str_replace(outputTA6, "Amnesty t-1", "Amnesty<sub> t-1</sub>")
## outputTA6 <- str_replace(outputTA6, "State t-1", "State<sub> t-1</sub>")
## write_lines(outputTA6, "DPMIR PTS.html")

#### REPLICATE TABLE A7: "CIRI DISAGGREGATED OUTCOMES AS PARTIAL PROPORTIONAL ODDS MODELS" ####
## Run Brant tests from models in Table 4
brant(olrK)
brant(olrT)
brant(olrD)
brant(olrP)
## olrT passes the Brant test
## Develop partial proportional odds models for the rest of the CIRI models
clmK <- clm(as.factor(kill) ~ police + cameo_protests + hasNHRI + polity2_P4 + pop_WDI_log10 + lji_LS, nominal = ~ gdp_WDI_log10 + kill_lag, data = data_main)
clmD <- clm(as.factor(disap) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10, nominal = ~ disap_lag + lji_LS + polity2_P4, data = data_main)
clmP <- clm(as.factor(polpris) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + polpris_lag + polity2_P4 + pop_WDI_log10, nominal = ~ lji_LS, data = data_main)
## Create table
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Political imprisonment t-1")
outputTA7 <- stargazer(clmD, clmP, clmK, type = "text", order = c(1, 3, 6, 8, 4, 2, 7, 5), covariate.labels = colabs, dep.var.labels = c("Disappearance", "Political Imprisonment", "Extrajudicial Killing"))
### If you want an HTML table (exactly how the table appears in the paper) saved to your working directory:
#### 1) Replace type = "text" with type = "html" in the stargazer command above
#### 2) Uncomment and run the following 3 lines by deleting "##" and using CTRL + ENTER
## outputTA7 <- str_replace(outputTA7, "Political imprisonment t-1", "Political imprisonment<sub> t-1</sub>")
## write_lines(outputTA7, "DPMIR CIRI Components CLM.html")

#### REPLICATE TABLE A8: "U.S. DATA ROBUSTNESS CHECKS"
## Pull data with the U.S. removed from the GitHub repository
data_USremoved <- read_csv(file.choose())
## Pull data with the U.S. recoded to having police militarization from the GitHub repository
data_USrecoded <- read_csv(file.choose())
## Compute ordered logits
olrRM <- polr(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + repress_index_lagged + polity2_P4 + pop_WDI_log10 + lji_LS, data = data_USremoved, method = "logistic")
olrRE <- polr(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + repress_index_lagged + polity2_P4 + pop_WDI_log10 + lji_LS, data = data_USrecoded, method = "logistic")
## Brant test for parallel regression/proportional odds assumption
brant(olrRM)
brant(olrRE)
## Develop partial proportional odds models using Brant test results
clmRM <- clm(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + repress_index_lagged + polity2_P4 + pop_WDI_log10 + hasNHRI, nominal = ~ lji_LS, data = data_USremoved)
clmRE <- clm(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + repress_index_lagged + polity2_P4 + pop_WDI_log10 + hasNHRI, nominal = ~ lji_LS, data = data_USrecoded)
## Compute linear regressions
linear_modelRM <- lm(repress_index ~ police + polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
                       lji_LS, data = data_USremoved)
linear_modelRE <- lm(repress_index ~ police + polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
                       lji_LS, data = data_USrecoded)
## Create table
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Repression t-1")
outputTA8 <- stargazer(olrRM, olrRE, clmRM, clmRE, linear_modelRM, linear_modelRE, type = "text", order = c(1, 3, 6, 8, 4, 2, 7, 5), covariate.labels = colabs, dep.var.labels = c("Repression Index (Inverted CIRI)", ""), column.labels = c("U.S. Removed", "U.S. Recoded to 1", "U.S. Removed", "U.S. Recoded to 1", "U.S. Removed", "U.S. Recoded to 1"))
### If you want an HTML table (exactly how the table appears in the paper) saved to your working directory:
#### 1) Replace type = "text" with type = "html" in the stargazer command above
#### 2) Uncomment and run the following 3 lines by deleting "##" and using CTRL + ENTER
## outputTA8 <- str_replace(outputTA8, "Repression t-1", "Repression<sub> t-1</sub>")
## write_lines(outputTA8, "DPMIR US Robustness Check.html")

#### REPLICATE TABLE A9: "PARALLEL REGRESSION VIOLATION COEFFICIENT ESTIMATES ON INVERTED CIRI LEVELS" ####
## This table was manually created using the summary information under "Threshold coefficients:"
outputTA9 <- summary(clm1)
outputTA9

#### REPLICATE TABLE A10: "FREQUENCY COUNT OF EXTRAJUDICIAL KILLING" ####
outputTA10 <- descriptives(data_main,
             vars = kill,
             freq = T)
outputTA10

#### REPLICATE TABLE A11: "FREQUENCY COUNT OF TORTURE" ####
outputTA11 <- descriptives(data_main,
             vars = tort,
             freq = T)
outputTA11

#### REPLICATE TABLE A12: "FREQUENCY COUNT OF DISAPPEARANCES" ####
outputTA12 <- descriptives(data_main,
                           vars = disap,
                           freq = T)
outputTA12

#### REPLICATE TABLE A13: "FREQUENCY COUNT OF POLITICAL IMPRISONMENT" ####
outputTA13 <- descriptives(data_main,
                           vars = polpris,
                           freq = T)
outputTA13

#### REPLICATE TABLE A14: "MEAN POLICE MILITARIZATION BY COUNTRY DURING PERIOD OF STUDY" ####
outputTA14 <- descriptives(
  formula = police ~ country.x,
  data = data_main,
  freq = F,
  desc = "rows",
  missing = FALSE,
  median = FALSE,
  sd = FALSE,
  min = FALSE,
  max = FALSE)
outputTA14

#### REPLICATE TABLE A15: "TWO-WAY FIXED EFFECTS ROBUSTNESS CHECK" ####
## Develop two-way fixed effects model
plm1 <- plm(repress_index ~ police + polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + lji_LS, data = data_main, index = c("cowcode", "year"), model = "within", effect = "twoways")
## Create table using stargazer
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Repression t-1")
outputTA15 <- stargazer(plm1, order = c(1, 5, 2, 8, 6, 4, 7, 3), type = "text", covariate.labels = colabs, dep.var.labels = c("Repression Index (Inverted CIRI Physical Integrity Index)", ""))
### If you want an HTML table (exactly how the table appears in the paper) saved to your working directory:
#### 1) Replace type = "text" with type = "html" in the stargazer command above
#### 2) Uncomment and run the following 3 lines by deleting "##" and using CTRL + ENTER
## outputTA15 <- str_replace(outputTA15, "Repression t-1", "Repression<sub> t-1</sub>")
## write_lines(outputTA15, "DPMIR_PLM.html")

#### REPLICATE TABLE A16: "IMPACT OF INTERIOR TROOPS ON REPRESSION"
## Develop linear, ordinal models with interior troops substituted for police
linear_modelINT <- lm(repress_index ~ troops + polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
                        lji_LS, data = data_main)
olrINT <- polr(as.factor(repress_index) ~ troops + gdp_WDI_log10 + cameo_protests + hasNHRI + repress_index_lagged + polity2_P4 + pop_WDI_log10 + lji_LS, data = data_main, method = "logistic")
## Brant test
brant(olrINT)
## Develop proportional odds model based on Brant test
clmINT <- clm(as.factor(repress_index) ~ troops + gdp_WDI_log10 + cameo_protests + repress_index_lagged + polity2_P4 + pop_WDI_log10 + hasNHRI, nominal = ~ lji_LS, data = data)
colabs <- c("Interior troops", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Repression t-1")
outputTA16 <- stargazer(linear_modelINT, olrINT, clmINT, type = "text", order = c(1,5,2,8,6,4,7,3), covariate.labels = colabs)
### If you want an HTML table (exactly how the table appears in the paper) saved to your working directory:
#### 1) Replace type = "text" with type = "html" in the stargazer command above
#### 2) Uncomment and run the following 3 lines by deleting "##" and using CTRL + ENTER
## outputTA16 <- str_replace(outputTA16, "Repression t-1", "Repression<sub> t-1</sub>")
## write_lines(outputTA16, "DPMIR TROOPS.html")

### END ###
