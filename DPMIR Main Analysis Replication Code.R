## MARTIN STAVRO
## 8.16.22
## DPMIR Main Analyses
rm(list=ls())
packages <- c("jmv", "readr", "MASS", "dplyr", "brant", "ordinal", "dotwhisker", "stargazer", "ggthemes", "googleVis", "DAMisc", "lattice", "stringr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())), repos="https://cran.rstudio.com/")
}

lapply(packages, library, character.only = T)
options(stringsAsFactors=FALSE)
options(scipen=999)
set.seed(33603)

## select the DPMIR Main Data file from 
## GitHub repo/zip folder
data <- read_csv(file.choose())

## how many countries and what period of time?
## data2 <- data %>% select(police, repress_index_lagged, hasNHRI, cameo_protests, pop_WDI_log10, polity2_P4, gdp_WDI_log10, lji_LS, ifs, cowcode, year, country.x)
## data3 <- na.omit(data2)
## n_distinct(data3$ifs)
## range(data3$year)

## choose csv called "geocoding"
## geocoding <- read_csv(file.choose())
## G1 <- gvisGeoChart(geocoding, "geocode", "police")
## plot(G1)

## ordered logit
olr1 <- polr(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + hasNHRI + repress_index_lagged + polity2_P4 + pop_WDI_log10 + lji_LS, data = data, method = "logistic")
summary(olr1)

## check the first model with just police
olr2 <- polr(as.factor(repress_index) ~ police, data = data, method = "logistic")
summary(olr2)

## produce stargazer tables
write(stargazer(olr2, olr1, type = "html"), "OrdinalTablesDPMIR.html")

## assumption test: parallel regression (proportional odds) Brant Test
brant(olr1)

## parallel regression / proportional odds assumption violated
## test on clm() package as a generalized ordinal logit / partial
## proportional odds model
clm1 <- clm(as.factor(repress_index) ~ gdp_WDI_log10 + cameo_protests + repress_index_lagged + polity2_P4 + pop_WDI_log10 + hasNHRI + lji_LS, nominal = ~ police, data = data)
clm1$convergence

## obtain McFadden's R for clm
clm0 <- clm(as.factor(repress_index) ~ 1, data = data)
McF.pr2 <- 1 - (clm1$logLik / clm0$logLik)

## plot the comparison of partial proportional odds
write(stargazer(olr2, olr1, clm1, type = "html"), "DPMIR Ordinal CLM Compare.html")

## report ONLY the odds ratios
stargazer(olr2, olr1, clm1, type = "html", apply.coef = exp, report = "vc", omit.table.layout = "n", out = "stargazer_oddsratios.html")

## linear regression
linear_model0 <- lm(repress_index ~ police, data = data)
linear_model1 <- lm(repress_index ~ police + polity2_P4 + repress_index_lagged + gdp_WDI_log10 + cameo_protests + hasNHRI + pop_WDI_log10 + 
                      lji_LS, data = data)
summary(linear_model0)
summary(linear_model1)

## output regression table as HTML
write(stargazer(linear_model0, linear_model1, type = "html"), "PMRepressionLinReg.html")

## output all the main models as html
colabs <- c("Police militarization", "Number of protests", "Polity IV score", "Latent judicial independence", "NHRI presence", "Log GDP (constant 2010 USD)", "Log population", "Repression t-1")
output <- stargazer(olr1, clm1, linear_model1, order = c(1, 3, 6, 8, 4, 2, 7, 5), type = "html", covariate.labels = colabs, dep.var.labels = c("Repression Index (Inverted CIRI Physical Integrity Index)", ""))
output <- str_replace(output, "Repression t-1", "Repression<sub> t-1</sub>")
write_lines(output, "DPMIR_MainModels.html")
## create corresponding odds ratio table for above
output <- stargazer(olr1, clm1, type = "html", apply.coef = exp, report = "vc", omit.table.layout = "n", order = c(1, 3, 6, 8, 4, 2, 7, 5), covariate.labels = colabs, dep.var.labels = "Repression Index (Inverted CIRI)")
output <- str_replace(output, "Repression t-1", "Repression<sub> t-1</sub>")
write_lines(output, "DPMIRMainModels_Odds.html")

## present uncontrolled models for appendix
stargazer(olr2, linear_model0, type = "html", out = "DPMIR_NoControls.html", covariate.labels = "Police militarization")

## violin plot to open up the analysis discussion section
theme_custom <- function (base_size = 12, color = "white", base_family = "sans", 
                          title_family = "sans") 
{
  colorhex <- ggthemes::ggthemes_data$wsj$bg[color]
  theme_foundation(base_size = base_size, base_family = base_family) + 
    theme(line = element_line(linetype = 1, colour = "black"), 
          rect = element_rect(fill = colorhex, linetype = 0, 
                              colour = NA), text = element_text(colour = "black"), 
          title = element_text(family = title_family, size = rel(1.03)), 
          axis.title = element_blank(), axis.text = element_text( 
            size = rel(1)), axis.text.x = element_text(colour = NULL), 
          axis.text.y = element_text(colour = NULL), axis.ticks = element_line(colour = NULL), 
          axis.ticks.y = element_blank(), axis.ticks.x = element_line(colour = NULL), 
          axis.line = element_line(), axis.line.y = element_blank(), 
          legend.background = element_rect(), legend.position = "top", 
          legend.direction = "horizontal", legend.box = "vertical", 
          panel.grid = element_line(colour = NULL, linetype = 3), 
          panel.grid.major = element_line(colour = "black"), 
          panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), 
          plot.title = element_text(hjust = 0, face = "bold"), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"), 
          strip.background = element_rect())
}
ggplot(data = data, aes(x = police, y = repress_index, group = police)) + geom_violin(orientation = "x") + scale_x_continuous(breaks = c(0,1), labels = c("No Police Militarization", "Police Militarization")) + scale_y_continuous(breaks = c(0:8)) + xlab("Police Militarization") + ylab("Repression Index (Inverted CIRI Index)") + stat_summary(fun = "median", geom = "point") + theme_custom() + theme(axis.title.y = element_text(vjust = 3))

## diagnostic plots lin reg
plot(linear_model1)

## for presenting ordinal regression results need maximal first differences
maximaldiffs <- ordChange(olr1, data = data)
maximaldiffs

## oc2plot will print the plot incorrectly as it starts the X scale at 1
## however, the DV starts at 0
## below is a rewritten version of oc2plot function to correct for the issue
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

oc2plotadjusted(maximaldiffs)
maxdiffs_tibble <- as_tibble(oc2plot(maximaldiffs, plot = FALSE))
maxdiffs_tibble <- maxdiffs_tibble %>% arrange(var)
