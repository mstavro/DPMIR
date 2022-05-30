## subpolice robustness check
## run the logit on countries only with centralized police control

rm(list=ls())
packages <- c("readr", "MASS", "dplyr", "brant", "ordinal", "stargazer", "DAMisc", "lattice")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())), repos="https://cran.rstudio.com/")
}

lapply(packages, library, character.only = T)
options(stringsAsFactors=FALSE)
options(scipen=999)
set.seed(33603)

## load subpolice and filter s.t. all cases on subpolice = 0
## subpolice 0 = full centralized authority over police
subpol <- read_csv(file.choose())
subpol <- subpol %>% filter(subpolice_IDC == 0)

olr1 <- polr(as.factor(repress_index) ~ police + gdp_WDI_log10 + cameo_protests + trade_WDI + repress_index_lagged + polity2_P4 + pop_WDI_log10 + lji_LS, data = subpol, method = "logistic")
summary(olr1)
olr2 <- polr(as.factor(repress_index) ~ police, data = subpol, method = "logistic")
summary(olr2)
stargazer(olr2, olr1, out = "Robustness Test Using Centralized Police Only.html")

maximaldiffs <- ordChange(olr1, data = subpol)
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