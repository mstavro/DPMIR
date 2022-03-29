#check if packages are installed, and if not, install them
packages <- c("haven", "ggplot2", "dplyr", "ggthemes", "readr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos="http://cran.rstudio.com/")  
}
lapply(packages, library, character.only = T)
## when prompted select the file location of parsed data
data <- read_csv(file.choose())
## creates a time series plot of avg. police militarization internationally
data %>% ggplot(aes(x = year, y = repress_index)) + stat_summary(fun.data = "mean_cl_normal", geom = "smooth", color = "black") + xlab("Year") + ylab("Average Repression Level") + theme_clean() + geom_vline(xintercept = 1994)
pre1994 <- data %>% filter(year < 1994)
mean(pre1994$repress_index, na.rm = TRUE)
post1994 <- data %>% filter(year >= 1994)
mean(post1994$repress_index, na.rm = TRUE)