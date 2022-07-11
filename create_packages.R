library(devtools)
library(roxygen2)
getwd()
setwd("./packages/")
#devtools::create("footballFunctions")
setwd("footballFunctions")
setwd("themes")
devtools::document()
#devtools::install_github('RobinKoetsier/xRank/themes')
library(themes)
setwd('~/Documents/R_with_git/Football/')


