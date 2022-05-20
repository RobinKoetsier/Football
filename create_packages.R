library(devtools)
library(roxygen2)

create("themes")
setwd("./packages/themes")
devtools::document()
devtools::install_github('RobinKoetsier/xRank/themes')
library(themes)
