library(devtools)
library(roxygen2)

create("themes")
setwd("./themes")
document()
devtools::install_github('RobinKoetsier/xRank/themes')
library(themes)
