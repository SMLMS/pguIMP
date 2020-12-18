# Clear workspace
rm(list=ls())

# load packages
library("devtools")
library("roxygen2")

# document
devtools::document()

# for later documentation
devtools::use_vignette('introduction')
