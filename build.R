# Clear workspace
rm(list=ls())

# load packages
library("devtools")
library("roxygen2")

# document
devtools::document()

# for later documentation
use_vignette('introduction')
