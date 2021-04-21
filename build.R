# Clear workspace
rm(list=ls())

# load packages
library("devtools")
library("roxygen2")

# document
devtools::document()
devtools::document()
devtools::check(args = "--no-examples")


# for later documentation
use_vignette('introduction')
