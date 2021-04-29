#setup the package and working directories 

wd <- list()
setwd('~/NAPS_project/NAPS_wrangling/')

wd$data   <- paste0(getwd(),"/data/")
wd$output   <- paste0(getwd(),"/output/")
wd$header   <- paste0(getwd(),"/header_files/")