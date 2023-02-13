#installing and loading INLA

# script was written by Hedvig Skirgård and Sam Passmore

# 1. Install/update and load BiocManager and other necessary packages
source("requirements.R")

if (!is_installed("INLA")) {message("INLA wasn't installed, it is now being installed.") 

# 2. Install INLA dependencies with BiocManager using: 
BiocManager::install(c("grap","Rgraphviz","sf","rgdal","rgl","spdep"), force=TRUE)#, version = '3.15') #version indication is relevant only for R version 4.2.0
#grap package is not available for Bioconductor 3.16 (latest version)

# 3. Install INLA using: 
# NOTE: This is a big download
install.packages("INLA", repos=c(getOption("repos"), 
                                 INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

}

library(INLA)
inla.setOption(inla.mode="experimental")
