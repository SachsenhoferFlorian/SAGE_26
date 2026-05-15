# CLEAR ENVIRONMENT-------------------------------------------------------------
rm(list = ls())

#SOURCING THE SCRIPTS---------
source("R/packages.R")
source("R/01load_manip.R")
source("R/02descriptive_stat.R")
source("R/02aDiversity.R")
source("R/03MCA_Clustering.R")
source("R/03aUsage.R")
source("R/04Allometric.R")
source("R/04aDensity.R")


source("R/03.1MCA_Clustering.R")   #clustering without plant form and branching
source("R/03.2MCA_Clustering.R")   #clustering with kramanioc as pre-defined cluster
source("R/03.3FAMD_Clustering.R")  #clustering with colours as continous variables
source("R/AHC_kmeans.R")           #test of kmeans and AHC
