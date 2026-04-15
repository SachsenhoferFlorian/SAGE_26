# LOADING PACKAGES-------------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(janitor)
library(tidyr)
library(dplyr)

library(FactoMineR)
library(factoextra)
library(vegan)
library(mclust)
library(rcompanion)
library(emmeans)
library(multcomp)
library(multcompView)

library(DescTools)
library(corrplot)

library(lubridate)
library(psych)

library(proxy)
library(fastDummies)

library(ggvenn)
library(VennDiagram)
library(UpSetR)

library(lme4)
library(lmerTest)
library(see)
library(performance)


#Functions---------------------------------------

cramers_v_matrix <- function(data) {
  n <- ncol(data)
  mat <- matrix(NA, n, n)
  colnames(mat) <- colnames(data)
  rownames(mat) <- colnames(data)
  
  for (i in 1:n) {
    for (j in 1:n) {
      tbl <- table(data[[i]], data[[j]])
      mat[i, j] <- CramerV(tbl)
    }
  }
  
  return(mat)
}
