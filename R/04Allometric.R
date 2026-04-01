#Descriptive Analysis---------------------------------
suivi_numeric <- suivi[sapply(suivi, is.numeric)]

summary(suivi_numeric)
describe(suivi_numeric)

boxplot(select(suivi_numeric, H, L0, L1))
boxplot(select(suivi_numeric, N0, N1))
boxplot(select(suivi_numeric, B0,B1, NR))
boxplot(select(suivi_numeric, D0, D1))
boxplot(select(suivi_numeric, PB, PR))

s_cor_mat <- cor(suivi_numeric, use = "pairwise.complete.obs")
s_cor_mat
corrplot(s_cor_mat,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 0,
         addCoef.col = "black",
         addCoefasPercent = TRUE) 
