#Joining with variete

suivi <- suivi %>% left_join(variete, by = c("Code_Var" = "Code_var"))

#Calculations--------------
suivi <- suivi %>% mutate(NLrat0 = N0/L0)
suivi <- suivi %>% mutate(NLrat1 = N1/L1)
suivi <- suivi %>% mutate(LLrat= L1/L0)
suivi <- suivi %>% mutate(NNrat= N1/N0)
suivi <- suivi %>% mutate(DDrat= D1/D0)
suivi <- suivi %>% mutate(LDrat0= L0/D0)
suivi <- suivi %>% mutate(LDrat1= L1/D1)
suivi <- suivi %>% mutate(HI= PR/(PR+PB))
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
         tl.srt = 50,
         addCoef.col = "black",
         addCoefasPercent = TRUE) 


#Modelling Yield Prediction------------------------------

#Variables corrplot
mod_corr <- lm(PR ~ H + D0 + L1 + D1,suivi)
summary(mod_corr)

mod_1 <- lm(PR ~   D0+ L1  + D1,suivi)
summary(mod_1)

mod_1.1 <- lm(PR ~ D0 + D1,suivi)
summary(mod_1.1)

mod_2 <- lm(PR ~ D1,suivi)
summary(mod_2)


#All measured variables
mod_full <- lm(PR ~ H + L0 + L1  + N0 + D0 + D1 + N1,suivi)
summary(mod_full)

#backward selection

mod_back <- lm(log(PR) ~  D1 ,suivi)
summary(mod_back)

student_res <- rstudent(mod_back)
ggplot(data = data.frame(Fitted = fitted(mod_back), Resid = student_res),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")




#All measured variables and severity 
mod_full <- lm(PR ~ H + L0 + L1  + N0 + D0 + D1 + N1 + Severite,suivi)
summary(mod_full)

#backward selection (Severity set fix as correction)
mod_full <- lm(PR ~  D1 + Severite,suivi)
summary(mod_full)

#Modelling Biomass-----------------------------------
mod_full <- lm(PB ~ H + L0 + L1  + N0 + D0 + D1 + N1,suivi)
summary(mod_full)

mod_back_PB <- lm(PB ~ I(L1^2) +L1  + I(N0^2) +N0  + D1 + N1,suivi)
summary(mod_back_PB)

mod_back_PB <- lm(log(PB) ~ L1  +N0  + D1 + N1,suivi)
summary(mod_back_PB)

student_res <- rstudent(mod_back_PB)
ggplot(data = data.frame(Fitted = fitted(mod_back_PB), Resid = student_res),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")




#Modelling Harvest Index------------------------
mod_a <- lm(HI ~  NLrat0 + NLrat1 + LLrat  + NNrat + DDrat + LDrat0 + LDrat1 ,suivi)
summary(mod_a)
