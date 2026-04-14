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


summary(suivi_numeric)
describe(suivi_numeric)

boxplot(dplyr::select(suivi_numeric, H, L0, L1))
boxplot(dplyr::select(suivi_numeric, N0, N1))
boxplot(dplyr::select(suivi_numeric, B0,B1, NR))
boxplot(dplyr::select(suivi_numeric, D0, D1))
boxplot(dplyr::select(suivi_numeric, PB, PR))

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

#All measured variables
mod_full <- lm(PR ~ H + L0 + L1  + N0 + D0 + D1 + N1,suivi)
summary(mod_full)

#after manual backward selection (p= 0.05)

mod_back <- lm(log(PR) ~  D1 ,suivi)                        # log transformation to correct student plot
summary(mod_back)


student_res <- rstudent(mod_back)
ggplot(data = data.frame(Fitted = fitted(mod_back), Resid = student_res),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")


#automatic stepwise selection
mod_step <- step(mod_full)                             #lm(formula = PR ~ L1 + N0 + D1, data = suivi)
summary(mod_step)

student_res <- rstudent(mod_step)                                                 # parabel formed pattern in plot -> log or quadratic
ggplot(data = data.frame(Fitted = fitted(mod_step), Resid = student_res),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

#log transformation

mod_full_log <- lm(log(PR) ~ H + L0 + L1  + N0 + D0 + D1 + N1,suivi)
mod_step_log <- step(mod_full_log)
summary(mod_step_log)

student_res <- rstudent(mod_step_log)
ggplot(data = data.frame(Fitted = fitted(mod_step_log), Resid = student_res),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

#quadratic model

mod_quadr <- lm(formula = PR ~ I(L1^2) + L1 + I(N0^2) + N0 + I(D1^2) + D1  + I(H^2) + H + I(L0^2) + L0 + I(D0^2) + D0 + I(N1^2) + N1 + Severite, data = suivi)
summary(mod_quadr)

mod_step_quadr <- step(mod_quadr)
mod_step_quadr
mod_step_quadr <- lm(formula = PR ~ I(L1^2) + N0 + I(D1^2) + D1 + I(H^2) + H + I(D0^2) + D0 + Severite, data = suivi) #adding severity as a correction factor
summary(mod_step_quadr)

student_res <- rstudent(mod_step_quadr)
ggplot(data = data.frame(Fitted = fitted(mod_step_quadr), Resid = student_res),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")



#Modelling yield prediction with growth period and type of manioc / variety cluster

model_full <- lm(log(PR) ~  Type_manioc + growth_period + Severite ,suivi)
summary(model_full)

model_step <- step(model_full)
summary(model_step)

model_full <- lm(log(PR) ~  cluster5 + growth_period + Severite ,suivi)
summary(model_full)

model_step <- step(model_full)
summary(model_step)


student_res <- rstudent(model_step)
ggplot(data = data.frame(Fitted = fitted(model_step), Resid = student_res),   #log transformation because of imbalanced student plot
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

#Modelling growth period

suivi$growth_period <- as.numeric(suivi$growth_period)
model_full <- lm(growth_period ~ H + L0 + L1  + N0 + D0 + D1 + N1 + Type_manioc +Severite,suivi)
summary(model_full)
model_step <- step(model_full)
summary(model_step)

student_res <- rstudent(model_step)
ggplot(data = data.frame(Fitted = fitted(model_step), Resid = student_res),   
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

suivi$growth_period <- as.numeric(suivi$growth_period)
model_full <- lm(growth_period ~ H + L0 + L1  + N0 + D0 + D1 + N1 + cluster5 +Severite,suivi)
summary(model_full)
model_step_sim <- step(model_full)
summary(model_step_sim)


suivi$growth_period <- as.numeric(suivi$growth_period)
model_full <- lm(growth_period ~ I(L1^2) + L1 + I(N0^2) + N0 + I(D1^2) + D1  + I(H^2) + H + I(L0^2) + L0 + I(D0^2) + D0 + I(N1^2) + N1 + cluster5 +Severite,suivi)
summary(model_full)
model_step_quadr <- step(model_full)
summary(model_step_quadr)

anova(model_step_sim,model_step_quadr)


student_res <- rstudent(model_step_quadr)
ggplot(data = data.frame(Fitted = fitted(model_step_quadr), Resid = student_res),   
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")



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
