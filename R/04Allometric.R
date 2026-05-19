
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
suivi_numeric$growth_period <- as.numeric(suivi_numeric$growth_period)
suivi$growth_period <- as.numeric(suivi$growth_period)
suivi$growth_period_m <- suivi$growth_period / 30    #Transformation to months
suivi <- suivi %>% filter(N0<150 & !is.na(N0) )      # deleting observation with mistake
suivi_numeric <- suivi_numeric %>% filter(N0<150 & !is.na(N0))      # deleting observation with mistake
suivi_numeric <- suivi_numeric %>% dplyr::select(-c(masse_air,poids_eau,masse_seche))


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

#Severity
mod_Sev <- lm(PR ~ Severite, suivi)
summary(mod_Sev)

#Modelling Yield Prediction------------------------------
suivi <- suivi %>% filter(PR > 0)

#All measured variables
mod_PR_full <- lm(PR ~ H + L0 + L1  + N0 + D0 + D1 + N1 + B0 + B1+ B0:D0 +B1:D1+ B0:L0 + B1:L1+ B0:N0+B1:N1+ Severite+ growth_period,suivi)
summary(mod_PR_full)
plot(fitted(mod_PR_full), rstudent(mod_PR_full))
check_model(mod_PR_full)

mod_PR_step <- step(mod_PR_full)

plot(fitted(mod_PR_step), rstudent(mod_PR_step))     
check_model(mod_PR_step)

ggplot(data = data.frame(Fitted = fitted(mod_PR_step), Resid = rstudent(mod_PR_step)),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

summary(mod_PR_step)
performance_aic(mod_PR_step)

#log transformed
mod_PR_log_full <- lm(log(PR) ~ H + L0 + L1  + N0 + D0 + D1 + N1 + B0 + B1 +B0:D0 +B1:D1+ B0:L0 + B1:L1+ B0:N0+B1:N1 +Severite + growth_period ,suivi)
summary(mod_PR_log_full)
plot(fitted(mod_PR_log_full), rstudent(mod_PR_log_full))
check_model(mod_PR_log_full)

mod_PR_log_step <- step(mod_PR_log_full)

plot(fitted(mod_PR_log_step), rstudent(mod_PR_log_step))     
check_model(mod_PR_log_step)

ggplot(data = data.frame(Fitted = fitted(mod_PR_log_step), Resid = rstudent(mod_PR_log_step)),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

summary(mod_PR_log_step)
performance_aic(mod_PR_log_step)


#Quadratic model------------------------------------------

mod_PR_quadr_full <-lm(formula = PR ~ I(L1^2) + L1 + I(N0^2) + N0 + I(D1^2) + D1  + I(H^2) + H + I(L0^2) + L0 + I(D0^2) + D0 + I(N1^2) + N1 + B0 + I(B0^2) + B1 + I(B1^2) + Severite + growth_period , data = suivi)
summary(mod_PR_quadr_full)
plot(fitted(mod_PR_quadr_full), rstudent(mod_PR_quadr_full))
check_model(mod_PR_quadr_full)

mod_PR_quadr_step <- step(mod_PR_quadr_full)

ggplot(data = data.frame(Fitted = fitted(mod_PR_quadr_step), Resid = rstudent(mod_PR_quadr_step)),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

summary(mod_PR_quadr_step)
AIC(mod_PR_quadr_step)

#Quadratic logistic model

mod_PR_quadrlog_full <-lm(formula = log(PR) ~ I(L1^2) + L1 + I(N0^2) + N0 + I(D1^2) + D1  + I(H^2) + H + I(L0^2) + L0 + I(D0^2) + D0 + I(N1^2) + N1 + B0 + I(B0^2) + B1 + I(B1^2) + Severite + growth_period, data = suivi)
summary(mod_PR_quadrlog_full)
plot(fitted(mod_PR_quadrlog_full), rstudent(mod_PR_quadrlog_full))
check_model(mod_PR_quadrlog_full)

mod_PR_quadrlog_step <- step(mod_PR_quadrlog_full)

summary(mod_PR_quadrlog_step)
plot(fitted(mod_PR_quadrlog_step), rstudent(mod_PR_quadrlog_step))
check_model(mod_PR_quadrlog_step)

#adding of relevant interactions
mod_PR_quadrlog_step_int <- lm(formula = log(PR) ~ I(L1^2) + I(N0^2) + N0 + I(D1^2) + D1 + I(D0^2) + I(N1^2) + N1 + B0 + D0:B0 + L0:B0, data = suivi)
summary(mod_PR_quadrlog_step_int)
performance_aic(mod_PR_quadrlog_step_int)

#Model comparison------------------------------------------------
performance_aic(mod_PR_step)
performance_aic(mod_PR_quadr_step)
performance_aic(mod_PR_log_step)
performance_aic(mod_PR_quadrlog_step)
performance_aic(mod_PR_quadrlog_step_int)




#Modelling yield prediction with growth period and type of manioc / variety cluster------------

mod_typ_full <- lm(log(PR) ~  Type_manioc + growth_period + Severite  ,suivi)
plot(fitted(mod_typ_full), rstudent(mod_typ_full))                                              # -> log transformation
summary(mod_typ_full)

mod_typ_step <- step(mod_typ_full)                                #no significant effect of Type_manioc
summary(mod_typ_step)

mod_clust_full <- lm(log(PR) ~  cluster + growth_period + Severite ,suivi)
plot(fitted(mod_clust_full), rstudent(mod_clust_full))                                                  #-> log transformation
summary(mod_clust_full)
mod_clust_step <- step(mod_clust_full)                            

ggplot(data = data.frame(Fitted = fitted(mod_clust_step), Resid = rstudent(mod_clust_step)),   #student plot
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")


emm_clust <- emmeans(mod_clust_full, ~ cluster, type = "response")
emm_clust
pairs(emm_clust)
cld_clust <- cld(emm_clust, Letters = letters)
cld_clust


ggplot(as.data.frame(cld_clust),
       aes(x = cluster, y = response)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)


ggplot(suivi, aes(x = growth_period, y = PR, color = cluster)) +
  geom_point() +
  geom_smooth(method = "lm")


#Mixed models------


mmod_clust_full <- lmer(PR ~  cluster + growth_period + cluster:growth_period + Severite + (1 | ID_Enquete/Code_Var)  ,suivi)
check_model(mmod_clust_full)
summary(mmod_clust_full)

ggplot(data = data.frame(Fitted = fitted(mmod_clust_full), Resid = rstudent(mmod_clust_full)),   #student plot
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

drop1(mmod_clust_full)
mmod_clust_fin <- lmer(PR ~  cluster + growth_period + cluster:growth_period + (1 | ID_Enquete/Code_Var)  ,suivi)
                                             

emm_clust_mm <- emmeans(mmod_clust_fin, ~ cluster, at = list(growth_period = 360))
emm_clust_mm
pairs(emm_clust_mm)
cld_clust_mm <- cld(emm_clust_mm, Letters = letters)
cld_clust_mm


ggplot(as.data.frame(cld_clust_mm),
       aes(x = cluster, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)






#Modelling Harvest Index------------------------

#on ratios
mod_HI <- lm(HI ~  NLrat0 + NLrat1 + LLrat  + NNrat + DDrat + LDrat0 + LDrat1, filter(suivi, N1 > 0))
plot(fitted(mod_HI), rstudent(mod_HI)) 
summary(mod_HI)

mod_HI_step <- step(mod_HI)
mod_HI_step
summary(mod_HI_step)


#on variety clusters
mod_HI_clust <- lm(HI ~  cluster + growth_period + cluster:growth_period + Severite  , suivi)
check_model(mod_HI_clust)
summary(mod_HI_clust)

mod_HI_clust_step <- step(mod_HI_clust)
plot(fitted(mod_HI_clust_step), rstudent(mod_HI_clust_step))
summary(mod_HI_clust_step)


emm_HI_clust <- emmeans(mod_HI_clust_step, ~ cluster, at = list(growth_period = 360))
emm_HI_clust
pairs(emm_HI_clust)
cld_HI_clust <- cld(emm_HI_clust, Letters = letters)
cld_HI_clust

ggplot(as.data.frame(cld_HI_clust),
       aes(x = cluster, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)





#Mixed alternative

mmod_HI_clust <- lmer(HI ~  cluster + Severite + (1|ID_Enquete/Code_Var) , suivi)
check_model(mmod_HI_clust)
plot(fitted(mmod_HI_clust), rstudent(mmod_HI_clust))
summary(mmod_HI_clust)

drop1(mmod_HI_clust, test = "Chisq")
mmod_HI_Severite <- lmer(HI ~  Severite + (1|ID_Enquete/Code_Var) , suivi)
anova(mmod_HI_Severite, mmod_HI_clust)    



