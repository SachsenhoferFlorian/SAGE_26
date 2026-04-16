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
suivi <- suivi[-58,] # delete NA and biased observation

#All measured variables
mod_PR_full <- lmer(PR ~ H + L0 + L1  + N0 + D0 + D1 + N1 + B0 + B1 +Severite + (1 | ID_Enquete) ,suivi)
summary(mod_PR_full)
plot(fitted(mod_PR_full), rstudent(mod_PR_full))
check_model(mod_PR_full)

drop1(mod_PR_full, test = "Chisq")
mod_PR_1 <- update(mod_PR_full, .~. - H)
anova(mod_PR_1, mod_PR_full)

drop1(mod_PR_1, test = "Chisq")
mod_PR_2 <- update(mod_PR_1, .~. - B0)
anova(mod_PR_2, mod_PR_1)

drop1(mod_PR_2, test = "Chisq")
mod_PR_3 <- update(mod_PR_2, .~. - N0)
anova(mod_PR_3, mod_PR_2)

drop1(mod_PR_3, test = "Chisq")
mod_PR_4 <- update(mod_PR_3, .~. - B1)
anova(mod_PR_4, mod_PR_3)

drop1(mod_PR_4, test = "Chisq")
mod_PR_5 <- update(mod_PR_4, .~. - L0)
anova(mod_PR_5, mod_PR_4)

drop1(mod_PR_5, test = "Chisq")
mod_PR_6 <- update(mod_PR_5, .~. - L1)
anova(mod_PR_6, mod_PR_5)

drop1(mod_PR_6, test = "Chisq")
mod_PR_7 <- update(mod_PR_6, .~. - N1)
anova(mod_PR_7, mod_PR_6)

drop1(mod_PR_7, test = "Chisq")
mod_PR_8 <- update(mod_PR_7, .~. - D1)   
anova(mod_PR_8, mod_PR_7)
AIC(mod_PR_8, mod_PR_7)

plot(fitted(mod_PR_8), rstudent(mod_PR_8))     
check_model(mod_PR_8)

ggplot(data = data.frame(Fitted = fitted(mod_PR_8), Resid = rstudent(mod_PR_8)),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")


#Quadratic model------------------------------------------

mod_PR_quadr_full <-lmer(formula = PR ~ I(L1^2) + L1 + I(N0^2) + N0 + I(D1^2) + D1  + I(H^2) + H + I(L0^2) + L0 + I(D0^2) + D0 + I(N1^2) + N1 + B0 + I(B0^2) + B1 + I(B1^2) + Severite + (1 | ID_Enquete), data = suivi)
summary(mod_PR_quadr_full)
plot(fitted(mod_PR_quadr_full), rstudent(mod_PR_quadr_full))
check_model(mod_PR_quadr_full)

drop1(mod_PR_quadr_full, test = "Chisq")                        #not yet tested, just copy of structure
mod_PR_quadr_1 <- update(mod_PR_quadr_full, .~. - L0)
anova(mod_PR_quadr_1, mod_PR_quadr_full)

drop1(mod_PR_quadr_1, test = "Chisq")
mod_PR_quadr_2 <- update(mod_PR_quadr_1, .~. - I(L0^2))
anova(mod_PR_quadr_2, mod_PR_quadr_1)

drop1(mod_PR_quadr_2, test = "Chisq")
mod_PR_quadr_3 <- update(mod_PR_quadr_2, .~. - I(L1^2))
anova(mod_PR_quadr_3, mod_PR_quadr_2)

drop1(mod_PR_quadr_3, test = "Chisq")
mod_PR_quadr_4 <- update(mod_PR_quadr_3, .~. - I(B0^2))
anova(mod_PR_quadr_4, mod_PR_quadr_3)

drop1(mod_PR_quadr_4, test = "Chisq")
mod_PR_quadr_5 <- update(mod_PR_quadr_4, .~. - B0)
anova(mod_PR_quadr_5, mod_PR_quadr_4)

drop1(mod_PR_quadr_5, test = "Chisq")
mod_PR_quadr_6 <- update(mod_PR_quadr_5, .~. - I(B1^2))
anova(mod_PR_quadr_6, mod_PR_quadr_5)

drop1(mod_PR_quadr_6, test = "Chisq")
mod_PR_quadr_7 <- update(mod_PR_quadr_6, .~. - N0)
anova(mod_PR_quadr_7, mod_PR_quadr_6)

drop1(mod_PR_quadr_7, test = "Chisq")
mod_PR_quadr_8 <- update(mod_PR_quadr_7, .~. - I(N0^2))
anova(mod_PR_quadr_8, mod_PR_quadr_7)

drop1(mod_PR_quadr_8, test = "Chisq")
mod_PR_quadr_9 <- update(mod_PR_quadr_8, .~. - D1)
anova(mod_PR_quadr_9, mod_PR_quadr_8)

drop1(mod_PR_quadr_9, test = "Chisq")
mod_PR_quadr_10 <- update(mod_PR_quadr_9, .~. - I(D1^2))
anova(mod_PR_quadr_10, mod_PR_quadr_9)

drop1(mod_PR_quadr_10, test = "Chisq")
mod_PR_quadr_11 <- update(mod_PR_quadr_10, .~. - B1)
anova(mod_PR_quadr_11, mod_PR_quadr_10)

drop1(mod_PR_quadr_11, test = "Chisq")
mod_PR_quadr_12 <- update(mod_PR_quadr_11, .~. - I(H^2))
anova(mod_PR_quadr_12, mod_PR_quadr_11)

drop1(mod_PR_quadr_12, test = "Chisq")
mod_PR_quadr_13 <- update(mod_PR_quadr_12, .~. - H)     #AIC is stagnating
anova(mod_PR_quadr_13, mod_PR_quadr_12)

drop1(mod_PR_quadr_13, test = "Chisq")
mod_PR_quadr_14 <- update(mod_PR_quadr_13, .~. - L1)
anova(mod_PR_quadr_14, mod_PR_quadr_13)

drop1(mod_PR_quadr_14, test = "Chisq")                 #all variables signifcant

summary(mod_PR_quadr_14)
plot(fitted(mod_PR_quadr_14), rstudent(mod_PR_quadr_14))
check_model(mod_PR_quadr_14)

anova(mod_PR_8, mod_PR_quadr_14)                       #quadratic model better




#Modelling yield prediction with growth period and type of manioc / variety cluster------------

mod_typ_full <- lm(log(PR) ~  Type_manioc + growth_period + Severite  ,suivi)
plot(mod_typ_full)                                                # -> log transformation
summary(mod_typ_full)

mod_typ_step <- step(mod_typ_full)                                #no significant effect of Type_manioc
summary(mod_typ_step)

mod_clust_full <- lm(log(PR) ~  cluster5 + growth_period + Severite ,suivi)
plot(mod_clust_full)                                               #-> log transformation
summary(mod_clust_full)

mod_clust_step <- step(mod_clust_full)                            #All variables significant

ggplot(data = data.frame(Fitted = fitted(mod_clust_full), Resid = rstudent(mod_clust_full)),   #student plot
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")


emm_clust <- emmeans(mod_clust_full, ~ cluster5, type = "response")
emm_clust
pairs(emm_clust)
cld_clust <- cld(emm_clust, Letters = letters)
cld_clust


ggplot(as.data.frame(cld_clust),
       aes(x = cluster5, y = response)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)


ggplot(suivi, aes(x = growth_period, y = PR, color = cluster5)) +
  geom_point() +
  geom_smooth(method = "lm")



mmod_clust_full <- lmer(PR ~  cluster5 + growth_period + Severite + (1 | ID_Enquete/Code_Var)  ,suivi)
check_model(mmod_clust_full)
summary(mmod_clust_full)

ggplot(data = data.frame(Fitted = fitted(mmod_clust_full), Resid = rstudent(mmod_clust_full)),   #student plot
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

drop1(mmod_clust_full)
mmod_PR_gp <- lmer(PR ~  growth_period + Severite + (1 | ID_Enquete/Code_Var)  ,suivi)
anova(mmod_clust_full, mmod_PR_gp)        #cluster have no significant effect on yield                                              

emm_clust_mm <- emmeans(mmod_clust_full, ~ cluster5)
emm_clust_mm
pairs(emm_clust_mm)
cld_clust_mm <- cld(emm_clust_mm, Letters = letters)
cld_clust_mm


ggplot(as.data.frame(cld_clust_mm),
       aes(x = cluster5, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)


#Modelling Harvest Index------------------------

#on ratios
mod_HI <- lm(HI ~  NLrat0 + NLrat1 + LLrat  + NNrat + DDrat + LDrat0 + LDrat1, filter(suivi, N1 > 0))
plot(mod_HI)
summary(mod_HI)

mod_HI_step <- step(mod_HI)
mod_HI_step
summary(mod_HI_step)


#on variety clusters
mmod_HI_clust <- lmer(HI ~  cluster5 + Severite + (1|ID_Enquete/Code_Var) , filter(suivi, N1 > 0))
check_model(mmod_HI_clust)
summary(mmod_HI_clust)

drop1(mmod_HI_clust, test = "Chisq")
mmod_HI_Severite <- lmer(HI ~  Severite + (1|ID_Enquete/Code_Var) , filter(suivi, N1 > 0))
anova(mmod_HI_Severite, mmod_HI_clust)    #variety clusters have no effect on Harvest Index





#Modelling growth period---------------------------------------------------
mod_gp_full <- lm(growth_period ~ H + L0 + L1  + N0 + D0 + D1 + N1 + B0+ B1 +Severite,suivi)   #lmer does not converge
summary(mod_gp_full)
mod_gp_step <- step(mod_gp_full)
summary(mod_gp_step)

ggplot(data = data.frame(Fitted = fitted(model_step), Resid = rstudent(model_step)),   
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

mod_gp_quadr_full <- lm(growth_period ~ H + L0 + L1  + N0 + D0 + D1 + N1 +Severite,suivi)
summary(mod_gp_quadr_full)


mod_gp_quadr_full <- lm(growth_period ~ I(L1^2) + L1 + I(N0^2) + N0 + I(D1^2) + D1  + I(H^2) + H + I(L0^2) + L0 + I(D0^2) + D0 + I(N1^2) + N1 + B0 + I(B0^2) + B1 + I(B1^2) +Severite,suivi)
summary(mod_gp_quadr_full)
mod_gp_quadr_step <- step(mod_gp_quadr_full)
summary(mod_gp_quadr_step)

anova(mod_gp_step, mod_gp_quadr_step)

ggplot(data = data.frame(Fitted = fitted(mod_gp_quadr_step), Resid = rstudent(mod_gp_quadr_step)),   
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")



#Modelling Biomass-----------------------------------
mod_bm_full <- lm(PB ~ H + L0 + L1  + N0 + D0 + D1 + N1 + B0 +B1 + Severite,suivi)
plot(mod_bm_full)
summary(mod_bm_full)

mmod_bm_full <- lmer(PB ~ H + L0 + L1  + N0 + D0 + D1 + N1 + B0 +B1 + Severite + (1|ID_Enquete/Code_Var),suivi)
check_model(mod_bm_full)
summary(mmod_bm_full)

ggplot(data = data.frame(Fitted = fitted(mmod_bm_full), Resid = rstudent(mmod_bm_full)),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")





