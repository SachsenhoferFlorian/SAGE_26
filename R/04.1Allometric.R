
#Modelling Yield Prediction------------------------------
suivi <- suivi %>% filter(PR > 0)

#All measured variables
mod_PR_full <- lmer(PR ~ H + L0 + L1  + N0 + D0 + D1 + N1 + B0 + B1 + B0:D0 + B1:D1 +Severite + growth_period + (1 | ID_Enquete) ,suivi)
summary(mod_PR_full)
plot(fitted(mod_PR_full), rstudent(mod_PR_full))
check_model(mod_PR_full)


#log transformed
mod_PR_log_full <- lmer(log(PR) ~ H + L0 + L1  + N0 + D0 + D1 + N1 + B0 + B1 +B0:D0 +B1:D1+ B0:L0 + B1:L1+ B0:N0+B1:N1 +Severite + growth_period + (1 | ID_Enquete) ,suivi)
summary(mod_PR_log_full)
plot(fitted(mod_PR_log_full), rstudent(mod_PR_log_full))
check_model(mod_PR_log_full)

drop1(mod_PR_log_full, test = "Chisq")
mod_PR_log_1 <- update(mod_PR_log_full, .~. - D1:B1)
anova(mod_PR_log_1, mod_PR_log_full)

drop1(mod_PR_log_1, test = "Chisq")
mod_PR_log_2 <- update(mod_PR_log_1, .~. - Severite)
anova(mod_PR_log_2, mod_PR_log_1)

drop1(mod_PR_log_2, test = "Chisq")
mod_PR_log_3 <- update(mod_PR_log_2, .~. - N0:B0)
anova(mod_PR_log_3, mod_PR_log_2)
performance_aic(mod_PR_log_3)

drop1(mod_PR_log_3, test = "Chisq")
mod_PR_log_4 <- update(mod_PR_log_3, .~. - N0)
anova(mod_PR_log_4, mod_PR_log_3)
performance_aic(mod_PR_log_4)

drop1(mod_PR_log_4, test = "Chisq")
mod_PR_log_5 <- update(mod_PR_log_4, .~. - D1)
anova(mod_PR_log_5, mod_PR_log_4)
performance_aic(mod_PR_log_5)

drop1(mod_PR_log_5, test = "Chisq")
mod_PR_log_6 <- update(mod_PR_log_5, .~. - N1:B1)
anova(mod_PR_log_6, mod_PR_log_5)
performance_aic(mod_PR_log_6)

drop1(mod_PR_log_6, test = "Chisq")
mod_PR_log_7 <- update(mod_PR_log_6, .~. - N1)
anova(mod_PR_log_7, mod_PR_log_6)
performance_aic(mod_PR_log_7)

drop1(mod_PR_log_7, test = "Chisq")
mod_PR_log_8 <- update(mod_PR_log_7, .~. - H)
anova(mod_PR_log_8, mod_PR_log_7)
performance_aic(mod_PR_log_8)

drop1(mod_PR_log_8, test = "Chisq")
mod_PR_log_9 <- update(mod_PR_log_8, .~. - L0:B0)
anova(mod_PR_log_9, mod_PR_log_8)
performance_aic(mod_PR_log_9)

drop1(mod_PR_log_9, test = "Chisq")
mod_PR_log_10 <- update(mod_PR_log_9, .~. - D0:B0)
anova(mod_PR_log_10, mod_PR_log_9)
performance_aic(mod_PR_log_10)

drop1(mod_PR_log_10, test = "Chisq")
mod_PR_log_11 <- update(mod_PR_log_10, .~. - L0)
anova(mod_PR_log_11, mod_PR_log_10)
performance_aic(mod_PR_log_11)

drop1(mod_PR_log_11, test = "Chisq")
mod_PR_log_12 <- update(mod_PR_log_11, .~. - L1:B1)
anova(mod_PR_log_12, mod_PR_log_11)
performance_aic(mod_PR_log_12)

drop1(mod_PR_log_12, test = "Chisq")
mod_PR_log_13 <- update(mod_PR_log_12, .~. - L1)
anova(mod_PR_log_13, mod_PR_log_12)
performance_aic(mod_PR_log_13)

drop1(mod_PR_log_13, test = "Chisq")
mod_PR_log_14 <- update(mod_PR_log_13, .~. - B1)
anova(mod_PR_log_14, mod_PR_log_13)
performance_aic(mod_PR_log_14)

drop1(mod_PR_log_14, test = "Chisq")
mod_PR_log_15 <- update(mod_PR_log_14, .~. - growth_period)
anova(mod_PR_log_15, mod_PR_log_14)
performance_aic(mod_PR_log_15)

drop1(mod_PR_log_15, test = "Chisq")



plot(fitted(mod_PR_log_15), rstudent(mod_PR_log_15))     
check_model(mod_PR_log_15)

ggplot(data = data.frame(Fitted = fitted(mod_PR_log_8), Resid = rstudent(mod_PR_log_8)),
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

summary(mod_PR_log_15)
performance_aic(mod_PR_log_15)

#Quadratic logistic model------------------------------------------

mod_PR_quadrlog_full <-lmer(formula = log(PR) ~ I(L1^2) + L1 + I(N0^2) + N0 + I(D1^2) + D1  + I(H^2) + H + I(L0^2) + L0 + I(D0^2) + D0 + I(N1^2) + N1 + B0 + I(B0^2) + B1 + I(B1^2) + Severite + growth_period + (1 | ID_Enquete), data = suivi)
summary(mod_PR_quadrlog_full)
plot(fitted(mod_PR_quadrlog_full), rstudent(mod_PR_quadrlog_full))
check_model(mod_PR_quadrlog_full)

drop1(mod_PR_quadrlog_full, test = "Chisq")                        
mod_PR_quadrlog_1 <- update(mod_PR_quadrlog_full, .~. - N0)
anova(mod_PR_quadrlog_1, mod_PR_quadrlog_full)

drop1(mod_PR_quadrlog_1, test = "Chisq")
mod_PR_quadrlog_2 <- update(mod_PR_quadrlog_1, .~. - I(L1^2))
anova(mod_PR_quadrlog_2, mod_PR_quadrlog_1)

drop1(mod_PR_quadrlog_2, test = "Chisq")
mod_PR_quadrlog_3 <- update(mod_PR_quadrlog_2, .~. - I(D1^2))
anova(mod_PR_quadrlog_3, mod_PR_quadrlog_2)

drop1(mod_PR_quadrlog_3, test = "Chisq")
mod_PR_quadrlog_4 <- update(mod_PR_quadrlog_3, .~. - L1)
anova(mod_PR_quadrlog_4, mod_PR_quadrlog_3)

drop1(mod_PR_quadrlog_4, test = "Chisq")
mod_PR_quadrlog_5 <- update(mod_PR_quadrlog_4, .~. - I(B0^2))
anova(mod_PR_quadrlog_5, mod_PR_quadrlog_4)

drop1(mod_PR_quadrlog_5, test = "Chisq")
mod_PR_quadrlog_6 <- update(mod_PR_quadrlog_5, .~. - Severite)
anova(mod_PR_quadrlog_6, mod_PR_quadrlog_5)

drop1(mod_PR_quadrlog_6, test = "Chisq")
mod_PR_quadrlog_7 <- update(mod_PR_quadrlog_6, .~. - I(H^2))
anova(mod_PR_quadrlog_7, mod_PR_quadrlog_6)

drop1(mod_PR_quadrlog_7, test = "Chisq")
mod_PR_quadrlog_8 <- update(mod_PR_quadrlog_7, .~. - H)
anova(mod_PR_quadrlog_8, mod_PR_quadrlog_7)

drop1(mod_PR_quadrlog_8, test = "Chisq")
mod_PR_quadrlog_9 <- update(mod_PR_quadrlog_8, .~. - I(B1^2))
anova(mod_PR_quadrlog_9, mod_PR_quadrlog_8)

drop1(mod_PR_quadrlog_9, test = "Chisq")
mod_PR_quadrlog_10 <- update(mod_PR_quadrlog_9, .~. - B1)
anova(mod_PR_quadrlog_10, mod_PR_quadrlog_9)

drop1(mod_PR_quadrlog_10, test = "Chisq")
mod_PR_quadrlog_11 <- update(mod_PR_quadrlog_10, .~. - I(N1^2))
anova(mod_PR_quadrlog_11, mod_PR_quadrlog_10)

drop1(mod_PR_quadrlog_11, test = "Chisq")
mod_PR_quadrlog_12 <- update(mod_PR_quadrlog_11, .~. - N1)
anova(mod_PR_quadrlog_12, mod_PR_quadrlog_11)

drop1(mod_PR_quadrlog_12, test = "Chisq")
mod_PR_quadrlog_13 <- update(mod_PR_quadrlog_12, .~. - D1)     
anova(mod_PR_quadrlog_13, mod_PR_quadrlog_12)
performance_aic(mod_PR_quadrlog_13)

drop1(mod_PR_quadrlog_13, test = "Chisq")
mod_PR_quadrlog_14 <- update(mod_PR_quadrlog_13, .~. - I(N0^2))
anova(mod_PR_quadrlog_14, mod_PR_quadrlog_13)
performance_aic(mod_PR_quadrlog_14)

drop1(mod_PR_quadrlog_14, test = "Chisq")                
mod_PR_quadrlog_15 <- update(mod_PR_quadrlog_14, .~. - growth_period)
anova(mod_PR_quadrlog_15, mod_PR_quadrlog_14)
performance_aic(mod_PR_quadrlog_15)
drop1(mod_PR_quadrlog_15, test = "Chisq")


summary(mod_PR_quadrlog_15)
plot(fitted(mod_PR_quadrlog_15), rstudent(mod_PR_quadrlog_15))
check_model(mod_PR_quadrlog_15)


#Model comparison------------------------------------------------
compare_performance(mod_PR_step,
                    mod_PR_quadr_step,
                    mod_PR_log_step,
                    mod_PR_quadrlog_step,
                    mod_PR_quadrlog_step_int,
                    mod_PR_log_15,
                    mod_PR_quadrlog_15)


#Interactions forward testing-------------
mlogtest <- lmer(log(PR) ~ H + D0 + Severite + B0+ B0:D0 + (1 | ID_Enquete),suivi)
anova(mlogtest, mod_PR_log_7)

mlogtest <- lmer(log(PR) ~ H + D0 + Severite + B1 + B1:D1 + (1 | ID_Enquete),suivi)
anova(mlogtest, mod_PR_log_7)


#Modelling yield prediction with growth period and type of manioc / variety cluster------------

mod_typ_full <- lm(log(PR) ~  Type_manioc + growth_period + Severite  ,suivi)
plot(mod_typ_full)                                                # -> log transformation
summary(mod_typ_full)

mod_typ_step <- step(mod_typ_full)                                #no significant effect of Type_manioc
summary(mod_typ_step)

mod_clust_full <- lm(log(PR) ~  cluster + growth_period + Severite ,suivi)
plot(mod_clust_full)                                               #-> log transformation
summary(mod_clust_full)
mod_clust_step <- step(mod_clust_full)                            #All variables significant

ggplot(data = data.frame(Fitted = fitted(mod_clust_full), Resid = rstudent(mod_clust_full)),   #student plot
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



mmod_clust_full <- lmer(PR ~  cluster + growth_period + Severite + (1 | ID_Enquete/Code_Var)  ,suivi)
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
mmod_HI_clust <- lmer(HI ~  cluster + Severite + (1|ID_Enquete/Code_Var) , filter(suivi, N1 > 0))
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

ggplot(data = data.frame(Fitted = fitted(mod_gp_step), Resid = rstudent(mod_gp_step)),   
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





