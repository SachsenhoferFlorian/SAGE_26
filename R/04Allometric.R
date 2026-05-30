Suivi_summary <- suivi %>% group_by(Farmer) %>%
        summarise(
        Accessions = n_distinct(Code_Var),
        Plants = n()
      ) %>% adorn_totals(name= "Totals")



suivi <- suivi %>% mutate(symptom = ifelse(Severite == 0, 0, 1),
                          symptom_marqu = ifelse(Severite_marqu== 0, 0, 1))
describe(dplyr::select(suivi, c("symptom","symptom_marqu")))
table(suivi$symptom)
table(suivi$symptom_marqu)



#Calculations--------------
#Calcultions of allometric ratios
suivi <- suivi %>% mutate(NLrat0 = N0/L0)
suivi <- suivi %>% mutate(NLrat1 = N1/L1)
suivi <- suivi %>% mutate(LLrat= L1/L0)
suivi <- suivi %>% mutate(NNrat= N1/N0)
suivi <- suivi %>% mutate(DDrat= D1/D0)
suivi <- suivi %>% mutate(LDrat0= L0/D0)
suivi <- suivi %>% mutate(LDrat1= L1/D1)
suivi <- suivi %>% mutate(HI= PR/(PR+PB))


#Calculations for Severity
suivi <- suivi %>% mutate(Sev_diff = Severite-Severite_marqu)
suivi <- suivi %>% mutate(Sev_diff = ifelse(Sev_diff <= 0, 10, Sev_diff))
suivi <- suivi %>% mutate(k= Sev_diff/(as.numeric(delta_Enqu)))
avg_k <- suivi %>%
          filter(as.numeric(delta_Enqu) > 30) %>%
          summarise(mean_k = mean(k)) %>%
          pull(mean_k)
suivi <- suivi %>% mutate(k = ifelse(as.numeric(delta_Enqu) < 30, avg_k, k))
suivi <- suivi %>% mutate(deltaT_infect= Sev_diff/k)
suivi <- suivi %>% mutate(Severite_cum = (deltaT_infect*Severite)/2)
suivi <- suivi %>% mutate(Severite_cum_percent = Severite_cum/(as.numeric(growth_period)))



#Descriptive Analysis---------------------------------
suivi_numeric$growth_period <- as.numeric(suivi_numeric$growth_period)
suivi$growth_period <- as.numeric(suivi$growth_period)
suivi$growth_period_m <- suivi$growth_period / 30    #Transformation to months
suivi_numeric <- suivi_numeric %>% dplyr::select(-c(masse_air,poids_eau,masse_seche, masse_air_cong,masse_air_decong,poids_eau_cong,poids_eau_decong))
suivi <- suivi %>% filter(N0<150 & !is.na(N0) )      # deleting observation with mistake
suivi_numeric <- suivi_numeric %>% filter(N0<150 & !is.na(N0))      # deleting observation with mistake


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
         type = "full",
         tl.col = "black",
         tl.srt = 50,
         addCoef.col = "black",
         addCoefasPercent = TRUE) 

#Severity
mod_Sev <- lm(PR ~ Severite_marqu*Severite + Severite_cum + Severite_cum_percent + growth_period, suivi)
plot(fitted(mod_Sev), rstudent(mod_Sev))
mod_Sev <- lm(log(PR) ~ Severite_marqu*Severite + Severite_cum + Severite_cum_percent + growth_period, suivi)
plot(fitted(mod_Sev), rstudent(mod_Sev))
summary(mod_Sev)

mod_Sev_step <- step(mod_Sev)
summary(mod_Sev_step)

#Modelling Yield Prediction------------------------------
suivi <- suivi %>% filter(PR > 0)

#All measured variables
mod_PR_full <- lm(PR ~ H + L0 + L1  + N0 + D0 + D1 + N1 + B0 + B1+ B0:D0 +B1:D1+ B0:L0 + B1:L1+ B0:N0+B1:N1+ Severite_cum+ growth_period,suivi)
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
mod_PR_log_full <- lm(log(PR) ~ H + L0 + L1  + N0 + D0 + D1 + N1 + B0 + B1 +B0:D0 +B1:D1+ B0:L0 + B1:L1+ B0:N0+B1:N1 +Severite_cum + growth_period ,suivi)
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
performance_aic(mod_PR_quadrlog_step)

#adding of relevant interactions
mod_PR_quadrlog_step_int <- lm(formula = log(PR) ~ I(L1^2) + I(N0^2) + N0 + I(D1^2) + D1 + I(D0^2) + I(N1^2) + N1 + B0 + D0:B0 + L0:B0, data = suivi)
summary(mod_PR_quadrlog_step_int)
performance_aic(mod_PR_quadrlog_step_int)

#Model comparison------------------------------------------------
compare_performance(mod_PR_step,
                    mod_PR_quadr_step,
                    mod_PR_log_step,
                    mod_PR_quadrlog_step,
                    mod_PR_quadrlog_step_int)



#Modelling yield prediction with growth period and type of manioc / variety cluster------------

mod_clust_full <- lm(PR ~  cluster*growth_period + Severite_cum ,suivi)
plot(fitted(mod_clust_full), rstudent(mod_clust_full))               #-> log-transformation
mod_clust_full <- lm(log(PR) ~  cluster*growth_period + Severite_cum ,suivi)
plot(fitted(mod_clust_full), rstudent(mod_clust_full))                                                   
summary(mod_clust_full)
mod_clust_step <- step(mod_clust_full)
summary(mod_clust_step)
anova(mod_clust_step)

ggplot(data = data.frame(Fitted = fitted(mod_clust_step), Resid = rstudent(mod_clust_step)),   #student plot
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

ggplot(suivi, aes(x = growth_period, y = PR, color = cluster)) +
  geom_point() +
  geom_parallel_slopes(formula = y ~ x)


emm_clust <- emmeans(mod_clust_step, ~ cluster, type = "response")
emm_clust
pairs(emm_clust)
cld_clust <- cld(emm_clust, Letters = letters)
cld_clust

ggplot(as.data.frame(cld_clust),
       aes(x = cluster, y = response)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)



mod_typ_full <- lm(PR ~   Type_manioc*growth_period + Severite_cum  ,suivi)
plot(fitted(mod_typ_full), rstudent(mod_typ_full)) 
mod_typ_full <- lm(log(PR) ~  cluster*growth_period + Type_manioc*growth_period + Severite_cum  ,suivi)
plot(fitted(mod_typ_full), rstudent(mod_typ_full)) 
summary(mod_typ_full)

mod_typ_step <- step(mod_typ_full)  
check_model(mod_typ_step)
summary(mod_typ_step)


emm_type <- emmeans(mod_typ_step, ~ Type_manioc, at = list(growth_period = 360),type = "response")    #  ,type = "response"
emm_type
pairs(emm_type)
cld_type <- cld(emm_type, Letters = letters)
cld_type


ggplot(as.data.frame(cld_type),
       aes(x = Type_manioc, y = response)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)



ggplot(suivi, aes(x = growth_period, y = PR, color = Type_manioc)) +
  geom_point() +
  geom_smooth(method = "lm")



#Mixed models------


mmod_clust_full <- lmer(PR ~ cluster*growth_period + Severite + (1 | ID_Enquete/Code_Var)  ,suivi)
check_model(mmod_clust_full)
anova(mmod_clust_full)

ggplot(data = data.frame(Fitted = fitted(mmod_clust_full), Resid = rstudent(mmod_clust_full)),   #student plot
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

drop1(mmod_clust_full)
mmod_clust_1 <- lmer(PR ~  cluster + growth_period + cluster:growth_period + (1 | ID_Enquete/Code_Var)  ,suivi)
drop1(mmod_clust_1)                                            
mmod_clust_2 <- lmer(PR ~  cluster + growth_period + (1 | ID_Enquete/Code_Var)  ,suivi)
drop1(mmod_clust_2)
mmod_clust_fin <- lmer(PR ~   growth_period + (1 | ID_Enquete/Code_Var)  ,suivi)
drop1(mmod_clust_fin)


mmod_clust_full <- lmer(PR ~ Type_manioc*growth_period + Severite + (1 | ID_Enquete/Code_Var)  ,suivi)
check_model(mmod_clust_full)
anova(mmod_clust_full)

ggplot(data = data.frame(Fitted = fitted(mmod_clust_full), Resid = rstudent(mmod_clust_full)),   #student plot
       aes(x = Fitted, y = Resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Studentized Residuals Plot")

drop1(mmod_clust_full)
mmod_clust_fin <- lmer(log(PR) ~  Type_manioc*growth_period + Severite + (1 | ID_Enquete/Code_Var)  ,suivi)
drop1(mmod_clust_fin)
mmod_clust_fin <- lmer(log(PR) ~  Type_manioc*growth_period  + (1 | ID_Enquete/Code_Var)  ,suivi)
anova(mmod_clust_fin)

emm_clust_mm <- emmeans(mmod_clust_fin, ~ Type_manioc, at = list(growth_period = 360), type="response")
emm_clust_mm
pairs(emm_clust_mm)
cld_clust_mm <- cld(emm_clust_mm, Letters = letters)
cld_clust_mm


ggplot(as.data.frame(cld_clust_mm),
       aes(x = cluster, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)

#Modelling Severity----------

mod_Sev_clust <- lm(Severite_cum ~ cluster+growth_period,suivi)
anova(mod_Sev_clust)
summary(mod_Sev_clust)

ggplot(suivi, aes(x = growth_period, y = Severite_cum, color = cluster)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(suivi, aes(x = growth_period, y = Severite_cum, color = cluster)) +
  geom_point() +
  geom_parallel_slopes(formula = y ~ x)

emt_Sevclust <- emtrends(mod_Sev_clust, ~ cluster, var = "growth_period")  
emt_Sevclust
pairs(emt_Sevclust)


mod_k_clust <- lm(k ~ cluster+growth_period,filter(suivi, as.numeric(delta_Enqu)> 30))
anova(mod_k_clust)
summary(mod_k_clust)

summary(suivi$k)

#Modelling Harvest Index------------------------

#on ratios
mod_HI <- lm(HI ~  NLrat0 + NLrat1 + LLrat  + NNrat + DDrat + LDrat0 + LDrat1, filter(suivi, N1 > 0))
plot(fitted(mod_HI), rstudent(mod_HI)) 
summary(mod_HI)

mod_HI_step <- step(mod_HI)
mod_HI_step
summary(mod_HI_step)


ggplot(filter(suivi, N1 > 0), aes(x = HI, y =DDrat)) +
  geom_point() +
  geom_smooth(method = "lm")


#on variety clusters
mod_HI_clust <- lm(HI ~  cluster*growth_period + Severite_cum  , suivi)
plot(fitted(mod_HI_clust), rstudent(mod_HI_clust))
check_model(mod_HI_clust)
anova(mod_HI_clust)
summary(mod_HI_clust)


mod_HI_clust_step <- step(mod_HI_clust)
anova(mod_HI_clust_step)
plot(fitted(mod_HI_clust_step), rstudent(mod_HI_clust_step))
summary(mod_HI_clust_step)
anova(mod_HI_clust_step)


emm_HI_clust <- emmeans(mod_HI_clust_step, ~ cluster:growth_period, at = list(growth_period = 360))
emm_HI_clust
pairs(emm_HI_clust)
cld_HI_clust <- cld(emm_HI_clust, Letters = letters)
cld_HI_clust

ggplot(as.data.frame(cld_HI_clust),
       aes(x = cluster, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)

ggplot(suivi, aes(x = HI, y = cluster:growth_period)) +
  geom_point() +
  geom_smooth(method = "lm")

emtrends(mod_HI_clust_step, ~ cluster, var = "growth_period")  
pairs(emtrends(mod_HI_clust_step, ~ cluster, var = "growth_period"))


# Get predictions across a range of growth_period values
pred_grid <- ref_grid(mod_HI_clust_step, 
                      at = list(growth_period = seq(160, 950, length = 100)))
preds <- as.data.frame(emmeans(pred_grid, ~ cluster * growth_period))

# Plot
ggplot(preds, aes(x = growth_period, y = emmean, color = cluster, fill = cluster)) +
  geom_point(data = suivi, 
             aes(x = growth_period, y = HI, color = cluster),
             alpha = 0.3, size = 1) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  labs(x = "Growth Period", 
       y = "Predicted Outcome", 
       color = "Cluster",
       title = "Significant Cluster × Growth Period Interaction",
       subtitle = "Lines show slopes differing by cluster (main effects non-significant)") +
  theme(legend.position = "bottom")


#on cassava type--------

mod_HI_clust <- lm(HI ~  growth_period*Type_manioc + Severite  , suivi)
check_model(mod_HI_clust)
anova(mod_HI_clust)


mod_HI_clust_step <- step(mod_HI_clust)
plot(fitted(mod_HI_clust_step), rstudent(mod_HI_clust_step))
summary(mod_HI_clust_step)
anova(mod_HI_clust_step)


emm_HI_clust <- emmeans(mod_HI_clust_step, ~ Type_manioc:growth_period, at = list(growth_period = 360))
emm_HI_clust
pairs(emm_HI_clust)
cld_HI_clust <- cld(emm_HI_clust, Letters = letters)
cld_HI_clust

ggplot(as.data.frame(cld_HI_clust),
       aes(x = Type_manioc, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)



emtrends(mod_HI_clust_step, ~ Type_manioc, var = "growth_period")  
pairs(emtrends(mod_HI_clust_step, ~ Type_manioc, var = "growth_period"))


# Get predictions across a range of growth_period values
pred_grid <- ref_grid(mod_HI_clust_step, 
                      at = list(growth_period = seq(160, 950, length = 100)))
preds <- as.data.frame(emmeans(pred_grid, ~ Type_manioc * growth_period))

# Plot
ggplot(preds, aes(x = growth_period, y = emmean, color = Type_manioc, fill = Type_manioc)) +
  geom_point(data = suivi, 
             aes(x = growth_period, y = HI, color = Type_manioc),
             alpha = 0.3, size = 1) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  labs(x = "Growth Period", 
       y = "Predicted Outcome", 
       color = "Cluster",
       title = "Significant Cluster × Growth Period Interaction",
       subtitle = "Lines show slopes differing by cluster (main effects non-significant)") +
  theme(legend.position = "bottom")



#Mixed alternative

mmod_HI_clust <- lmer(HI ~  cluster + Severite + (1|ID_Enquete/Code_Var) , suivi)
check_model(mmod_HI_clust)
plot(fitted(mmod_HI_clust), rstudent(mmod_HI_clust))
summary(mmod_HI_clust)

drop1(mmod_HI_clust, test = "Chisq")
mmod_HI_Severite <- lmer(HI ~  Severite + (1|ID_Enquete/Code_Var) , suivi)
anova(mmod_HI_Severite, mmod_HI_clust)    



