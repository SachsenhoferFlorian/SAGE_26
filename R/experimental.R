
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





