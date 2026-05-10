suivi <- suivi %>% mutate(volume = masse_air - poids_eau)
suivi <- suivi %>% mutate(spec_grav = masse_air / volume)
suivi <- suivi %>% mutate(DMC = masse_seche / masse_air)

DMC_reg <- lm(DMC ~ spec_grav, suivi)
summary(DMC_reg)
plot(DMC_reg)

ggplot(suivi, aes(x = spec_grav, y = DMC)) +
  geom_point() +                       
  geom_smooth(method = "lm", se = TRUE)

# Application of the formula
suivi <- suivi %>% mutate(DMC_pre = predict(DMC_reg,newdata = .))
suivi <- suivi %>% mutate(DM_harvest = PR*DMC_pre)



#Models
PR_DMC_mod <- lm(PR ~ DMC_pre, suivi)
summary(PR_DMC_mod)

DMC_sev_mod <- lm(DMC_pre ~ Severite, suivi)
summary(DMC_sev_mod)


DMC_var_mod <- lm(DMC_pre ~ cluster5,suivi)
summary(DMC_var_mod)
emm_DMC_var <- emmeans(DMC_var_mod, ~ cluster5)
pairs(emm_DMC_var)


