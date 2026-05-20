suivi <- suivi %>% mutate(volume = masse_air - poids_eau)
suivi <- suivi %>% mutate(spec_grav = masse_air / volume)
suivi <- suivi %>% mutate(DMC = masse_seche / masse_air)
suivi <- suivi %>% mutate(volume_d = masse_air_decong - poids_eau_decong)
suivi <- suivi %>% mutate(spec_grav_d = masse_air_decong / volume_d)


DMC_reg <- lm(DMC ~ spec_grav, suivi)
summary(DMC_reg)
plot(DMC_reg)

ggplot(suivi, aes(x = spec_grav, y = DMC)) +
  geom_point() +                       
  geom_smooth(method = "lm", se = TRUE)

#Values of frozen roots
suivi <- suivi %>% mutate(spec_grav = ifelse(is.na(spec_grav), spec_grav_d, spec_grav ))
suivi <- suivi %>% mutate(frozen = ifelse(is.na(volume_d), 0, 1))

mod_froz <- lm(spec_grav ~ frozen, suivi)
anova(mod_froz)

emm_froz <- emmeans(mod_froz, ~ frozen)
pairs(emmeans(mod_froz, ~ frozen))
ratio_froz <- (emm_froz@bhat[2]+emm_froz@bhat[1]) / emm_froz@bhat[1] 
ratio_froz
suivi <- suivi %>% mutate(spec_grav = ifelse(frozen == 1, spec_grav/ratio_froz, spec_grav))

# Application of the formula
suivi <- suivi %>% mutate(DMC_pre = predict(DMC_reg,newdata = .))
suivi <- suivi %>% mutate(DM_harvest = PR*DMC_pre)





#Models
PR_DMC_mod <- lm(PR ~ DMC_pre, suivi)
summary(PR_DMC_mod)

DMC_sev_mod <- lm(DMC_pre ~ Severite, suivi)
summary(DMC_sev_mod)

DMC_farm_mod <- lm(DMC_pre ~ ID_Enquete, suivi)
anova(DMC_farm_mod)


DMC_var_mod <- lm(DMC_pre ~ cluster,suivi)
anova(DMC_var_mod)
emm_DMC_var <- emmeans(DMC_var_mod, ~ cluster)
emm_DMC_var
pairs(emm_DMC_var)

DMC_var_mod <- lm(DMC_pre ~ Type_manioc,suivi)
anova(DMC_var_mod)
emm_DMC_var <- emmeans(DMC_var_mod, ~ Type_manioc)
emm_DMC_var
pairs(emm_DMC_var)



