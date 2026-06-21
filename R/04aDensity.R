suivi <- suivi_full
suivi <- suivi %>% mutate(volume = masse_air - poids_eau)
suivi <- suivi %>% mutate(spec_grav = masse_air / volume)
suivi <- suivi %>% mutate(DMC = masse_seche / masse_air)
suivi <- suivi %>% mutate(DMC_d = masse_seche / masse_air_decong)
suivi <- suivi %>% mutate(volume_d = masse_air_decong - poids_eau_decong)
suivi <- suivi %>% mutate(spec_grav_d = masse_air_decong / volume_d)


DMC_reg <- lm(DMC ~ spec_grav, suivi)
summary(DMC_reg)

#used
fig_DMCSpecGra <- ggplot(suivi, aes(x = spec_grav, y = DMC)) +
  geom_point() +                       
  geom_smooth(method = "lm", se = TRUE)+
  ylab("Rel. dry matter content")+
  xlab("Specific gravity")
fig_DMCSpecGra

ggsave("data/figures/DMCSpecGra.png", 
       plot = fig_DMCSpecGra,
       width = 6, 
       height = 4.5,  
       dpi = 300)


DMC_froz_reg <- lm(DMC_d ~ spec_grav_d, suivi)
summary(DMC_froz_reg)
plot(fitted(DMC_froz_reg),rstudent(DMC_froz_reg))

ggplot(suivi, aes(x = spec_grav_d, y = DMC_d)) +
  geom_point() +                       
  geom_smooth(method = "lm", se = TRUE)


#Values of frozen roots
suivi <- suivi %>% mutate(spec_grav = ifelse(is.na(spec_grav), spec_grav_d, spec_grav ))
suivi <- suivi %>% mutate(frozen = ifelse(is.na(volume_d), 0, 1))


mod_froz <- lm(spec_grav ~ frozen, suivi)
anova(mod_froz)

#correction due to water loss by freezing and thawing
emm_froz <- emmeans(mod_froz, ~ frozen)
pairs(emmeans(mod_froz, ~ frozen))
ratio_froz <- (emm_froz@bhat[2]+emm_froz@bhat[1]) / emm_froz@bhat[1] 
ratio_froz
suivi <- suivi %>% mutate(spec_grav = ifelse(frozen == 1, spec_grav/ratio_froz, spec_grav))

# Application of the formula
suivi <- suivi %>% mutate(DMC_pre = predict(DMC_reg,newdata = .))
suivi <- suivi %>% mutate(DM_harvest = PR*DMC_pre)





#Models
PR_DMC_mod <- lm(DMC_pre ~ PR, suivi)
summary(PR_DMC_mod)

GP_DMC_mod <- lm(DMC_pre ~ growth_period, suivi)

summary(GP_DMC_mod)

#Modelling Severité---------
DMC_sev_mod <- lm(DMC_pre ~ Severite_marqu*Severite + Severite_cum + Severite_cum_percent + growth_period, suivi)
summary(DMC_sev_mod)

DMC_sev_mod_step <- step(DMC_sev_mod)
summary(DMC_sev_mod_step)

DMC_sev_mod2 <- lm(DMC_pre ~  Severite_cum + Severite_cum_percent, suivi)
DMC_sev_mod2_step <- step(DMC_sev_mod2)
summary(DMC_sev_mod2_step)


#Modelling Cluster differences
DMC_var_mod <- lm(DMC_pre ~   cluster + Severite*Severite_marqu + Severite_cum + Severite_cum_percent + growth_period,suivi) 
check_model((DMC_var_mod))
shapiro.test(residuals(DMC_var_mod))
anova(DMC_var_mod)
DMC_var_mod_step <- step(DMC_var_mod)
check_model((DMC_var_mod_step))
plot(DMC_var_mod_step)

#DMC_var_mod_step <- lm(DMC_pre ~   cluster + Severite_marqu + growth_period,suivi) 
summary(DMC_var_mod_step)
anova(DMC_var_mod_step)
emm_DMC_var <- emmeans(DMC_var_mod_step, ~ cluster)
emm_DMC_var
pairs(emm_DMC_var)
cld_clustDMC <- cld(emm_DMC_var, Letters = letters)
cld_clustDMC

fig_DMCSevclustemm <- ggplot(as.data.frame(cld_clustDMC),
       aes(x = cluster,y=emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)+
  ylab("Rel. dry matter content (predicted from specific gravity)")+
  xlab("Cluster")
fig_DMCSevclustemm

ggsave("data/figures/DMCSeverityClustersemm.png", 
       plot = fig_DMCSevclustemm,
       width = 3.5, 
       height = 4.5,  
       dpi = 300)


#used
fig_DMCSevclust <- ggplot(suivi, aes(x = Severite_marqu, y = DMC_pre, color = cluster)) +
  geom_point() +
  geom_parallel_slopes(formula = y ~ x)+
  ylab("Rel. dry matter content (predicted from specific gravity)")+
  xlab("Severity at time of marking (%)")
fig_DMCSevclust

ggsave("data/figures/DMCSeverityClusters.png", 
       plot = fig_DMCSevclust,
       width = 6, 
       height = 4.5,  
       dpi = 300)



DMC_var_mod <- lm(DMC_pre ~ Type_manioc,suivi)
anova(DMC_var_mod)
emm_DMC_var <- emmeans(DMC_var_mod, ~ Type_manioc)
emm_DMC_var
pairs(emm_DMC_var)



