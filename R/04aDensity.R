suivi <- suivi %>% mutate(volume = masse_air - poids_eau)
suivi <- suivi %>% mutate(spec_grav = masse_air / volume)
suivi <- suivi %>% mutate(DMC = masse_seche / volume)


DMC_reg <- lm(DMC ~ spec_grav, suivi)
summary(DMC_reg)
plot(DMC_reg)

ggplot(suivi, aes(x = spec_grav, y = DMC)) +
  geom_point() +                       
  geom_smooth(method = "lm", se = TRUE)
