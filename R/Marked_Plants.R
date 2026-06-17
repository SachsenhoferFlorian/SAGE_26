library(lubridate)
plants = read.xlsx("data/raw/SuiviAccSAGE.xlsx", sheet = 2)
#Joining with variete
plants <- plants %>% left_join(variete, by = c("Code_Var" = "Code_var"))
plants$Severite_marqu <- mapping_severite[as.character(plants$Severite_marqu)]
plants$Severite_marqu <- as.numeric(plants$Severite_marqu)

plants$Date_enquete <- dmy(substr(plants$Date_enquete, 1, 10))
plants$Date_plante <- dmy(plants$Date_plante)
plants$growth_period <- plants$Date_enquete - plants$Date_plante
plants$growth_period_marqu <- as.numeric(plants$growth_period)


mod_Sev_clust <- lm(Severite_marqu ~ growth_period_marqu + cluster,plants)
anova(mod_Sev_clust)
summary(mod_Sev_clust)


emm_Sev_clust <- emmeans(mod_Sev_clust, ~ cluster)
emm_Sev_clust
pairs(emm_Sev_clust)
cld_Sev_clust <- cld(emm_Sev_clust, Letters = letters)
cld_Sev_clust


ggplot(as.data.frame(cld_Sev_clust),
       aes(x = cluster, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)

