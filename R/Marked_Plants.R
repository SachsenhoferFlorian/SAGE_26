plants = read.xlsx("data/raw/Plantes_marquAccSAGE.xlsx", sheet = 1)
#Joining with variete
plants <- plants %>% left_join(variete, by = c("Code_Var" = "Code_var"))
plants$Severite_marqu <- mapping_severite[as.character(plants$Severite_marqu)]
plants$Severite_marqu <- as.numeric(plants$Severite_marqu)

plants$Date_enquete <- dmy(substr(plants$Date_enquete, 1, 10))
plants$Date_plante <- dmy(plants$Date_plante.x)
plants$growth_period <- plants$Date_enquete - plants$Date_plante
plants$growth_period_marqu <- as.numeric(plants$growth_period)


mod_Sev_clust <- lm(Severite_marqu ~ growth_period_marqu + cluster,plants)
anova(mod_Sev_clust)
summary(mod_Sev_clust)

plants$Severite_marqu1 <- plants$Severite_marqu / 100
mod_Sev_clust <- glm(Severite_marqu1 ~ growth_period_marqu + cluster,
                     family = quasibinomial(link = "logit"),
                     data = plants)
anova(mod_Sev_clust)
summary(mod_Sev_clust)
deviance(mod_Sev_clust) / df.residual(mod_Sev_clust)


emm_Sev_clust <- emmeans(mod_Sev_clust, ~ cluster,type="response")
emm_Sev_clust
pairs(emm_Sev_clust)
cld_Sev_clust <- cld(emm_Sev_clust, Letters = letters)
cld_Sev_clust


fig_SevClust <- ggplot(as.data.frame(cld_Sev_clust),
       aes(x = cluster, y = prob)) +
  geom_col() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2)+
  labs(x = "Cluster", y = "Severity at the time of marking (%) (adjusted means)")
fig_SevClust 

ggsave("data/figures/SevClust.png", 
       plot = fig_SevClust ,
       width = 4.5, 
       height = 4.5,  
       dpi = 300)
