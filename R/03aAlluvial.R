

variete_alluv <- variete %>%
  group_by(cluster5, clust_usage , Communaute) %>%
  summarise(n = n(), .groups = "drop")


#Alluvial diagram------

ggplot(variete_alluv,
       aes(axis1 = cluster5,
           axis2 = clust_usage,
           axis3 = Communaute,
           y = n)) +
  geom_alluvium(aes(fill = Communaute)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Clust_Variete", "Clust_Usage", "Communauté"))+
  theme_minimal()

