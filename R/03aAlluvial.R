variete_alluv <- variete %>%
  group_by(cluster5, clust_usage , Communaute) %>%
  summarise(n = n(), .groups = "drop")


#Alluvial diagram------

ggplot(variete_alluv,
       aes(axis1 = cluster5,
           axis2 = Communaute,
           axis3 = clust_usage,
           y = n)) +
  geom_alluvium(aes(fill = Communaute)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Clust_Variete", "Clust_Usage", "Communauté"))+
  theme_minimal()


#Manual Usage groups-------------

variete_alluv1 <- variete %>%
  group_by(cluster5, groupUsage , Communaute) %>%
  summarise(n = n(), .groups = "drop")


#Alluvial diagram------

ggplot(variete_alluv1,
       aes(axis1 = cluster5,
           axis2 = Communaute,
           axis3 = groupUsage,
           y = n)) +
  geom_alluvium(aes(fill = Communaute)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Clust_Variete", "Clust_Usage", "Communauté"))+
  theme_minimal()



# ---- Data preparation: explode dummy variables + fractional weights ----

variete_alluv2 <- variete %>%  mutate(id = row_number()) %>%
  pivot_longer(
    cols = starts_with("Utilisation_"),
    names_to = "usage",
    values_to = "active"
  ) %>%
  filter(active == 1) %>%
  group_by(id) %>%
  mutate(weight = 1 / n()) %>%
  ungroup() %>%
  group_by(cluster5, usage, Communaute) %>%
  summarise(n = sum(weight), .groups = "drop")

# ---- Alluvial plot ----

ggplot(variete_alluv2,
       aes(axis1 = cluster5,
           axis2 = Communaute,
           axis3 = usage,
           y = n)) +
  geom_alluvium(aes(fill = Communaute), alpha = 0.8) +
  geom_stratum(width = 0.3) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Clust_Variete",  "Communauté", "Usages")) +
  theme_minimal() +
  labs(
    title = "Alluvial Diagram Variety -> Communauté -> Usage",
    y = "weighted count"
  )
