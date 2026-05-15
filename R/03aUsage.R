

#usage clusters--------

usage_MCA <- variete[, c("Code_var", "Utilisation_bowo",	"Utilisation_cachiri",	"Utilisation_cassave",	"Utilisation_couac", "Utilisation_crabio", "Utilisation_domi_afiingi",	"Utilisation_sispa", "Utilisation_tapioca",	"Utilisation_cramanioc")]
usage_MCA[] <- lapply(usage_MCA, factor)
res.mca_usage <- MCA(usage_MCA,quali.sup = 1, graph = FALSE)
res.mca_usage$eig
fviz_mca_ind(res.mca_usage, repel=TRUE)

fviz_mca_var(res.mca_usage, repel=TRUE, invisible= "quali.sup")

fviz_mca_biplot(res.mca_usage, repel=TRUE, invisible= "quali.sup")


res.hcpc_usage <- HCPC(res.mca_usage, nb.clust=6)
plot(res.hcpc_usage)

mca_usage_clust <- res.hcpc_usage$data.clust

res.hcpc_usage$desc.var

variete$clust_usage <- mca_usage_clust$clust


#Manual Clustering Usage------------------------
#Kramanioc
variete$groupUsage <- if_else(variete$Kramanioc == 1 , "Kra-manioc", NA)
#Simplecouac etc.
variete <- variete %>% mutate(groupUsage = if_else(Utilisation_couac == 1 & Kramanioc == 0 , "Couac+", groupUsage ))
#Cassave without couac
variete <- variete %>% mutate(groupUsage = if_else(Utilisation_couac == 1 & Utilisation_cassave == 0 , "Cassave w/Couac", groupUsage ))
#Cachiri
variete <- variete %>% mutate(groupUsage = if_else(Utilisation_cachiri == 1 , "Cachiri+", groupUsage ))
#DomiAffingi+Cassave+couac
variete <- variete %>% mutate(groupUsage = if_else(Utilisation_domi_afiingi == 1 & Kramanioc ==0, "Couac/Cassave/Domi", groupUsage ))



#Comparison Usage Variety Clusters--------
variete$clust_hcpc <- mca_data_clustered$clust[match(variete$Code_var, mca_data_clustered$Code_var)]
variete$clust_usage <- mca_usage_clust$clust[match(variete$Code_var, mca_usage_clust$Code_var)]

adjustedRandIndex(variete$clust_hcpc, variete$clust_usage)

usage_var_tab <- table(variete$clust_hcpc,variete$clust_usage)
chisq.test(usage_var_tab)
cramerV(usage_var_tab)

usage_typ_tab <- table(variete$Type_manioc,variete$clust_usage)
chisq.test(usage_typ_tab)
cramerV(usage_typ_tab)

ggplot(data=variete, aes(x=clust_hcpc, fill=clust_usage)) +
  geom_bar()

ggplot(data=variete, aes(x=clust_usage, fill=clust_hcpc)) +
  geom_bar()

#Comparison Usage Cluster Communauté----------

ggplot(data=variete, aes(x=Communaute, fill=clust_usage)) +
  geom_bar()
ggplot(data=variete, aes(x=clust_usage, fill=Communaute)) +
  geom_bar()
tab_cluus_comm <- table(variete$clust_usage, variete$Communaute)
chisq.test(tab_cluus_comm)

utili_names <-c("Utilisation_bowo",	"Utilisation_cachiri",	"Utilisation_cassave",	"Utilisation_couac", "Utilisation_crabio", "Utilisation_domi_afiingi",	"Utilisation_sispa", "Utilisation_tapioca",	"Utilisation_cramanioc")
lapply(utili_names, function(utili){
  cat("\nVariable", utili, "\n")
  print(table(variete$Communaute, variete[[utili]]))
})





variete_alluv <- variete %>%
  group_by(cluster, clust_usage , Communaute) %>%
  summarise(n = n(), .groups = "drop")


#Alluvial diagram------

ggplot(variete_alluv,
       aes(axis1 = cluster,
           axis2 = Communaute,
           axis3 = clust_usage,
           y = n)) +
  geom_alluvium(aes(fill = cluster)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Clust_Variete", "Clust_Usage", "Communauté"))+
  theme_minimal()


#Manual Usage groups-------------

variete_alluv1 <- variete %>%
  group_by(cluster, groupUsage , Communaute) %>%
  summarise(n = n(), .groups = "drop")


#Alluvial diagram------

ggplot(variete_alluv1,
       aes(axis1 = cluster,
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
  group_by(cluster, usage, Communaute) %>%
  summarise(n = sum(weight), .groups = "drop")


variete_alluv2 <- variete_alluv2 %>%
  mutate(usage = factor(
    case_when(
      usage == "Utilisation_bowo" ~ "Bowo",
      usage == "Utilisation_sispa" ~ "Sispa",
      usage == "Utilisation_couac" ~ "Couac",
      usage == "Utilisation_cachiri" ~ "Cachiri",
      usage == "Utilisation_cassave" ~ "Cassave",
      usage == "Utilisation_crabio" ~ "Crabio",
      usage == "Utilisation_cramanioc" ~ "Kramanioc",
      usage == "Utilisation_tapioca" ~ "Tapioca",
      usage == "Utilisation_domi_afiingi" ~ "Domi"
    ),
    levels = c("Cachiri", "Cassave", "Bowo", "Sispa", "Domi", "Tapioca", "Crabio","Couac","Kramanioc")
  ))
# ---- Alluvial plot ----

ggplot(variete_alluv2,
       aes(axis1 = cluster,
           axis2 = Communaute,
           axis3 = usage,
           y = n)) +
  geom_alluvium(aes(fill = cluster), alpha = 0.8, lode.guidance = "forward") +
  geom_stratum(width = 0.3) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Clust_Variete",  "Communauté", "Usages")) +
  theme_minimal() +
  labs(
    title = "Alluvial Diagram Variety -> Communauté -> Usage",
    y = "weighted count"
  )
