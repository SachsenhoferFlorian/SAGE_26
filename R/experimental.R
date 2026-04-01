
#Shannon--------

#Diversity descriptors
counts_list <- lapply(mca_data,table)
div_descript <- sapply(counts_list, diversity, index = "shannon")
div_table <- enframe(div_descript, name = "group", value = "shannon")

cluster_model <- lm(Mois_fin_recolte ~ Type_manioc + Couleur_feuilles_ap + Pubescence + Couleur_nervure + Couleur_petiole + Forme_lobes + Nombre_lobes + Couleur_tige + Couleur_branches + Ramification + Forme_plante, data= variete)
anova(cluster_model)
summary(cluster_model)

#Diversity in farmers and communes

shannon_per_commune <- variete %>%
  dplyr::count(Commune, Code_var) %>%
  group_by(Commune) %>%
  summarise(shannon = diversity(n, index = "shannon"))

shannon_farmer <- variete %>%
  dplyr::count(Commune, Farmer, Code_var) %>%
  group_by(Commune, Farmer) %>%
  summarise(
    shannon_farmer = diversity(n),
    .groups = "drop"
  )

shannon_commune <- shannon_farmer %>%
  group_by(Commune) %>%
  summarise(
    alpha = mean(shannon_farmer)
  )

gamma_commune <- variete %>%
  dplyr::count(Commune, Code_var) %>%
  group_by(Commune) %>%
  summarise(
    gamma = diversity(n),
    .groups = "drop"
  )

diversity_partition <- left_join(shannon_commune, gamma_commune, by = "Commune") %>%
  mutate(beta = gamma - alpha)


#Hill numbers


# Gamma (Commune-Ebene, effektiv)
gamma_commune <- variete %>%
  dplyr::count(Commune, Code_var) %>%
  group_by(Commune) %>%
  summarise(
    gamma = exp(diversity(n)),
    .groups = "drop"
  )

# Alpha (Farmer-Ebene → Mittelwert)
shannon_farmer <- variete %>%
  dplyr::count(Commune, Farmer, Code_var) %>%
  group_by(Commune, Farmer) %>%
  summarise(
    hill_farmer = exp(diversity(n)),
    .groups = "drop"
  )

alpha_commune <- shannon_farmer %>%
  group_by(Commune) %>%
  summarise(
    alpha = mean(hill_farmer),
    .groups = "drop"
  )

# Beta (multiplikativ!)
diversity_partition <- left_join(alpha_commune, gamma_commune, by = "Commune") %>%
  mutate(
    beta = gamma / alpha
  )

#Selected Descriptors------------


mca_data_1 <- variete[, c("Type_manioc","Couleur_feuille", "Couleur_nervure", "Couleur_petiole", "Couleur_branches", "Couleur_tige", "Forme_lobes", "Nombre_lobes")]
res.mca_1 <- MCA(mca_data_1, graph = FALSE)
fviz_mca_ind(res.mca_1)
fviz_mca_var(res.mca_1)


coords_1 <- res.mca_1$ind$coord
res.hcpc_1 <- HCPC(res.mca_1, nb.clust = -1)
res.hcpc_1$desc.var

res.hcpc_1$call$t$nb.clust
plot(res.hcpc_1)

