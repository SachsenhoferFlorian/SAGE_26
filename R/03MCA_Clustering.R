#All_descriptors-----------

mca_data <- variete[, c("Code_var", "Commune", "Intercomm", "Farmer", "Type_manioc", "Couleur_feuilles_ap", "Pubescence", "Couleur_nervure", "Couleur_petiole", "Forme_lobes", "Nombre_lobes", "Couleur_tige", "Couleur_branches", "Ramification", "Forme_plante")]
res.mca <- MCA(mca_data,quali.sup = c(1,2,3,4), graph = FALSE)


fviz_mca_ind(res.mca, repel=TRUE)

fviz_mca_var(res.mca, repel=TRUE, invisible= "quali.sup")

fviz_mca_biplot(res.mca, repel=TRUE, invisible= "quali.sup")

res.mca$var$contrib

res.mca$var$coord

fviz_screeplot(res.mca, addlabels = TRUE)


coords <- res.mca$ind$coord
res.hcpc <- HCPC(res.mca, nb.clust = 5)
res.hcpc$desc.var
plot(res.hcpc)

mca_data_clustered <- res.hcpc$data.clust


ggplot(data=mca_data_clustered, aes(x=clust, fill=Commune)) +
  geom_bar()

ggplot(data=mca_data_clustered, aes(x=Commune, fill=clust)) +
  geom_bar()


ggplot(data=mca_data_clustered, aes(x=Farmer, fill=clust)) +
  geom_bar()
ggplot(data=mca_data_clustered, aes(x=clust, fill=Farmer)) +
  geom_bar()


ggplot(data=mca_data_clustered, aes(x=Commune, fill=Farmer)) +
  geom_bar()


ggplot(data=mca_data_clustered, aes(x=Farmer, fill=clust)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=mca_data_clustered, aes(x=clust, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=mca_data_clustered, aes(x=Farmer, fill=clust)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

ggplot(data=mca_data_clustered, aes(x=clust, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

#kmeans---------
set.seed(345)
kmeans_res <- kmeans(res.mca$ind$coord, centers = 5)
mca_data_clustered$clusters_kmeans <- kmeans_res$cluster

adjustedRandIndex(mca_data_clustered$clusters_kmeans, mca_data_clustered$clust)

#AHC---------------------
dist_mat <- dist(coords)
hc <- hclust(dist_mat, method = "ward.D2")
plot(hc)
mca_data_clustered$clusters_ahc <- cutree(hc, k = 5)
mca_data_clustered$clusters_ahc <- as.factor(mca_data_clustered$clusters_ahc)

ggplot(data=mca_data_clustered, aes(x=clusters_ahc, fill=Commune)) +
  geom_bar()

ggplot(data=mca_data_clustered, aes(x=Commune, fill=clusters_ahc)) +
  geom_bar()


ggplot(data=mca_data_clustered, aes(x=Farmer, fill=clusters_ahc)) +
  geom_bar()
ggplot(data=mca_data_clustered, aes(x=clusters_ahc, fill=Farmer)) +
  geom_bar()


ggplot(data=mca_data_clustered, aes(x=Commune, fill=Farmer)) +
  geom_bar()


ggplot(data=mca_data_clustered, aes(x=Farmer, fill=clusters_ahc)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=mca_data_clustered, aes(x=Farmer, fill=clusters_ahc)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

adjustedRandIndex(mca_data_clustered$clusters_ahc, mca_data_clustered$clust)

#usage clusters--------

usage_MCA <- variete[, c("Code_var", "Utilisation_bowo",	"Utilisation_cachiri",	"Utilisation_cassave",	"Utilisation_couac", "Utilisation_crabio", "Utilisation_domi_afiingi",	"Utilisation_sispa", "Utilisation_tapioca",	"Utilisation_cramanioc")]
usage_MCA[] <- lapply(usage_MCA, factor)
res.mca_usage <- MCA(usage_MCA,quali.sup = 1, graph = FALSE)

fviz_mca_ind(res.mca_usage, repel=TRUE)

fviz_mca_var(res.mca_usage, repel=TRUE, invisible= "quali.sup")

fviz_mca_biplot(res.mca_usage, repel=TRUE, invisible= "quali.sup")

#coords_usage <- res.mca_usage$ind$coord
res.hcpc_usage <- HCPC(res.mca_usage, nb.clust=-1)
plot(res.hcpc_usage)

mca_usage_clust <- res.hcpc_usage$data.clust

res.hcpc_usage$desc.var

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

#Comparison Maturity Variety Clusters---------
ggplot(data=variete, aes(x=clust_hcpc, fill=mature_class)) +
  geom_bar()

ggplot(data=variete, aes(x=mature_class, fill=clust_hcpc)) +
  geom_bar()

mat_var_tab <- table(variete$clust_hcpc,variete$mature_class)
chisq.test(mat_var_tab)
cramerV(mat_var_tab)

ggplot(data=variete, aes(x=clust_hcpc, fill=Mois_fin_recolte)) +
  geom_bar()

ggplot(data=variete, aes(x=Mois_fin_recolte, fill=clust_hcpc)) +
  geom_bar()

finrec_var_tab <- table(variete$clust_hcpc,variete$Mois_fin_recolte)
chisq.test(finrec_var_tab)
cramerV(finrec_var_tab)

finrec_nerv_tab <- table(variete$clust_hcpc,variete$Mois_fin_recolte)
chisq.test(finrec_nerv_tab)
cramerV(finrec_nerv_tab)


#ANOVA

modell_debut_rec <- lm( Mois_debut_recolte ~ clust_hcpc, data=variete) 
anova(modell_debut_rec)                                                         # not significant
summary(modell_debut_rec)
pairs(emmeans(modell_debut_rec, ~clust_hcpc))

variete$Mois_fin_recolte <- as.numeric(as.character(variete$Mois_fin_recolte))

modell_fin_rec <- lm( Mois_fin_recolte ~ clust_hcpc, data=variete)
anova(modell_fin_rec)                                                           # significant
summary(modell_fin_rec)
pairs(emmeans(modell_fin_rec, ~clust_hcpc))


ggplot(variete, aes(x = clust_hcpc, y = Mois_fin_recolte)) +
  geom_boxplot()

emm_df <- as.data.frame(emmeans(modell_fin_rec, ~ clust_hcpc))

ggplot(emm_df, aes(x = clust_hcpc, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(x = "Cluster", y = "Mois_fin_recolte")

modell_fin_rec_nerv <- lm( Mois_fin_recolte ~ Couleur_nervure, data=variete)
anova(modell_fin_rec_nerv)                                                           # significant
summary(modell_fin_rec_nerv)
emm_finrec_nerv <- emmeans(modell_fin_rec_nerv, ~Couleur_nervure)
emm_finrec_nerv
pairs(emm_finrec_nerv)

typ_nerv_tab <- table(variete$Couleur_nervure,variete$Type_manioc)
chisq.test(typ_nerv_tab)
cramerV(typ_nerv_tab)


modell_typ_fr <- lm( Mois_fin_recolte ~ Couleur_nervure +Type_manioc, data=variete)
anova(modell_typ_fr)                                                          
summary(modell_typ_fr)
emm_typ_fr <- emmeans(modell_typ_fr, ~Type_manioc)
emm_typ_fr
pairs(emm_typ_fr)

table(variete$Type_manioc, variete$Couleur_nervure)

variete$nervure_vert <- fct_other(variete$Couleur_nervure, keep = "vert")
typ_vert_tab <- table(variete$Type_manioc, variete$nervure_vert)
cramerV(typ_vert_tab)


#JACCARD similarity

mca_data <- mca_data %>%
  mutate(across(5:15, as.factor))
mca_data_sub <- select(mca_data,5:15)
mca_dummy <- dummy_cols(mca_data_sub, remove_selected_columns = TRUE)

dist_mat <- dist(mca_dummy, method = "Jaccard")
sim_mat <- 1- as.matrix(dist_mat)
rownames(sim_mat) <- mca_data$Code_var
colnames(sim_mat) <- mca_data$Code_var
sim_mat
sim_df <- as.data.frame(as.table(sim_mat))
sim_df <- sim_df %>% filter(Var1 != Var2) %>% arrange(desc(Freq))
head(sim_df, 50)
