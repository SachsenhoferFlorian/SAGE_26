#All_descriptors-----------

mca_data <- variete[, c("Code_var", "Commune", "Intercomm", "Farmer", "Communaute", "Cultivation_depuis", "Type_manioc", "Couleur_feuilles_ap", "Pubescence", "Couleur_nervure", "Couleur_petiole", "Forme_lobes", "Nombre_lobes", "Couleur_tige", "Couleur_branches")]
res.mca <- MCA(mca_data,quali.sup = c(1,2,3,4,5,6), graph = FALSE)


fviz_mca_ind(res.mca, repel=TRUE)

fviz_mca_var(res.mca, repel=TRUE, invisible= "quali.sup")

fviz_mca_biplot(res.mca, repel=TRUE, invisible= "quali.sup")

res.mca$var$contrib

res.mca$var$coord

fviz_screeplot(res.mca, addlabels = TRUE)

res.hcpc5 <- HCPC(res.mca, nb.clust = -1) #Clustering with manual choice (5)
res.hcpc5$desc.var
plot(res.hcpc5)

mca_data_clustered <- res.hcpc5$data.clust
variete$cluster5 <- res.hcpc5$data.clust$clust

table(variete$cluster5, mca_data_clustered$clust) #Check if clusters were attributed well




ggplot(data=variete, aes(x=cluster5, fill=Commune)) +
  geom_bar()
ggplot(data=variete, aes(x=Commune, fill=cluster5)) +
  geom_bar()


ggplot(data=variete, aes(x=Farmer, fill=cluster5)) +
  geom_bar()
ggplot(data=variete, aes(x=cluster5, fill=Farmer)) +
  geom_bar()


ggplot(data=variete, aes(x=Commune, fill=Farmer)) +
  geom_bar()


ggplot(data=variete, aes(x=Farmer, fill=cluster5)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=variete, aes(x=cluster5, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=variete, aes(x=Farmer, fill=cluster5)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

ggplot(data=variete, aes(x=cluster5, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

ggplot(data=variete, aes(x=Communaute, fill=cluster5)) +
  geom_bar()
ggplot(data=variete, aes(x=cluster5, fill=Communaute)) +
  geom_bar()

table(mca_data_clustered$clust,mca_data_clustered$Type_manioc)

#Cross table
mca_data_clustered$cluster5 <- mca_data_clust
trait_names <- colnames(mca_data_clustered)
trait_names <- trait_names[-c(1,2,3,4,16)]
cluster_freq <- variete %>%
  pivot_longer(cols = all_of(trait_names), names_to = "Trait", values_to = "Value") %>%
  group_by(cluster5, Trait, Value) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  pivot_wider(names_from = c(Trait, Value), values_from = Freq, values_fill = 0)
cluster_freq

lapply(trait_names, function(trait) table(variete$cluster5, variete[[trait]]))

#Analyse Cultivation depuis----------
variete$Cultivation_depuis <- factor(variete$Cultivation_depuis, levels = c("0-5" ,
                                                                            "5-10" ,
                                                                            "10-15",
                                                                            "15-20",
                                                                            "20"))


ggplot(data=variete, aes(x= Cultivation_depuis, fill=cluster5)) +
  geom_bar()
ggplot(data=variete, aes(x=cluster5, fill=Cultivation_depuis)) +
  geom_bar()



ggplot(data=variete, aes(x= Cultivation_depuis, fill=Communaute)) +
  geom_bar()
ggplot(data=variete, aes(x=Communaute, fill=Cultivation_depuis)) +
  geom_bar()


mapping_Cultiv <- c(
  "0-5" = "2.5",
  "5-10" = "7.5",
  "10-15" = "12.5",
  "15-20" = "17.5",
  "20" = "25"
)
variete$Cultiv_num <- as.numeric(mapping_Cultiv[as.character(variete$Cultivation_depuis)])

mod_ComCult <- lm(data= variete, Cultiv_num ~ Communaute)
anova(mod_ComCult)
summary(mod_ComCult)
emm_MCoC <- emmeans(mod_ComCult, ~ Communaute)
pairs(emm_MCoC)
cld_MCoC <- cld(emm_MCoC, Letters = letters)

mod_cluster5Cult <- lm(data= variete, Cultiv_num ~ cluster5)
anova(mod_cluster5Cult)
summary(mod_cluster5Cult)
emm_MCC <- emmeans(mod_cluster5Cult, ~ cluster5)
pairs(emm_MCC)
cld_MCC <- cld(emm_MCC, Letters = letters)


ggplot(variete, aes(x = Communaute, y = Cultiv_num)) +
  geom_boxplot()


ggplot(as.data.frame(cld_MCoC),
       aes(x = Communaute, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)

emm_MCC_df <-as.data.frame(cld_MCC)
ggplot(emm_MCC_df,
       aes(x = cluster5, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_text(aes(label= .group, y = upper.CL), size = 6)

#Analysis with automatic clusters------------

res.hcpc3 <- HCPC(res.mca, nb.clust = -1)   #Clustering with automatic cluster choice 
res.hcpc3$desc.var
plot(res.hcpc3)

variete$cluster3 <- res.hcpc3$data.clust$clust

ggplot(data=variete, aes(x=cluster3, fill=Commune)) +
  geom_bar()
ggplot(data=variete, aes(x=Commune, fill=cluster3)) +
  geom_bar()


ggplot(data=variete, aes(x=Farmer, fill=cluster3)) +
  geom_bar()
ggplot(data=variete, aes(x=cluster3, fill=Farmer)) +
  geom_bar()


ggplot(data=variete, aes(x=Commune, fill=Farmer)) +
  geom_bar()


ggplot(data=variete, aes(x=Farmer, fill=cluster3)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=variete, aes(x=cluster3, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=variete, aes(x=Farmer, fill=cluster3)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

ggplot(data=variete, aes(x=cluster3, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

ggplot(data=variete, aes(x=Communaute, fill=cluster3)) +
  geom_bar()
ggplot(data=variete, aes(x=cluster3, fill=Communaute)) +
  geom_bar()

#Analyse Cultivation depuis----------
variete$Cultivation_depuis <- factor(variete$Cultivation_depuis, levels = c("0-5" ,
                                                                            "5-10" ,
                                                                            "10-15",
                                                                            "15-20",
                                                                            "20"))


ggplot(data=variete, aes(x= Cultivation_depuis, fill=cluster3)) +
  geom_bar()
ggplot(data=variete, aes(x=cluster3, fill=Cultivation_depuis)) +
  geom_bar()



ggplot(data=variete, aes(x= Cultivation_depuis, fill=Communaute)) +
  geom_bar()
ggplot(data=variete, aes(x=Communaute, fill=Cultivation_depuis)) +
  geom_bar()


mapping_Cultiv <- c(
  "0-5" = "2.5",
  "5-10" = "7.5",
  "10-15" = "12.5",
  "15-20" = "17.5",
  "20" = "25"
)
variete$Cultiv_num <- as.numeric(mapping_Cultiv[as.character(variete$Cultivation_depuis)])

mod_ComCult <- lm(data= variete, Cultiv_num ~ Communaute)
anova(mod_ComCult)
summary(mod_ComCult)
emm_MCoC <- emmeans(mod_ComCult, ~ Communaute)
pairs(emm_MCoC)
cld_MCoC <- cld(emm_MCoC, Letters = letters)

mod_cluster3Cult <- lm(data= variete, Cultiv_num ~ cluster3)
anova(mod_cluster3Cult)
summary(mod_cluster3Cult)
emm_MCC <- emmeans(mod_cluster3Cult, ~ cluster3)
pairs(emm_MCC)
cld_MCC <- cld(emm_MCC, Letters = letters)



ggplot(variete, aes(x = Communaute, y = Cultiv_num)) +
  geom_boxplot()


ggplot(as.data.frame(cld_MCoC),
       aes(x = Communaute, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)

emm_MCC_df <-as.data.frame(cld_MCC)
ggplot(emm_MCC_df,
       aes(x = cluster3, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_text(aes(label= .group, y = upper.CL), size = 6)


#kmeans---------
set.seed(345)
kmeans_res <- kmeans(res.mca$ind$coord, centers = 5)
mca_data_clustered$clusters_kmeans <- kmeans_res$cluster

adjustedRandIndex(mca_data_clustered$clusters_kmeans, mca_data_clustered$clust)

#AHC---------------------
dist_mat <- dist(res.mca$ind$coord)
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
res.mca_usage$eig
fviz_mca_ind(res.mca_usage, repel=TRUE)

fviz_mca_var(res.mca_usage, repel=TRUE, invisible= "quali.sup")

fviz_mca_biplot(res.mca_usage, repel=TRUE, invisible= "quali.sup")

#coords_usage <- res.mca_usage$ind$coord
res.hcpc_usage <- HCPC(res.mca_usage, nb.clust=-1)
plot(res.hcpc_usage)

mca_usage_clust <- res.hcpc_usage$data.clust

res.hcpc_usage$desc.var

#Comparison Usage Cluster Communauté----------

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
mca_data_sub <- dplyr::select(mca_data,5:15)
mca_dummy <- dummy_cols(mca_data_sub, remove_selected_columns = TRUE)

dist_mat <- dist(mca_dummy, method = "Jaccard")
sim_mat <- 1- as.matrix(dist_mat)
rownames(sim_mat) <- mca_data$Code_var
colnames(sim_mat) <- mca_data$Code_var
sim_mat
sim_df <- as.data.frame(as.table(sim_mat))
sim_df <- sim_df %>% filter(Var1 != Var2) %>% arrange(desc(Freq))
head(sim_df, 50)

