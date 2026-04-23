#All descriptors-----------

mca_data <- variete[, c("Code_var", "Commune", "Intercomm", "Farmer", "Communaute", "Kramanioc", "Cultivation_depuis", "Type_manioc", "Couleur_feuilles_ap", "Pubescence", "Couleur_nervure", "Couleur_petiole", "Forme_lobes", "Nombre_lobes", "Couleur_tige", "Couleur_branches","Ramification", "Forme_plante")]

mca_data_kra <- filter(mca_data, Kramanioc == 1)
mca_data_amer <- filter(mca_data, Kramanioc == 0)

res.mca_amer <- MCA(mca_data_amer,quali.sup = c(1,2,3,4,5,6,7), graph = FALSE)


fviz_mca_ind(res.mca_amer, repel=TRUE)

fviz_mca_var(res.mca_amer, repel=TRUE, invisible= "quali.sup")

fviz_mca_biplot(res.mca_amer, repel=TRUE, invisible= "quali.sup")

res.mca_amer$var$contrib

res.mca_amer$var$coord

fviz_screeplot(res.mca_amer, addlabels = TRUE)

res.hcpc5 <- HCPC(res.mca_amer, nb.clust = -1) #Clustering with manual choice (5)
res.hcpc5$desc.var
plot(res.hcpc5)

mca_data_amer_cla <- res.hcpc5$data.clust
mca_data_amer_cla$cluster5 <- mca_data_amer_cla$clust


ggplot(data=mca_data_amer_cla, aes(x=cluster5, fill=Commune)) +
  geom_bar()
ggplot(data=mca_data_amer_cla, aes(x=Commune, fill=cluster5)) +
  geom_bar()


ggplot(data=mca_data_amer_cla, aes(x=Farmer, fill=cluster5)) +
  geom_bar()
ggplot(data=mca_data_amer_cla, aes(x=cluster5, fill=Farmer)) +
  geom_bar()


ggplot(data=mca_data_amer_cla, aes(x=Commune, fill=Farmer)) +
  geom_bar()


ggplot(data=mca_data_amer_cla, aes(x=Farmer, fill=cluster5)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=mca_data_amer_cla, aes(x=cluster5, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=mca_data_amer_cla, aes(x=Farmer, fill=cluster5)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")


ggplot(data=mca_data_amer_cla, aes(x=cluster5, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

ggplot(data=mca_data_amer_cla, aes(x=Communaute, fill=cluster5)) +
  geom_bar()
ggplot(data=mca_data_amer_cla, aes(x=cluster5, fill=Communaute)) +
  geom_bar()

table(mca_data_clustered$cluster5,mca_data_clustered)

#Cross table
trait_names <- colnames(mca_data_clustered)
trait_names <- trait_names[-c(1,2,3,4,18)]
cluster_freq <- mca_data_amer_cla %>%
  pivot_longer(cols = all_of(trait_names), names_to = "Trait", values_to = "Value") %>%
  group_by(cluster5, Trait, Value) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  pivot_wider(names_from = c(Trait, Value), values_from = Freq, values_fill = 0)
cluster_freq

lapply(trait_names, function(trait) table(mca_data_amer_cla$cluster5, mca_data_amer_cla[[trait]]))


variete$clust_kra <- mca_data_amer_cla$clust[match(variete$Code_var, mca_data_amer_cla$Code_var)]
variete <- variete %>% mutate(clust_kra = if_else(Kramanioc == 1, "kra",as.character(clust_kra)))
variete$clust_kra <- as.factor(variete$clust_kra)

#Analyse Cultivation depuis----------
variete$Cultivation_depuis <- factor(variete$Cultivation_depuis, levels = c("0-5","5-10","10-15","15-20","20"))

ggplot(data=variete, aes(x= Cultivation_depuis, fill=clust_kra)) +
  geom_bar()
ggplot(data=mca_data_amer_cla, aes(x=clust_kra, fill=Cultivation_depuis)) +
  geom_bar()



ggplot(data=mca_data_amer_cla, aes(x= Cultivation_depuis, fill=Communaute)) +
  geom_bar()
ggplot(data=mca_data_amer_cla, aes(x=Communaute, fill=Cultivation_depuis)) +
  geom_bar()


mapping_Cultiv <- c(
  "0-5" = "2.5",
  "5-10" = "7.5",
  "10-15" = "12.5",
  "15-20" = "17.5",
  "20" = "25"
)
mca_data_amer_cla$Cultiv_num <- as.numeric(mapping_Cultiv[as.character(mca_data_amer_cla$Cultivation_depuis)])

mod_ComCult <- lm(data= mca_data_amer_cla, Cultiv_num ~ Communaute)
anova(mod_ComCult)
summary(mod_ComCult)
emm_MCoC <- emmeans(mod_ComCult, ~ Communaute)
pairs(emm_MCoC)
cld_MCoC <- cld(emm_MCoC, Letters = letters)
er
mod_cluster5Cult <- lm(data= mca_data_amer_cla, Cultiv_num ~ cluster5)
anova(mod_cluster5Cult)
summary(mod_cluster5Cult)
emm_MCC <- emmeans(mod_cluster5Cult, ~ cluster5)
pairs(emm_MCC)
cld_MCC <- cld(emm_MCC, Letters = letters)


ggplot(mca_data_amer_cla, aes(x = Communaute, y = Cultiv_num)) +
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

#Analyse kramanioc--------


res.mca_kra <- MCA(mca_data_kra,quali.sup = c(1,2,3,4,5,6,7), graph = FALSE)


fviz_mca_ind(res.mca_kra, repel=TRUE)

fviz_mca_var(res.mca_kra, repel=TRUE, invisible= "quali.sup")

fviz_mca_biplot(res.mca_kra, repel=TRUE, invisible= "quali.sup")

res.mca_kra$var$contrib

res.mca_kra$var$coord

fviz_screeplot(res.mca_kra, addlabels = TRUE)

res.hcpc5 <- HCPC(res.mca_kra, nb.clust = -1) #Clustering with manual choice (5)
res.hcpc5$desc.var
plot(res.hcpc5)

#Analysis of both-----------------

ggplot(data=variete, aes(x=clust_kra, fill=Commune)) +
  geom_bar()
ggplot(data=variete, aes(x=Commune, fill=clust_kra)) +
  geom_bar()


ggplot(data=variete, aes(x=Farmer, fill=clust_kra)) +
  geom_bar()
ggplot(data=variete, aes(x=clust_kra, fill=Farmer)) +
  geom_bar()


ggplot(data=variete, aes(x=Commune, fill=Farmer)) +
  geom_bar()


ggplot(data=variete, aes(x=Farmer, fill=clust_kra)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=variete, aes(x=clust_kra, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=variete, aes(x=Farmer, fill=clust_kra)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

ggplot(data=variete, aes(x=clust_kra, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

ggplot(data=variete, aes(x=Communaute, fill=clust_kra)) +
  geom_bar()
ggplot(data=variete, aes(x=clust_kra, fill=Communaute)) +
  geom_bar()

table(mca_data_clustered$clust_kra,mca_data_clustered)

#Cross table
trait_names <- colnames(mca_data_clustered)
trait_names <- trait_names[-c(1,2,3,4,18)]
cluster_freq <- variete %>%
  pivot_longer(cols = all_of(trait_names), names_to = "Trait", values_to = "Value") %>%
  group_by(clust_kra, Trait, Value) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  pivot_wider(names_from = c(Trait, Value), values_from = Freq, values_fill = 0)
cluster_freq

lapply(trait_names, function(trait) table(variete$clust_kra, variete[[trait]]))




#usage clusters--------

usage_MCA <- variete[, c("Code_var", "Kramanioc", 	"Utilisation_cachiri",	"Utilisation_cassave",	"Utilisation_couac", "Utilisation_crabio", "Utilisation_domi_afiingi",	"Utilisation_sispa", "Utilisation_tapioca")]
usage_MCA <- usage_MCA %>% filter(Kramanioc == 0)
usage_MCA[] <- lapply(usage_MCA, factor)
usage_MCA <- usage_MCA [,-2]
res.mca_usage <- MCA(usage_MCA,quali.sup = c(1,2), graph = FALSE)
res.mca_usage$eig
fviz_mca_ind(res.mca_usage, repel=TRUE)

fviz_mca_var(res.mca_usage, repel=TRUE, invisible= "quali.sup")

fviz_mca_biplot(res.mca_usage, repel=TRUE, invisible= "quali.sup")


res.hcpc_usage <- HCPC(res.mca_usage, nb.clust=4)
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

modell_debut_rec <- lm( Mois_debut_recolte ~ clust_kra, data=variete) 
anova(modell_debut_rec)                                                         # not significant
summary(modell_debut_rec)
pairs(emmeans(modell_debut_rec, ~clust_kra))

variete$Mois_fin_recolte <- as.numeric(as.character(variete$Mois_fin_recolte))

modell_fin_rec <- lm( Mois_fin_recolte ~ clust_kra, data=variete)
anova(modell_fin_rec)                                                           # significant
summary(modell_fin_rec)
pairs(emmeans(modell_fin_rec, ~clust_kra))


ggplot(variete, aes(x = clust_kra, y = Mois_fin_recolte)) +
  geom_boxplot()

emm_df <- as.data.frame(emmeans(modell_fin_rec, ~ clust_kra))

ggplot(emm_df, aes(x = clust_kra, y = emmean)) +
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
