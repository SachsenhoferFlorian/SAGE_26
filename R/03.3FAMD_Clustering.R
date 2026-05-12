#MCA and HCPC-----------
variete <- variete %>% mutate(Couleur_feuilles_ap = factor(Couleur_feuilles_ap, levels = c("vert_clair","vert_fonce","vert_violace","violet"), ordered =TRUE))
variete <- variete %>% mutate(Couleur_petiole = factor(Couleur_petiole, levels = c("vert","vert_jaunatre","vert_rougeatre","rougeatre-vert","rougeatre","violet"), ordered =TRUE))
variete <- variete %>% mutate(Couleur_nervure = factor(Couleur_nervure, levels = c("vert","rougeatre_minorite","rougeatre_majorite","rouge"), ordered =TRUE))
variete <- variete %>% mutate(Couleur_branches = factor(Couleur_branches, levels = c("vert","vert_violet","violet"), ordered =TRUE))
variete <- variete %>% mutate(Nombre_lobes = factor(Nombre_lobes, levels = c("cinq","sept","neuf"), ordered =TRUE))

variete <- variete %>% mutate(
Couleur_feuilles_ap_n = as.numeric(Couleur_feuilles_ap),
Couleur_petiole_n = as.numeric(Couleur_petiole),
Couleur_nervure_n = as.numeric(Couleur_nervure),
Couleur_branches_n = as.numeric(Couleur_branches),
Nombre_lobes_n = as.numeric(Nombre_lobes),
)

famd_data <- variete[, c("Code_var", "Commune", "Intercomm", "Farmer", "Communaute", "Cultivation_depuis", "Type_manioc", "Couleur_feuilles_ap_n", "Pubescence", "Couleur_nervure_n", "Couleur_petiole_n", "Forme_lobes", "Nombre_lobes_n", "Couleur_tige", "Couleur_branches_n","Ramification")]
res.famd <- FAMD(famd_data, sup.var = c(1,2,3,4,5,6), graph = FALSE)

res.famd$var$contrib
res.famd$var$coord

fviz_screeplot(res.famd, addlabels = TRUE)

res.hcpc5 <- HCPC(res.famd, nb.clust = 4) #Clustering with manual choice (5)
res.hcpc5$desc.var

mca_data_clustered <- res.hcpc5$data.clust
variete$clusterFAMD <- res.hcpc5$data.clust$clust

suivi <- suivi %>% left_join(variete %>% dplyr::select(Code_var, clusterFAMD),by = c("Code_Var" ="Code_var"))

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

table(variete$cluster5,variete$Type_manioc)





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


#Analyse Cultivation depuis cluster5----------
variete$Cultivation_depuis <- factor(variete$Cultivation_depuis, levels = c("0-5" , "5-10" ,"10-15","15-20","20"))
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

ggplot(variete, aes(x = Communaute, y = Cultiv_num)) +
  geom_boxplot()

mod_ComCult <- lm(data= variete, Cultiv_num ~ Communaute)
anova(mod_ComCult)
summary(mod_ComCult)
emm_MCoC <- emmeans(mod_ComCult, ~ Communaute)
pairs(emm_MCoC)
cld_MCoC <- cld(emm_MCoC, Letters = letters)

ggplot(as.data.frame(cld_MCoC),
       aes(x = Communaute, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_text(aes(label= .group, y = upper.CL), size = 6)

mod_cluster5Cult <- lm(data= variete, Cultiv_num ~ cluster5)
anova(mod_cluster5Cult)
summary(mod_cluster5Cult)
emm_MCC <- emmeans(mod_cluster5Cult, ~ cluster5)
pairs(emm_MCC)
cld_MCC <- cld(emm_MCC, Letters = letters)
emm_MCC_df <-as.data.frame(cld_MCC)

ggplot(emm_MCC_df,
       aes(x = cluster5, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_text(aes(label= .group, y = upper.CL), size = 6)



#Comparison Maturity Variety Clusters---------
ggplot(data=variete, aes(x=cluster5, fill=mature_class)) +
  geom_bar()

ggplot(data=variete, aes(x=mature_class, fill=cluster5)) +
  geom_bar()

mat_var_tab <- table(variete$cluster5,variete$mature_class)
chisq.test(mat_var_tab)
cramerV(mat_var_tab)

ggplot(data=variete, aes(x=cluster5, fill=Mois_fin_recolte)) +
  geom_bar()

ggplot(data=variete, aes(x=Mois_fin_recolte, fill=cluster5)) +
  geom_bar()

finrec_var_tab <- table(variete$cluster5,variete$Mois_fin_recolte)
chisq.test(finrec_var_tab)
cramerV(finrec_var_tab)

finrec_nerv_tab <- table(variete$Couleur_nervure,variete$Mois_fin_recolte)
chisq.test(finrec_nerv_tab)
cramerV(finrec_nerv_tab)


#Models Maturity Clusters----------

model_debut_rec <- lm( Mois_debut_recolte ~ cluster5, data=variete) 
anova(model_debut_rec)                                                         #significant
summary(model_debut_rec)
pairs(emmeans(model_debut_rec, ~cluster5))

variete$Mois_fin_recolte <- as.numeric(as.character(variete$Mois_fin_recolte))

model_fin_rec <- lm( Mois_fin_recolte ~ cluster5, data=variete)
anova(model_fin_rec)                                                           # significant
summary(model_fin_rec)
pairs(emmeans(model_fin_rec, ~cluster5))


ggplot(variete, aes(x = cluster5, y = Mois_fin_recolte)) +
  geom_boxplot()

emm_df <- as.data.frame(emmeans(model_fin_rec, ~ cluster5))

ggplot(emm_df, aes(x = cluster5, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(x = "Cluster", y = "Mois_fin_recolte")

modell_fin_rec_nerv <- lm( Mois_fin_recolte ~ Couleur_nervure_n, data=variete)
anova(modell_fin_rec_nerv)                                                           # significant
summary(modell_fin_rec_nerv)
emm_finrec_nerv <- emmeans(modell_fin_rec_nerv, ~Couleur_nervure)
emm_finrec_nerv
pairs(emm_finrec_nerv)

typ_nerv_tab <- table(variete$Couleur_nervure,variete$Type_manioc)
chisq.test(typ_nerv_tab)
cramerV(typ_nerv_tab)


modell_typ_fr <- lm( Mois_fin_recolte ~ Type_manioc+Couleur_nervure , data=variete)
anova(modell_typ_fr)                                                          
summary(modell_typ_fr)
emm_typ_fr <- emmeans(modell_typ_fr, ~Type_manioc)
emm_typ_fr
pairs(emm_typ_fr)

table(variete$Type_manioc, variete$Couleur_nervure)

modell_typ_fr <- lm( Mois_fin_recolte ~ Type_manioc+cluster5 , data=variete)
anova(modell_typ_fr)                                                          
summary(modell_typ_fr)
emm_typ_fr <- emmeans(modell_typ_fr, ~Type_manioc)
emm_typ_fr
pairs(emm_typ_fr)

variete$nervure_vert <- fct_other(variete$Couleur_nervure, keep = "vert")
typ_vert_tab <- table(variete$Type_manioc, variete$nervure_vert)
cramerV(typ_vert_tab)



