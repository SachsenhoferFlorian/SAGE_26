#MCA and HCPC analysis-----------

fviz_mca_ind(res.mca, repel=TRUE)
fviz_mca_var(res.mca, repel=TRUE, invisible= "quali.sup")
fviz_mca_biplot(res.mca, repel=TRUE, invisible= "quali.sup")
fviz_screeplot(res.mca, addlabels = TRUE)

print(get_eigenvalue(res.mca))

res.mca$var$contrib
res.mca$var$coord

res.hcpc$desc.var
res.hcpc$desc.var$category

mca_data_clustered <- res.hcpc$data.clust
variete$cluster <- res.hcpc$data.clust$clust


table(variete$cluster, mca_data_clustered$clust) #Check if clusters were attributed well


ggplot(data=variete, aes(x=cluster, fill=Commune)) +
  geom_bar()
ggplot(data=variete, aes(x=Commune, fill=cluster)) +
  geom_bar()

ggplot(data=variete, aes(x=cluster, fill=Intercomm)) +
  geom_bar()
ggplot(data=variete, aes(x=Intercomm, fill=cluster)) +
  geom_bar()


ggplot(data=variete, aes(x=Farmer, fill=cluster)) +
  geom_bar()
ggplot(data=variete, aes(x=cluster, fill=Farmer)) +
  geom_bar()


ggplot(data=variete, aes(x=Commune, fill=Farmer)) +
  geom_bar()


ggplot(data=variete, aes(x=Farmer, fill=cluster)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=variete, aes(x=cluster, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Commune, scales = "free_x")

ggplot(data=variete, aes(x=Farmer, fill=cluster)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

ggplot(data=variete, aes(x=cluster, fill=Farmer)) +
  geom_bar() +
  facet_wrap (~ Intercomm, scales = "free_x")

ggplot(data=variete, aes(x=Communaute, fill=cluster)) +
  geom_bar()
ggplot(data=variete, aes(x=cluster, fill=Communaute)) +
  geom_bar()

table(variete$cluster,variete$Type_manioc)





#Analyse Cultivation depuis cluster----------
variete$Cultivation_depuis <- factor(variete$Cultivation_depuis, levels = c("0-5","5-10","10-15","15-20","20"))
ggplot(data=variete, aes(x= Cultivation_depuis, fill=cluster)) +
  geom_bar()
ggplot(data=variete, aes(x=cluster, fill=Cultivation_depuis)) +
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

mod_clusterCult <- lm(data= variete, Cultiv_num ~ cluster)
anova(mod_clusterCult)
summary(mod_clusterCult)
emm_MCC <- emmeans(mod_clusterCult, ~ cluster)
pairs(emm_MCC)
cld_MCC <- cld(emm_MCC, Letters = letters)
emm_MCC_df <-as.data.frame(cld_MCC)

ggplot(emm_MCC_df,
       aes(x = cluster, y = emmean)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_text(aes(label= .group, y = upper.CL), size = 6)




mod_ClustCult <- lm(data= variete, Cultiv_num ~ cluster)
anova(mod_ClustCult)
summary(mod_ClustCult)
emm_MClC <- emmeans(mod_ClustCult, ~ cluster)
pairs(emm_MClC)
cld_MClC <- cld(emm_MClC, Letters = letters)


#Comparison Maturity Variety Clusters---------
ggplot(data=variete, aes(x=cluster, fill=mature_class)) +
  geom_bar()

ggplot(data=variete, aes(x=mature_class, fill=cluster)) +
  geom_bar()

mat_var_tab <- table(variete$cluster,variete$mature_class)
chisq.test(mat_var_tab)
cramerV(mat_var_tab)

ggplot(data=variete, aes(x=cluster, fill=Mois_fin_recolte)) +
  geom_bar()

ggplot(data=variete, aes(x=Mois_fin_recolte, fill=cluster)) +
  geom_bar()

finrec_var_tab <- table(variete$cluster,variete$Mois_fin_recolte)
chisq.test(finrec_var_tab)
cramerV(finrec_var_tab)

finrec_nerv_tab <- table(variete$Couleur_nervure,variete$Mois_fin_recolte)
chisq.test(finrec_nerv_tab)
cramerV(finrec_nerv_tab)


#Models Maturity Clusters----------

model_debut_rec <- lm( Mois_debut_recolte ~ cluster, data=variete) 
anova(model_debut_rec)                                                         
summary(model_debut_rec)
emmeans(model_debut_rec, ~cluster)
pairs(emmeans(model_debut_rec, ~cluster))

model_debut_rec <- lm( Mois_debut_recolte ~ Type_manioc + cluster, data=variete) 
anova(model_debut_rec)                                                         
summary(model_debut_rec)
emmeans(model_debut_rec, ~cluster)
pairs(emmeans(model_debut_rec, ~cluster))

model_debut_rec <- lm( Mois_debut_recolte ~ Type_manioc, data=variete) 
anova(model_debut_rec)                                                         
summary(model_debut_rec)
emmeans(model_debut_rec, ~Type_manioc)
pairs(emmeans(model_debut_rec,~Type_manioc ))

variete$Mois_fin_recolte <- as.numeric(as.character(variete$Mois_fin_recolte))

model_fin_rec <- lm( Mois_fin_recolte ~ cluster, data=variete)
anova(model_fin_rec)                                                           # significant
summary(model_fin_rec)
pairs(emmeans(model_fin_rec, ~cluster))


ggplot(variete, aes(x = cluster, y = Mois_fin_recolte)) +
  geom_boxplot()

emm_df <- as.data.frame(emmeans(model_fin_rec, ~ cluster))

ggplot(emm_df, aes(x = cluster, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(x = "Cluster", y = "Mois_fin_recolte")




modell_typ_fr <- lm( Mois_fin_recolte ~ Type_manioc+cluster , data=variete)
anova(modell_typ_fr)                                                          
summary(modell_typ_fr)
modell_typ_fr <- lm( Mois_fin_recolte ~ Type_manioc , data=variete)
emm_typ_fr <- emmeans(modell_typ_fr, ~Type_manioc)
emm_typ_fr
pairs(emm_typ_fr)

table(variete$Type_manioc, variete$Couleur_nervure)

