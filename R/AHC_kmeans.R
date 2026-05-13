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


#Manual Clustering Varieties------------------------
#Kramanioc
variete$groupVariete <- if_else(variete$Kramanioc == 1 , "Kra-manioc", NA)
#Green venation
variete <- variete %>% mutate(groupVariete = if_else(Couleur_nervure == "vert", "Ven_green", groupVariete ))
#Branches purple or green-purple
variete <- variete %>% mutate(groupVariete = if_else(Couleur_branches == "violet" | Couleur_branches == "vert_violet", "Purple", groupVariete ))
#Kramanioc
variete <- variete %>% mutate(groupVariete = if_else( Kramanioc ==1, "Kra-manioc", groupVariete ))
#Rest
variete <- variete %>% mutate(groupVariete = if_else(is.na(groupVariete), "Rest", groupVariete ))

variete$cluster3 <- variete$groupVariete

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

#Analyse Cultivation depuis cluster3----------
variete$Cultivation_depuis <- factor(variete$Cultivation_depuis, levels = c("0-5" ,"5-10" , "10-15","15-20", "20"))


ggplot(data=variete, aes(x= Cultivation_depuis, fill=cluster3)) +
  geom_bar()
ggplot(data=variete, aes(x=cluster3, fill=Cultivation_depuis)) +
  geom_bar()



ggplot(data=variete, aes(x= Cultivation_depuis, fill=Communaute)) +
  geom_bar()
ggplot(data=variete, aes(x=Communaute, fill=Cultivation_depuis)) +
  geom_bar()




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
