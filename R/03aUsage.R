

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


utilisations_rename <- variete %>% dplyr::select(all_of(utili_names)) %>%
  rename(
    Bowo = Utilisation_bowo,
    Sispa = Utilisation_sispa,
    Couac = Utilisation_couac,
    Cachiri = Utilisation_cachiri,
    Cassave = Utilisation_cassave,
    Crabio = Utilisation_crabio,
    Sweet_cassava = Utilisation_cramanioc,
    Tapioca = Utilisation_tapioca,
    Domi = Utilisation_domi_afiingi
  )



#UpSet diagram
tiff("data/figures/upset_plot.tiff", width = 10, height = 7, units = "in", res = 300)
upset(utilisations_rename, sets=names(utilisations_rename), order.by= "freq")
dev.off()




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
variete_alluv2 <- variete %>%  mutate(Communaute = factor(Communaute, levels= c("indigenous","bushinengues","other"))) 

variete_alluv2 <- variete_alluv2 %>%  mutate(id = row_number()) %>%
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
      usage == "Utilisation_cramanioc" ~ "Sweet cassava",
      usage == "Utilisation_tapioca" ~ "Tapioca",
      usage == "Utilisation_domi_afiingi" ~ "Domi"
    ),
    levels = c("Cachiri", "Cassave", "Bowo",  "Domi", "Sispa", "Tapioca", "Crabio","Couac","Sweet cassava")
  )) 

# ---- Alluvial plot ----

fig_alluvial <- ggplot(variete_alluv2,
       aes(axis1 = cluster,
           axis2 = Communaute,
           axis3 = usage,
           y = n)) +
  geom_alluvium(aes(fill = cluster), alpha = 0.8, lode.guidance = "forward") +
  geom_stratum(width = 0.3) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Cluster",  "Community", "Uses")) +
  theme_minimal() +
  labs(
    title = "Alluvial Diagram Variety cluster -> Community -> Uses",
    y = "weighted count of accessions"
  )
fig_alluvial

ggsave("data/figures/UsagesAlluvial.png", 
       plot = fig_alluvial,
       width = 7, 
       height = 6,  
       dpi = 300)

library(ggrepel)
library(dplyr)
library(ggplot2)
library(ggalluvial)

# Zuerst die Stratum-Daten extrahieren
p_temp <- ggplot(variete_alluv2,
                 aes(axis1 = cluster,
                     axis2 = Communaute,
                     axis3 = usage,
                     y = n)) +
  geom_alluvium(aes(fill = cluster), alpha = 0.8, lode.guidance = "forward") +
  geom_stratum(width = 0.3)

build_data <- ggplot_build(p_temp)
stratum_data <- build_data$data[[2]]

# Alle x-Werte anzeigen
unique_x <- unique(stratum_data$x)
print(unique_x)

# Daten für jede Säule vorbereiten
all_strata <- data.frame(
  x = stratum_data$x,
  ymin = stratum_data$ymin,
  ymax = stratum_data$ymax,
  label = stratum_data$stratum
)
all_strata$y_center <- (all_strata$ymin + all_strata$ymax) / 2

# Rechte Säule (maximaler x-Wert) - hier sollen Sispa und Bowo verschoben werden
max_x <- max(all_strata$x)
right_data <- all_strata[all_strata$x == max_x, ]

# Linke und mittlere Säule (alle anderen x-Werte)
left_mid_data <- all_strata[all_strata$x != max_x, ]

# Sispa und Bowo aus der rechten Säule für ggrepel
repel_data <- right_data[right_data$label %in% c("Sispa", "Bowo", "Sweet cassava"), ]

# Alle anderen Labels der rechten Säule (normal)
normal_right_data <- right_data[!right_data$label %in% c("Sispa", "Bowo", "Sweet cassava"), ]

# Finaler Plot mit allen Beschriftungen
fig_alluvial <- ggplot(variete_alluv2,
                       aes(axis1 = cluster,
                           axis2 = Communaute,
                           axis3 = usage,
                           y = n)) +
  geom_alluvium(aes(fill = cluster), alpha = 0.8, lode.guidance = "forward") +
  geom_stratum(width = 0.3) +
  
  # Beschriftungen für linke und mittlere Säule (normal)
  geom_text(
    data = left_mid_data,
    aes(x = x, y = y_center, label = label),
    size = 3.5,
    inherit.aes = FALSE
  ) +
  
  # Normale Beschriftungen für die rechte Säule (außer Sispa und Bowo)
  geom_text(
    data = normal_right_data,
    aes(x = x, y = y_center, label = label),
    size = 3.5,
    inherit.aes = FALSE
  ) +
  
  # Versetzte Beschriftungen für Sispa und Bowo mit Linien
  geom_text_repel(
    data = repel_data,
    aes(x = x, y = y_center, label = label),
    nudge_x = 0.4,
    hjust = 0,
    direction = "y",
    segment.color = "gray50",
    segment.size = 0.5,
    size = 3.5,
    min.segment.length = 0,
    box.padding = 0.5,
    inherit.aes = FALSE
  ) +
  
  scale_x_discrete(limits = c("Cluster", "Community", "Uses")) +
  theme_minimal() +
  labs(
    title = "Alluvial Diagram:  Variety clusters -> Community -> Uses",
    y = "(Weighted) count of varieties",
    x = ""
  )

fig_alluvial

ggsave("data/figures/UsagesAlluvial.png", 
       plot = fig_alluvial,
       width = 8.9, 
       height = 6,  
       dpi = 300)

