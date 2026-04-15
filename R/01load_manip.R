#Varietes--------------------------------
variete = read.xlsx("data/raw/VarietesAccSAGE.xlsx", sheet = 1)
variete <- variete[!is.na(variete$Couleur_petiole), ]
rownames(variete) <- variete$ID
variete$ID_Enquete <- as.factor(variete$ID_Enquete)
variete$Farmer <- sprintf("F%02d", as.numeric(factor(variete$ID_Enquete)))

variete$Date <- as.Date(variete$Date,origin = "1899-12-30")
variete$Date_plante <- as.Date(variete$Date_plante,origin = "1899-12-30")

mapping <- c(
  "saint_laurent_du_maroni" = "CCOG",
  "mana" = "CCOG",
  "iracoubo" = "CCDS",
  "sinnaamary" = "CCDS",
  "kourou" = "CCDS",
  "macouria" = "CACL",
  "montsinerya_tonnegrande" = "CACL",
  "rouraa" = "CACL",
  "regina" = "CCEG",
  "sainta_georges" = "CCEG"
)

variete$Intercomm <- mapping[as.character(variete$Commune)]

variete$mature_class  <- cut(variete$Mois_debut_recolte,
                             breaks = c(-Inf, 7, 11, Inf),
                             labels = c("6", "8+", "12"))

variete$Mois_fin_recolte <- as.factor(variete$Mois_fin_recolte)


mapping_Cultiv <- c(
  "0-5" = "2.5",
  "5-10" = "7.5",
  "10-15" = "12.5",
  "15-20" = "17.5",
  "20" = "25"
)
variete$Cultiv_num <- as.numeric(mapping_Cultiv[as.character(variete$Cultivation_depuis)])

#Suivi-----------------------------------------

suivi = read.xlsx("data/raw/SuiviAccSAGE.xlsx", sheet = 1)
suivi$ID <- as.character(suivi$ID)
suivi$s_ID <- as.character(suivi$s_ID)
suivi$Date_enquete <- as.Date(suivi$Date_enquete,origin = "1899-12-30")
suivi <- rename(suivi, PB = poids_biomasse_sur_sol, PR= poids_total_racines, NR=nombre_racines)



mapping_severite <- c(
  "sain0" = "0",
  "p20" = "10",
  "p40" = "30",
  "p60" = "50",
  "p80" = "70",
  "p100" = "90",
  "mort100" = "100"
 )

suivi$Severite <- mapping_severite[as.character(suivi$Severite)]
suivi$Severite <- as.numeric(suivi$Severite)

variete$Kramanioc <-  1-(as.numeric(as.factor(variete$Type_manioc))-1)
Utilisation_cols <- c("Kramanioc", "Utilisation_bowo",	"Utilisation_cachiri",	"Utilisation_cassave",	"Utilisation_couac", "Utilisation_crabio", "Utilisation_domi_afiingi",	"Utilisation_sispa", "Utilisation_tapioca",	"Utilisation_cramanioc")
variete <- variete %>%
  mutate(across(all_of(Utilisation_cols), ~ as.numeric(as.character(.))))


#numeric values for correlation in suivi
suivi_numeric <- suivi[sapply(suivi, is.numeric)]

#secondary branching present
suivi$branche1  <- as.numeric(suivi$L1 > 0)

#MCA and HCPC-----------

mca_data <- variete[, c("Code_var", "Commune", "Intercomm", "Farmer", "Communaute", "Cultivation_depuis", "Type_manioc", "Couleur_feuilles_ap", "Pubescence", "Couleur_nervure", "Couleur_petiole", "Forme_lobes", "Nombre_lobes", "Couleur_tige", "Couleur_branches","Ramification", "Forme_plante")]
res.mca <- MCA(mca_data,quali.sup = c(1,2,3,4,5,6), graph = FALSE)

res.hcpc5 <- HCPC(res.mca, nb.clust = 5) #Clustering with manual choice (5)
res.hcpc5$desc.var
plot(res.hcpc5)

mca_data_clustered <- res.hcpc5$data.clust
variete$cluster5 <- res.hcpc5$data.clust$clust


#Joining with variete
suivi <- suivi %>% left_join(variete, by = c("Code_Var" = "Code_var"))

#Growth period
suivi$growth_period <- suivi$Date - suivi$Date_plante
suivi_numeric$growth_period <- suivi$growth_period
