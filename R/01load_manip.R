#Varietes--------------------------------
variete = read.xlsx("data/raw/VarietesAccSAGE.xlsx", sheet = 1)
variete <- variete[!is.na(variete$Couleur_petiole), ]
rownames(variete) <- variete$ID
variete$ID_Enquete <- as.factor(variete$ID_Enquete)
variete$Farmer <- sprintf("F%02d", as.numeric(factor(variete$ID_Enquete)))



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

#Suivi-----------------------------------------

suivi = read.xlsx("data/raw/SuiviAccSAGE.xlsx", sheet = 1)
suivi$ID <- as.character(suivi$ID)
suivi$s_ID <- as.character(suivi$s_ID)
suivi$Date_enquete <- as.Date(suivi$Date_enquete,origin = "1899-12-30")
suivi <- rename(suivi, PB = poids_biomasse_sur_sol, PR= poids_total_racines, NR=nombre_racines)

suivi <- suivi %>% mutate(across(c(L1,N1,B1,D1), ~na_if(.,0)))

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
