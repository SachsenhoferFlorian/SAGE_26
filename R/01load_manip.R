
variete = read.xlsx("data/raw/VarietesAccessSAGE.xlsx", sheet = 1)
suivi = read.xlsx("data/raw/SuiviAccessSAGE.xlsx", sheet = 1)
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