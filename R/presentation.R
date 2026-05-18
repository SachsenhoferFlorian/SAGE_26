
#hcpc descriptor table----------------------------
library(flextable)
library(officer)

descvar_to_flextable <- function(hcpc_res) {
  all_ft <- list()
  
  if(is.null(hcpc_res$desc.var$category)) {
    warning("Keine 'category' in desc.var gefunden")
    return(all_ft)
  }
  
  cluster_names <- names(hcpc_res$desc.var$category)
  
  for(cl in cluster_names) {
    # Matrix in Dataframe umwandeln
    mat <- hcpc_res$desc.var$category[[cl]]
    
    if(!is.null(mat) && nrow(mat) > 0) {
      df <- as.data.frame(mat)
      df$Category <- rownames(df)
      rownames(df) <- NULL
      
      # Spaltenreihenfolge
      df <- df[, c("Category", "Cla/Mod", "Mod/Cla", "Global", "p.value", "v.test")]
      
      # p-Wert formatieren
      df$p.value <- format.pval(df$p.value, digits = 3)
      
      # Numerische Spalten runden
      df$`Cla/Mod` <- round(df$`Cla/Mod`, 2)
      df$`Mod/Cla` <- round(df$`Mod/Cla`, 2)
      df$Global <- round(df$Global, 2)
      df$v.test <- round(df$v.test, 2)
      
      # Flextable erstellen
      ft <- flextable(df) %>%
        set_caption(paste("Cluster", cl, "- Characteristic Categories")) %>%
        autofit() %>%
        theme_booktabs()
      
      all_ft[[cl]] <- ft
    }
  }
  
  return(all_ft)
}

# Anwenden
ftables <- descvar_to_flextable(res.hcpc)
cat("Anzahl erstellter Flextables:", length(ftables), "\n")

if(length(ftables) > 0) {
  doc <- read_docx()
  doc <- body_add_par(doc, "HCPC Cluster Descriptions", style = "heading 1")
  
  for(i in seq_along(ftables)) {
    doc <- body_add_par(doc, paste("Cluster", names(ftables)[i]), style = "heading 2")
    doc <- body_add_flextable(doc, ftables[[i]])
    doc <- body_add_par(doc, "")
  }
  
  print(doc, target = "cluster_descriptions.docx")
  cat("Exportiert nach: cluster_descriptions.docx\n")
}

#absolute v test----------------------------

# Extract all significant categories with v.test
all_vtest <- do.call(rbind, lapply(names(res.hcpc$desc.var$category), function(cl) {
  mat <- res.hcpc$desc.var$category[[cl]]
  df <- as.data.frame(mat)
  df$Category <- rownames(df)
  df$Cluster <- cl
  df <- df[df$p.value < 0.05, ]  # Only significant
  return(df[, c("Category", "Cluster", "v.test")])
}))

# Extract variable name
all_vtest$Variable <- gsub("=.*", "", all_vtest$Category)

# Mean absolute v.test per variable
var_importance_vtest <- all_vtest %>%
  group_by(Variable) %>%
  summarise(
    N_Clusters = n_distinct(Cluster),
    Mean_abs_vtest = mean(abs(v.test)),
    Max_abs_vtest = max(abs(v.test)),
    .groups = "drop"
  ) %>%
  arrange(-Mean_abs_vtest)

print(var_importance_vtest)
