
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


#Presentation of tables
tab_type <- as.data.frame(table(variete$Type_manioc))
colnames(tab_type) <- c("Type de manioc", "Nb")
tab_type <- tab_type[order(-tab_type$Nb), ]
ft1 <- flextable(tab_type)


tab_colour <- as.data.frame(table(variete$Couleur_racine_enquete))
colnames(tab_colour) <- c("Couleur racine", "Nb")
tab_colour <- tab_colour[order(-tab_colour$Nb), ]
ft2 <- flextable(tab_colour)


tab_com <- as.data.frame(table(variete$Commune))
colnames(tab_com) <- c("Communes", "Nb")
tab_com <- tab_com[order(-tab_com$Nb), ]
ft3 <- flextable(tab_com)


tab_comm <- as.data.frame(table(variete$Communaute))
colnames(tab_comm) <- c("Communauté", "Nb")
tab_comm <- tab_comm[order(-tab_comm$Nb), ]
ft4 <- flextable(tab_comm)


ft1 <- style_ft(ft1)
ft2 <- style_ft(ft2)
ft3 <- style_ft(ft3)
ft4 <- style_ft(ft4)

ft1 <- bold(ft1, part = "header", bold = TRUE)
ft2 <- bold(ft2, part = "header", bold = TRUE)
ft3 <- bold(ft3, part = "header", bold = TRUE)
ft4 <- bold(ft4, part = "header", bold = TRUE)

doc <- read_pptx() %>%
  add_slide(layout = "Blank", master = "Office Theme") %>%
  ph_with(
    value= "Categories et nombres",
    location = ph_location(left = 0.5, top = 0.2, width = 9, height = 0.8)) %>%
  ph_with(ft1,location = ph_location(left = 0.5, top = 1.2, width = 4.2, height = 2.8)) %>%
  ph_with(ft2,location = ph_location(left = 5.2, top = 1.2, width = 4.2, height = 2.8)) %>%
  ph_with(ft4,location = ph_location(left = 0.5, top = 4.2, width = 4.2, height = 2.8)) %>%
  ph_with(ft3,location = ph_location(left = 5.2, top = 4.2, width = 4.2, height = 3.8))

print(doc, target = "data/presentation/tableaux_pres.pptx")



#Table printing--------------------------



# Create flextable
ft <- counts_sampling_Eth5 %>%
  flextable() %>%
  colformat_double(j = "V_per_F", digits = 2) %>%
  autofit()

# Save to Word
save_as_docx(ft, path = "data/presentation/counts_sampling_table.docx")


# Create flextable
ft1 <- div_table %>%
  flextable() %>%
  autofit()

# Save to Word
save_as_docx(ft1, path = "data/presentation/diversity_descriptors.docx")






usage_summary <- lapply(seq_along(utili_names), function(i) {
  usage_name <- names(utilisations_rename)[i]
  original_var <- utili_names[i]
  data.frame(
    Use = usage_name,
    Indigenous = sum(variete$Communaute == "indigenous" & variete[[original_var]] == 1, na.rm = TRUE),
    Bushinengues = sum(variete$Communaute == "bushinengues" & variete[[original_var]] == 1, na.rm = TRUE),
    Other = sum(variete$Communaute == "other" & variete[[original_var]] == 1, na.rm = TRUE)
  )
}) %>%
  bind_rows() %>%
  mutate(Total = Indigenous + Bushinengues + Other) %>%
  arrange(desc(Total))

# View the table
print(usage_summary)

# Load libraries
library(flextable)
library(officer)
library(dplyr)
library(tidyr)

# Create the summary table (dynamic version)
usage_summary <- lapply(seq_along(utili_names), function(i) {
  usage_name <- names(utilisations_rename)[i]
  original_var <- utili_names[i]
  
  tab <- table(variete$Communaute, variete[[original_var]] == "Oui")
  
  result <- as.data.frame.matrix(tab)
  result$Usage <- usage_name
  result$Community <- rownames(result)
  result <- result[, c("Usage", "Community", "TRUE")]
  names(result)[3] <- "Count"
  
  return(result)
}) %>%
  bind_rows() %>%
  pivot_wider(
    names_from = Community,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(select(., -Usage))) %>%
  arrange(desc(Total))

# Create flextable
ft <- flextable(usage_summary)

# Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "data/presentation/usage_summary.docx")
