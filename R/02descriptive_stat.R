#Descriptive statistics--------
traits <- variete[, c("Type_manioc", "Couleur_racine_enquete", "Couleur_feuilles_ap", "Pubescence", "Couleur_nervure", "Couleur_petiole", "Forme_lobes", "Nombre_lobes", "Couleur_tige", "Couleur_branches", "Ramification", "Forme_plante")]
traits <- traits %>% mutate(across(everything(), as.factor))
summary(traits)



matrix_traits_cv <- cramers_v_matrix(traits)
matrix_traits_cv

corrplot(matrix_traits_cv,
         method = "color",
         type = "full",
         tl.col = "black",
         tl.srt = 50,
         addCoef.col = "black",
         addCoefasPercent = TRUE)


#Cross table
trait_names <- colnames(mca_data_clustered)
trait_names <- trait_names[-c(1,2,3,4,18)]
cluster_freq <- variete %>%
  pivot_longer(cols = all_of(trait_names), names_to = "Trait", values_to = "Value") %>%
  group_by(cluster5, Trait, Value) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  pivot_wider(names_from = c(Trait, Value), values_from = Freq, values_fill = 0)
cluster_freq

lapply(trait_names, function(trait) table(variete$cluster5, variete[[trait]]))

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

#Distribution sampling sites --------

counts_sampling_Comm <-
  variete %>%
  group_by(Commune) %>%
  summarise(
    Intercom = first(Intercomm),
    n_farmer = n_distinct(Farmer),
    n_variete = n_distinct(Code_var),
  ) %>% mutate(alpha= n_variete/n_farmer)

counts_sampling_Intercomm <-
  variete %>%
  group_by(Intercomm) %>%
  summarise(
    number_farmer = n_distinct(Farmer),
    number_variete = n_distinct(Code_var)
  ) %>% mutate(alpha= number_variete/number_farmer)

counts_sampling_Farm <- 
  variete %>%
  group_by(Commune, Farmer) %>%
  summarise(n = n())

#Pivot table with number of farmers from communities

counts_sampling_Eth <-
  variete %>%
  group_by(Communaute) %>%
  summarise(
    number_farmer = n_distinct(Farmer),
    number_variete = n_distinct(Code_var)
  ) %>% mutate(alpha= number_variete/number_farmer)

counts_sampling_Eth2 <- variete %>%
  group_by(Commune,Communaute) %>%
  summarise(n_farmer = n_distinct(Farmer), n_variete = n_distinct(Code_var), .groups = "drop") %>% 
  mutate(alpha= n_variete/n_farmer) %>%
  pivot_wider(
    names_from = Communaute,      
    values_from = n_farmer,
    names_prefix = "Ethn",
    values_fill = 0
  )

counts_sampling_Eth3 <- variete %>%
  group_by(Commune) %>%
  summarise(
    n_variete = n_distinct(Code_var),
    n_farmer = n_distinct(Farmer),
    F_autoch = n_distinct(Farmer[Communaute == "amerindien"]),
    F_bushi = n_distinct(Farmer[Communaute == "bushinengues"]),
    F_non = n_distinct(Farmer[Communaute == "non"]),
    V_autoch = sum(Communaute == "amerindien", na.rm = TRUE),
    V_bushi = sum(Communaute == "bushinengues", na.rm = TRUE),
    V_non = sum(Communaute == "non", na.rm = TRUE),
    alpha = n_variete / n_farmer,
    .groups = "drop"
  )

#Venn diagram

variete_Venn <- variete %>%
  mutate(across(all_of(Utilisation_cols), ~ . == 1))
ggvenn(select(variete_Venn, all_of(c("Utilisation_couac","Utilisation_cassave", "Utilisation_crabio"))))


venn.plot <- venn.diagram(
  x = select(variete_Venn, all_of(Utilisation_cols)),
  filename = NULL,
  alpha = 0.5,
  cex = 1.5,
  cat.cex = 1.5
)

venn.plot <- venn.diagram(
  x = select(variete_Venn, all_of(c("Utilisation_couac","Utilisation_cassave", "Utilisation_crabio", "Utilisation_domi_afiingi"))),
  scaled=TRUE,
  filename = NULL,
  alpha = 0.5,
  cex = 1.5,
  cat.cex = 1.5
)
grid.draw(venn.plot)


#UpSet diagram
upset(variete, sets=Utilisation_cols, order.by= "freq")





