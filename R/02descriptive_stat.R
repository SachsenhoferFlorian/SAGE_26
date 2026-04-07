#Descriptive statistics--------
traits <- variete[, c("Type_manioc", "Couleur_feuilles_ap", "Pubescence", "Couleur_nervure", "Couleur_petiole", "Forme_lobes", "Nombre_lobes", "Couleur_tige", "Couleur_branches", "Ramification", "Forme_plante")]
traits <- traits %>% mutate(across(everything(), as.factor))
summary(traits)

matrix_traits_cv <- cramers_v_matrix(traits)
matrix_traits_cv

corrplot(matrix_traits_cv,
         method = "pie",
         type = "full",
         tl.col = "black",
         tl.srt = 75)

#Distribution sampling sites --------

counts_sampling_Comm <-
  variete %>%
  group_by(Commune) %>%
  summarise(
    number_farmer = n_distinct(Farmer),
    number_variete = n_distinct(Code_var)
  ) %>% mutate(alpha= number_variete/number_farmer)

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

counts_sampling_Eth <-
  variete %>%
  group_by(Communaute) %>%
  summarise(
    number_farmer = n_distinct(Farmer),
    number_variete = n_distinct(Code_var)
  ) %>% mutate(alpha= number_variete/number_farmer)


#Venn diagram

variete_Venn <- variete %>%
  mutate(across(all_of(Utilisation_cols), ~ . == 1))
ggvenn(select(variete_Venn, all_of(c("Utilisation_couac","Utilisation_cassave", "Utilisation_crabio"))))
#ggvenn(select(variete_Venn, all_of(Utilisation_cols)))

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





