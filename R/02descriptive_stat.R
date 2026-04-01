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
  ) %>% mutate(beta= number_variete/number_farmer)

counts_sampling_Intercomm <-
  variete %>%
  group_by(Intercomm) %>%
  summarise(
    number_farmer = n_distinct(Farmer),
    number_variete = n_distinct(Code_var)
  ) %>% mutate(beta= number_variete/number_farmer)

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
  ) %>% mutate(beta= number_variete/number_farmer)