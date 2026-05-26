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
trait_names <- colnames(mca_data)
trait_names <- trait_names[-c(1,2,3,4,18)]
cluster_freq <- variete %>%
  pivot_longer(cols = all_of(trait_names), names_to = "Trait", values_to = "Value") %>%
  group_by(cluster, Trait, Value) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  pivot_wider(names_from = c(Trait, Value), values_from = Freq, values_fill = 0)
cluster_freq

lapply(trait_names, function(trait) table(variete$cluster, variete[[trait]]))



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
    Intercomm = first(Intercomm),
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



#UpSet diagram
upset(variete, sets=Utilisation_cols, order.by= "freq")


#JACCARD similarity for finding of doubles--------

mca_data <- mca_data %>%
  mutate(across(5:15, as.factor))
mca_data_sub <- dplyr::select(mca_data,7:17)
mca_dummy <- dummy_cols(mca_data_sub, remove_selected_columns = TRUE)

dist_mat <- dist(mca_dummy, method = "Jaccard")
sim_mat <- 1- as.matrix(dist_mat)
rownames(sim_mat) <- mca_data$Code_var
colnames(sim_mat) <- mca_data$Code_var
sim_mat
sim_df <- as.data.frame(as.table(sim_mat))
sim_df <- sim_df %>% filter(Var1 != Var2) %>% arrange(desc(Freq))
head(sim_df, 50)


