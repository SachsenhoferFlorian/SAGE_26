#Descriptive statistics--------
traits <- variete[, c("Type_manioc", "Couleur_feuilles_ap", "Pubescence", "Couleur_nervure", "Couleur_petiole", "Forme_lobes", "Nombre_lobes", "Couleur_tige", "Couleur_branches", "Ramification", "Forme_plante")]
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
  group_by(Intercomm,Commune) %>%
  summarise(
    n_variete = n_distinct(Code_var),
    n_farmer = n_distinct(Farmer),
    F_autoch = n_distinct(Farmer[Communaute == "indigenous"]),
    F_bushi = n_distinct(Farmer[Communaute == "bushinengues"]),
    F_non = n_distinct(Farmer[Communaute == "other"]),
    V_autoch = sum(Communaute == "indigenous", na.rm = TRUE),
    V_bushi = sum(Communaute == "bushinengues", na.rm = TRUE),
    V_non = sum(Communaute == "other", na.rm = TRUE),
    alpha = n_variete / n_farmer,
    .groups = "drop"
  )


details <- variete %>%
  group_by(Intercomm, Commune) %>%
  summarise(
    n_varieties = n_distinct(Code_var),
    n_farmers = n_distinct(Farmer),
    F_ind = n_distinct(Farmer[Communaute == "indigenous"]),
    F_bushi = n_distinct(Farmer[Communaute == "bushinengues"]),
    F_other = n_distinct(Farmer[Communaute == "other"]),
    V_ind = sum(Communaute == "indigenous", na.rm = TRUE),
    V_bushi = sum(Communaute == "bushinengues", na.rm = TRUE),
    V_other = sum(Communaute == "other", na.rm = TRUE),
    alpha = n_varieties / n_farmers,
    .groups = "drop"
  )


subtotals <- details %>%
  group_by(Intercomm) %>%
  summarise(
    Commune = "Subtotal",
    across(c(n_varieties, n_farmers, F_ind, F_bushi, F_other,
             V_ind, V_bushi, V_other), sum),
    alpha = n_varieties / n_farmers,
    .groups = "drop"
  )


total <- subtotals %>%
  summarise(
    Intercomm = "Total",
    Commune = "Total",
    across(c(n_varieties, n_farmers, F_ind, F_bushi, F_other,
             V_ind, V_bushi, V_other), sum),
    alpha = n_varieties / n_farmers
  )


counts_sampling_Eth4 <- bind_rows(details, subtotals, total) %>%
  arrange(Intercomm == "Total", Intercomm, Commune == "Subtotal")



details5 <- variete %>%
  group_by(Intercomm, Commune,Communaute) %>%
  summarise(
    n_varieties = n_distinct(Code_var),
    n_farmers = n_distinct(Farmer),
    V_per_F = n_varieties / n_farmers,
    .groups = "drop"
  )


subtotals5 <- details5 %>%
  group_by(Intercomm) %>%
  summarise(
    Commune = "Subtotal",
    across(c(n_varieties, n_farmers), sum),
    V_per_F = n_varieties / n_farmers,
    .groups = "drop"
  )

subtotalsComm5 <- details5 %>%
  group_by(Communaute) %>%
  summarise(
    Intercomm = "Subtotal",
    across(c(n_varieties, n_farmers), sum),
    V_per_F = n_varieties / n_farmers,
    .groups = "drop"
  ) %>% arrange(desc(V_per_F))


total5 <- subtotals5 %>%
  summarise(
    Intercomm = "Total",
    Commune = "Total",
    across(c(n_varieties, n_farmers), sum),
    V_per_F = n_varieties / n_farmers
  ) 


counts_sampling_Eth5 <- bind_rows(details5, subtotals5, subtotalsComm5, total5) %>%
  arrange(Intercomm == "Total", Intercomm, Commune == "Subtotal")





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


