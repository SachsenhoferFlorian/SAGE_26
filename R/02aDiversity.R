
#Shannon--------

#Diversity descriptors
counts_list_data <- mca_data[,-c(1,2,3,4,5,6)]
counts_list <- lapply(counts_list_data,table)
div_descript <- sapply(counts_list, diversity, index = "shannon")
div_table <- enframe(div_descript, name = "group", value = "shannon")
div_table <- div_table %>% mutate(n_levels = sapply(counts_list, length)) %>%
                        relocate(n_levels, .before = shannon) %>%arrange(desc(shannon))


#Diversity in farmers and communes

shannon_per_commune <- variete %>%
  dplyr::count(Commune, Code_var) %>%
  group_by(Commune) %>%
  summarise(shannon = diversity(n, index = "shannon"))

shannon_farmer <- variete %>%
  dplyr::count(Commune, Farmer, Code_var) %>%
  group_by(Commune, Farmer) %>%
  summarise(
    shannon_farmer = diversity(n),
    .groups = "drop"
  )

shannon_commune <- shannon_farmer %>%
  group_by(Commune) %>%
  summarise(
    alpha = mean(shannon_farmer)
  )

gamma_commune <- variete %>%
  dplyr::count(Commune, Code_var) %>%
  group_by(Commune) %>%
  summarise(
    gamma = diversity(n),
    .groups = "drop"
  )

diversity_partition <- left_join(shannon_commune, gamma_commune, by = "Commune") %>%
  mutate(beta = gamma - alpha)


#Hill numbers


# Gamma (Commune-Ebene, effektiv)
gamma_commune <- variete %>%
  dplyr::count(Commune, Code_var) %>%
  group_by(Commune) %>%
  summarise(
    gamma = exp(diversity(n)),
    .groups = "drop"
  )

# Alpha (Farmer-Ebene → Mittelwert)
shannon_farmer <- variete %>%
  dplyr::count(Commune, Farmer, Code_var) %>%
  group_by(Commune, Farmer) %>%
  summarise(
    hill_farmer = exp(diversity(n)),
    .groups = "drop"
  )

alpha_commune <- shannon_farmer %>%
  group_by(Commune) %>%
  summarise(
    alpha = mean(hill_farmer),
    .groups = "drop"
  )

# Beta (multiplikativ!)
diversity_partition <- left_join(alpha_commune, gamma_commune, by = "Commune") %>%
  mutate(
    beta = gamma / alpha
  )


