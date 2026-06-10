library(sf)
library(ggrepel)

suivi <- suivi %>% mutate(GPS_long_ferme = ifelse(ID_Enquete == "742862779", -53.542698, GPS_long_ferme))
suivi <- suivi %>% mutate(GPS_lat_ferme = ifelse(ID_Enquete == "742862779", 5.510749, GPS_lat_ferme))

#Load map
communes_guyane <- st_read("data/maps/ADMIN_EXPRESS_GUF/ADMIN-EXPRESS-COG-CARTO/1_DONNEES_LIVRAISON_2024-03-00169/ADECOGC_3-2_SHP_UTM22RGFG95_GUF-ED2024-02-22/COMMUNE.shp")
communes_wgs84 <- st_transform(communes_guyane, 4326)

variete$GPS_long_ferme <- as.numeric(variete$GPS_long_ferme )
variete$GPS_lat_ferme <- as.numeric(variete$GPS_lat_ferme )
sites_sf <- st_as_sf(variete, coords = c("GPS_long_ferme", "GPS_lat_ferme"), crs = 4326)



# Mapping of intercommunalities
intercommunalite_map <- data.frame(
  comm_name = c(
    # CACL - Communauté d'agglomération du Centre Littoral
    "Cayenne",
    "Remire-Montjoly", 
    "Matoury",
    "Montsinéry-Tonnegrande",
    "Macouria",
    "Roura",
    
    # CCOG - Communauté de communes de l'Ouest Guyanais
    "Saint-Laurent-du-Maroni",
    "Mana",
    "Awala-Yalimapo",
    "Maripasoula",
    "Papaichton", 
    "Saül",
    "Grand-Santi",
    "Apatou",
    
    # CCS - Communauté de communes des Savanes
    "Kourou",
    "Sinnamary",
    "Iracoubo",
    "Saint-Élie",
    
    # CCEG - Communauté de communes de l'Est Guyanais
    "Régina",
    "Ouanary",
    "Saint-Georges",
    "Camopi"
  ),
  intercommunalite = c(
    rep("CACL", 6),
    rep("CCOG", 8),
    rep("CCDS", 4),
    rep("CCEG", 4)
  )
)

communes_colored <- communes_wgs84 %>%
  left_join(intercommunalite_map, by = c("NOM" = "comm_name"))

# Labelling of surveyed municipalities
communes_to_label <- communes_colored %>%
  filter(NOM %in% c(
    "Saint-Laurent-du-Maroni",
    "Mana",
    "Kourou",
    "Sinnamary",
    "Iracoubo",
    "Roura",
    "Montsinéry-Tonnegrande",
    "Macouria",
    "Régina",
    "Saint-Georges"
  ))

communes_labels <- communes_to_label %>%
  mutate(
    label_x = st_coordinates(st_centroid(geometry))[,1],
    label_y = st_coordinates(st_centroid(geometry))[,2]
  )

#communes_labels <- communes_to_label %>% mutate(label_x = ifelse(NOM = "Mana", label_x+)

ggplot() +
  geom_sf(data = communes_colored, 
          aes(fill = intercommunalite), 
          color = "black", 
          size = 0.3) +
  geom_sf(data = sites_sf, aes(color = Communaute), size = 3
          ) +
  coord_sf(xlim = c(-54.5, -51.5), ylim = c(3.6, 5.8)) +
  geom_text(data = communes_labels,
            aes(x = label_x, y = label_y, label = NOM),
            size = 3,
            fontface = "bold",
            color = "black",
            bg.color = "white",
            bg.r = 0.1) +
  scale_color_brewer(palette = "Set1", name = "Communaute") +
  scale_fill_brewer(palette = "Set2", 
                    name = "Intercommunalité",
                    na.value = "gray90") +
  labs(title = "Municipalities of French Guiana") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Count how many varieties per location and community
sites_counts <- variete %>%
  group_by(GPS_long_ferme, GPS_lat_ferme, Communaute) %>%
  summarise(count = n(), .groups = 'drop')

ggplot() +
  geom_sf(data = communes_colored, 
          aes(fill = intercommunalite), 
          color = "black", 
          size = 0.3) +
  geom_point(data = sites_counts,
             aes(x = GPS_long_ferme, y = GPS_lat_ferme, 
                 color = Communaute, size = count),
             alpha = 0.7) +
  geom_text_repel(data = communes_labels,
                  aes(x = label_x, y = label_y, label = NOM),
                  size = 3,
                  fontface = "bold",
                  segment.color = "gray50",
                  segment.size = 0.3) +
  coord_sf(xlim = c(-54.5, -51.5), ylim = c(3.6, 5.8)) +
  
  scale_color_brewer(palette = "Set1", name = "Communaute") +
  scale_size_continuous(name = "Number of varieties", range = c(2, 8)) +
  scale_fill_brewer(palette = "Set2", name = "Intercommunalité", na.value = "gray90") +
  labs(title = "Municipalities of French Guiana") +
  theme_minimal() +
  theme(legend.position = "bottom")





# Hauptkarte OHNE Legenden
main_map <- ggplot() +
  geom_sf(data = communes_colored, 
          aes(fill = intercommunalite), 
          color = "#9D8189", 
          size = 0.3) +
  geom_point(data = sites_counts, 
             aes(x = GPS_long_ferme, y = GPS_lat_ferme, 
                 color = Communaute, size = count), 
             alpha = 0.7) +
  geom_text_repel(data = communes_labels, 
                  aes(x = label_x, y = label_y, label = NOM), 
                  size = 3, fontface = "bold", 
                  segment.color = "gray70", 
                  segment.size = 0.2, 
                  force = 0.5, 
                  force_pull = 0.5, 
                  max.overlaps = 15, 
                  point.padding = 0.05, 
                  box.padding = 0.1, 
                  direction = "both", 
                  seed = 123) +
  coord_sf(xlim = c(-54.5, -51.5), ylim = c(3.6, 5.8)) +
  scale_fill_manual(name = "Intercommunalité", values = c("CACL" = "#D4E6D4", "CCOG" = "#FFE4C4", "CCDS" = "#E8D5E8", "CCEG" = "#C9E5D9"), na.value = "#F5F5DC") +
  scale_color_brewer(palette = "Set1", name = "Community") +
  scale_size_continuous(name = "Number of varieties", range = c(2, 8)) +
  labs(title = "Accessions collected in French Guiana", x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(),
        axis.text.y = element_text(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Nur die Intercommunalité-Legende
interco_legend_plot <- ggplot() +
  geom_sf(data = communes_colored, aes(fill = intercommunalite)) +
  scale_fill_manual(name = "Intercommunalité", values = c("CACL" = "#D4E6D4", "CCOG" = "#FFE4C4", "CCDS" = "#E8D5E8", "CCEG" = "#C9E5D9"), na.value = "#F5F5DC") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black", size = 0.3),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8))

# Nur die Community-Legende
community_legend_plot <- ggplot() +
  geom_point(data = sites_counts, aes(x = 1, y = 1, color = Communaute), size = 5) +
  scale_color_brewer(palette = "Set1", name = "Community") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black", size = 0.3),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8))

# Nur die Size-Legende
size_legend_plot <- ggplot() +
  geom_point(data = sites_counts, aes(x = 1, y = 1, size = count), color = "black") +
  scale_size_continuous(name = "Number of varieties", range = c(2, 8)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black", size = 0.3),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8))

# Extrahiere die Legenden als Grobs
interco_legend <- cowplot::get_legend(interco_legend_plot)
community_legend <- cowplot::get_legend(community_legend_plot)
size_legend <- cowplot::get_legend(size_legend_plot)



# Füge die Legenden mit inset_element (patchwork) hinzu
main_map +
  inset_element(interco_legend, 
                left = 0.12, bottom = 0.08, right = 0.67, top = 0.1,  # Unten links
                align_to = "full") +
  inset_element(size_legend, 
                left = 0.4, bottom = 0.85, right = 0.98, top = 0.90,  # Oben rechts
                align_to = "full") +
  inset_element(community_legend, 
                left = 0.1, bottom = 0.13, right = 0.60, top = 0.16,  # Oben rechts
                align_to = "full")

