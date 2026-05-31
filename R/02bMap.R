library(sf)

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

