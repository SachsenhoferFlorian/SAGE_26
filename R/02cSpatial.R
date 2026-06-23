
suivi <- suivi %>% mutate(Severite_cum = ifelse(deltaT_infect > growth_period, Severite_cum - (((as.numeric(deltaT_infect-growth_period))^2)*k)/2 , Severite_cum))
suivi <- suivi %>% mutate(Severite_cum_percent = Severite_cum/(as.numeric(growth_period)))
# 1. CREATE CLEAN COORDINATE VARIABLES (if not already done)
# Convert to numeric and strip any attributes
suivi$long_clean <- as.numeric(suivi$GPS_long_ferme)
suivi$lat_clean <- as.numeric(suivi$GPS_lat_ferme)

# Remove any hidden attributes (just to be safe)
attributes(suivi$long_clean) <- NULL
attributes(suivi$lat_clean) <- NULL

# 2. CREATE PROPORTION VARIABLE (if not already done)
suivi$Severite_prop <- suivi$Severite / 100


# 3. VERIFY YOUR DATA
head(suivi[, c("Severite_prop", "long_clean", "lat_clean")])
summary(suivi[, c("Severite_prop", "long_clean", "lat_clean")])

# 4. SIMPLE GLM 
spatial_glm <- glm(Severite_prop ~ growth_period +long_clean * lat_clean ,
                   data = suivi,
                   family = quasibinomial())

# 5. VIEW RESULTS
summary(spatial_glm)



# 6. PREDICT FOR OBSERVATIONS ONLY
suivi$predicted <- predict(spatial_glm, type = "response")

# 7. PLOT OBSERVED VS PREDICTED
library(ggplot2)
ggplot(suivi, aes(x = Severite_prop, y = predicted)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "Observed vs Predicted Disease Severity",
       x = "Observed Severity (proportion)", 
       y = "Predicted Severity (proportion)") +
  annotate("text", x = 0.2, y = 0.8, 
           label = paste("R² =", round(cor(suivi$Severite_prop, suivi$predicted)^2, 3)),
           size = 5)

# 8. VISUALIZE SPATIAL PREDICTIONS ON MAP
ggplot(suivi, aes(x = long_clean, y = lat_clean, color = predicted)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0.5, limits = c(0, 1),
                        name = "Predicted\nSeverity") +
  theme_minimal() +
  labs(title = "Spatial Distribution of Predicted Disease Severity",
       x = "Longitude", y = "Latitude")

# 9. CHECK RESIDUALS FOR SPATIAL PATTERN
suivi$residuals <- residuals(spatial_glm, type = "response")

ggplot(suivi, aes(x = long_clean, y = lat_clean, color = residuals)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, name = "Residuals") +
  theme_minimal() +
  labs(title = "Spatial Distribution of Residuals",
       subtitle = "Random pattern = no spatial structure left",
       x = "Longitude", y = "Latitude")

# 10. COMPARE WITH NULL MODEL (NO SPATIAL TERMS)
null_model <- glm(Severite_prop ~ 1, data = suivi, family = quasibinomial())
anova(null_model, spatial_glm, test = "F")

# 11. SIMPLER SPATIAL TEST (JUST LINEAR TERMS)
simple_spatial <- glm(Severite_prop ~ long_clean + lat_clean,
                      data = suivi,
                      family = quasibinomial())
summary(simple_spatial)

# 12. TEST FOR SPATIAL AUTOCORRELATION IN RESIDUALS
library(spdep)

coords <- as.matrix(suivi[, c("long_clean", "lat_clean")])
nb <- knn2nb(knearneigh(coords, k = 5))
listw <- nb2listw(nb)

moran_resid <- moran.test(suivi$residuals, listw)
print(moran_resid)

# 13. VISUALIZE SEVERITY BY LOCATION (RAW DATA)
ggplot(suivi, aes(x = long_clean, y = lat_clean, color = Severite_prop)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0.5, limits = c(0, 1),
                        name = "Severity") +
  theme_minimal() +
  labs(title = "Observed Disease Severity by Location",
       x = "Longitude", y = "Latitude")


#Severity at marking------------------------------
# 1. CREATE CLEAN COORDINATE VARIABLES (if not already done)
# Convert to numeric and strip any attributes
plants$long_clean <- as.numeric(plants$GPS_long_ferme)
plants$lat_clean <- as.numeric(plants$GPS_lat_ferme)

# Remove any hidden attributes (just to be safe)
attributes(plants$long_clean) <- NULL
attributes(plants$lat_clean) <- NULL

# 2. CREATE PROPORTION VARIABLE (if not already done)
plants$Severite_prop <- plants$Severite_marqu / 100

# 3. VERIFY YOUR DATA
head(suivi[, c("Severite_prop", "long_clean", "lat_clean")])
summary(suivi[, c("Severite_prop", "long_clean", "lat_clean")])

# 4. SIMPLE GLM 
spatial_glm_marqu <- glm(Severite_prop ~ long_clean * lat_clean + growth_period_marqu,
                         data = plants,
                         family = quasibinomial())

# 5. VIEW RESULTS
summary(spatial_glm_marqu)

