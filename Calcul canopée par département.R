## Exemple de code pour un département n° 00. Les '00' devraient être remplacés par le numéro du département.

library(sf)
library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
library(units)

Arbres_00 <- st_read("NATURE-EN-VILLE/000_Communes_COSIA_light.gpkg", layer = "couvert_arbore")
Arbres_00$superficie <- as.numeric(st_area(Arbres_00))

Iris_00 <- st_read("Iris/contours-iris-pe.gpkg")
Iris_00 <- Iris_00 %>% filter(str_detect(code_iris, "^00"))
Iris_00 <- st_transform(Iris_00, st_crs(Arbres_00))

Arbres_centralise_00 <- st_centroid(Arbres_00)
Arbres_Iris_00 <- st_join(Arbres_centralise_00, Iris_00, join = st_intersects, left = FALSE)

Arbres_surface_00 <- Arbres_Iris_00 %>%
st_drop_geometry() %>% group_by(code_iris) %>% summarise(surface_totale_m2 = sum(superficie, na.rm = TRUE), nombre_zones = n())
Iris_superficie_00 <- Iris_00 %>% mutate(surface = st_area(geometrie))
Couverture_Arbres_Iris_00 <- left_join(Iris_superficie_00, Arbres_surface_00, by = "code_iris")
Couverture_Arbres_Iris_00 <- Couverture_Arbres_Iris_00 %>% mutate(taux_arbre = (surface_totale_m2 / surface) * 100)
# Revenu
Revenu_00 <- read_excel("Revenu/Revenu 2021 00.xlsx")
names(Revenu_00)[1] <- 'code_iris'
Revenu_median_00 <- Revenu_00 %>% select(code_iris, DEC_MED21)


Donnees_territoire <- left_join(Couverture_Arbres_Iris_00, Revenu_median_00, by = "code_iris")
# supprimer données vides
Donnees_territoire_prep <- Donnees_territoire %>% mutate(DEC_MED21 = ifelse(DEC_MED21 == "nd", NA, DEC_MED21))
Donnees_territoire_prep <- Donnees_territoire_prep %>% mutate(DEC_MED21 = ifelse(DEC_MED21 == "ns", NA, DEC_MED21))
Donnees_territoire_prep <- Donnees_territoire_prep %>% mutate(DEC_MED21 = as.numeric(DEC_MED21), taux_vegetation = as.numeric(taux_arbre))
regression <- lm(DEC_MED21 ~ taux_arbre, data = Donnees_territoire_prep)
summary(regression)

Donnees_territoire_prep$taux_arbre_chiffre <- drop_units(Donnees_territoire_prep$taux_arbre)
ggplot(Donnees_territoire_prep, aes(x = taux_arbre_chiffre, y = DEC_MED21)) +
    geom_point() +
    xlim(0, 100) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "Relation entre revenu médian et couverture arborée par Iris",
        x = "Couverture arborée (canopée/m²)",
        y = "Revenu médian"
    )