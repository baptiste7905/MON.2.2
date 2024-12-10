library(jsonlite)

# Charger le fichier JSON
games_data <- fromJSON("Games/11-24.json")

# Aperçu des données


library(dplyr)

# Filtrer les parties de novembre 2024
games_november <- games_data$games %>%
  filter(grepl("^2024\\.11\\.", pgn)) # Les dates au format AAAA.MM.JJ

# Vérifier les données filtrées

# Extraire les mouvements (partie après la double ligne vide dans le PGN)
games_november <- games_november %>%
  mutate(moves = stringr::str_extract(pgn, "(?<=\\n\\n)(.*)")) # Tout ce qui suit la double ligne vide

# Aperçu des mouvements extraits
head(games_november$moves)
