##1. Initialisation
options(repos = c(CRAN = "https://cloud.r-project.org"))

#Packages
install.packages(c("dplyr", "ggplot2", "leaflet"))

#Manipulation
library(dplyr)
#Visualisation
library(ggplot2)
#Cartes
library(leaflet)

#On charge les donnes
data <- read.csv("/Users/baptisteaudouin/Documents/GitHub/MON.2.2/Data/listings.csv", stringsAsFactors = FALSE)


##2. Nettoyage des donnes

# Corriger les problèmes d'encodage (accents)
data$name <- iconv(data$name, from = "UTF-8", to = "ASCII//TRANSLIT")
data$neighbourhood <- iconv(data$neighbourhood, from = "UTF-8", to = "ASCII//TRANSLIT")

# Gérer les valeurs manquantes
data <- data %>%
  filter(!is.na(price) & price > 0) %>%  # Supprimer les prix manquants ou nuls
  mutate(price = as.numeric(price))  # Convertir en numérique la colonne prix

# Supp license qui est inutile
data <- data %>% select(-license)  

# On enregistre maintenant notre jeu de données nettoyé
write.csv(data, "/Users/baptisteaudouin/Documents/GitHub/MON.2.2/Data/data_nettoye.csv", row.names = FALSE)

##3. Premières visualisations

#Prix par type de chambres
price_by_room <- data %>%
  group_by(room_type) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

write.csv(price_by_room, "/Users/baptisteaudouin/Documents/GitHub/MON.2.2/Data/price_by_room.csv", row.names = FALSE)

# histogramme de prix
plot1 <- ggplot(data, aes(x = price)) +
# type de graphe
  geom_histogram(binwidth = 50, fill = "blue", color = "white") + 
# Limite de pris à 500
  xlim(0, 500) +
# légende
  labs(title = "Distribution des prix Airbnb à Paris", x = "Prix (€)", y = "Nombre de locations") #

ggsave("/Users/baptisteaudouin/Documents/GitHub/MON.2.2/Graphs/price_repartition.pdf")


# boxplot
plot2 <- ggplot(data, aes(x = room_type, y = price)) +
  geom_boxplot(fill = "lightblue") +
  ylim(0, 500) +
  labs(title = "Prix par type de logement", x = "Type de logement", y = "Prix (€)")

ggsave("/Users/baptisteaudouin/Documents/GitHub/MON.2.2/Graphs/price_repartition_boxplot.pdf")

# violinplot
plot3 <- ggplot(data, aes(x = room_type, y = price, fill = room_type)) +
  geom_violin() +
  ylim(0, 500) +
  labs(title = "Distribution des prix par type de logement", x = "Type de logement", y = "Prix (€)")

ggsave("/Users/baptisteaudouin/Documents/GitHub/MON.2.2/Graphs/price_repartition_violonplot.pdf")


##4. Cartes des locations
install.packages("htmlwidgets")

library(htmlwidgets)

# Créer la carte interactive et l'enregistrer en fichier HTML
map <- leaflet(data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    radius = 3,
    color = ~ifelse(price > 200, "red", "green"),
    popup = ~paste(name, "<br>Prix : ", price, "€")
  ) %>%
  addLegend("bottomright", 
            colors = c("green", "red"), 
            labels = c("Prix bas", "Prix élevé"), 
            title = "Légende")
print(map)
# Sauvegarder la carte dans un fichier HTML
saveWidget(map, "/Users/baptisteaudouin/Documents/GitHub/MON.2.2/Graphs/map_simple.html")




# Exécuter l'application
#shinyApp(ui, server)
