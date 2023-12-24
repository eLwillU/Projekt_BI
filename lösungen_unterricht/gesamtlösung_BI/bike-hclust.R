library(dplyr)
library(stringr)
library(ISLR2)

setwd("C:/Users/Dominic/Projects/BI/Projekt_BI/gesamtlösung_BI_xml")
## inkosistenzen 
# das muss man eventuell anpassen
bike <- read.csv("bike.csv", stringsAsFactors = FALSE)
# alle buchstaben entfernen
bike <- bike %>%
  mutate(humidity = gsub('[a-z A-Z]','',humidity))

# casting
# auf richtigen datentyp casten
bike$humidity <- as.numeric(bike$humidity)
bike$holiday <- factor(bike$holiday, levels = c(0, 1), labels = c("no", "yes"))
bike$workingday <- factor(bike$workingday, levels = c(0, 1), labels = c("no", "yes"))
bike$season <- factor(bike$season, levels = c(1, 2, 3, 4), labels = c("spring",
                                                                      "summer", "fall", "winter"), ordered = TRUE )
bike$weather <- factor(bike$weather, levels = c(1,2,3,4), labels = c("clear","mist + cloudy", "light rain/snow", "heavy rain"))
bike$casual <- as.numeric(bike$casual)
bike$registered <- as.numeric(bike$registered)
bike$count <- as.numeric(bike$count)


# strings zu datum machen
library(lubridate)
bike <- bike %>%
  mutate(datetime = mdy_hm(datetime))

# adapt
# duplicates zusammenfC<hren mit string cleaning
unique(bike$sources)
bike$sources <- tolower(bike$sources)
bike$sources <- str_trim(bike$sources)
na_loc <- is.na(bike$sources)
bike$sources[na_loc] <- "unknown"

#Remove missing Values
bike <- bike %>% select_if(~ !any(is.na(.)))


library(dplyr)
library(ggplot2)


library(dplyr)

bike_copy <- bike
bike_filtered <- bike %>% 
  sample_n(2500) %>%
  select(temp, atemp, humidity, windspeed)


bike_filtered <- scale(bike_filtered)


model <- hclust(bike_filtered)


# Auswahl der relevanten Variablen
bike_data_selected <- bike_copy %>%
  sample_n(1000) %>%
  select(season, temp, atemp, humidity, windspeed) %>% 
  na.omit()
    # Normalisierung der Daten

# Hierarchisches Clustering durchführen
distance_matrix <- dist(bike_data_selected)  # Distanzmatrix berechnen
hclust_result <- hclust(distance_matrix)    # Hierarchisches Clustering durchführen

# Dendrogramm zeichnen
plot(hclust_result)


head(bike)

library(dplyr)

# Auswahl der relevanten numerischen Variablen
bike_data_selected <- bike %>%
  sample_n(2500) 
  select(temp, atemp, humidity, windspeed) 

# Normalisierung der numerischen Daten
bike_data_scaled <- scale(bike_data_selected)

# Falls 'season' als Faktor verwendet werden soll, muss es separat behandelt werden
# bike_data$season <- as.factor(bike_data$season)

# Hierarchisches Clustering durchführen
distance_matrix <- dist(bike_data_scaled)  # Distanzmatrix berechnen
hclust_result <- hclust(distance_matrix)  # Hierarchisches Clustering durchführen

# Dendrogramm zeichnen
plot(hclust_result)






