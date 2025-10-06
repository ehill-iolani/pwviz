library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(shinydashboard)
library(shinyBS)
library(DT)
library(htmlwidgets)
library(leaflet)
library(tidyr)

#####################
### DATA CLEANING ###
#####################
# Clean the imported site data
sdat <- sdat[, c(2, 3, 5, 7, 8)]
sdat[1, 2] <- "Ala Wai Canal"
sdat$`Longitude Bottom` <- as.numeric(sdat$`Latitude Top`)
sdat$`Longitude Top` <- as.numeric(sdat$`Longitude Top`)
sdat <- sdat[!(sdat$Stream %in% c("Ala Wai Canal", "Pauoa",
              "Nuuanu", "Waihee", "Kaaawa", "Hakipuu", "Heeia", "Punaluu", "Waimanalo", "Kalihi")), ]
pwpalette <- c("Makiki" = "#2962FF", "Manoa" = "green", "Manoa-Palolo" = "orange", "Palolo" = "#FFDE21")
color_palette <- colorFactor(palette = pwpalette, domain = sdat$Stream)

# Restrict survey data to only paepae
ldat <- ldat %>%
  filter(ldat$`Survey type` == "Paepae")

# Remove Kalihi stream surveys
ldat <- ldat %>%
  filter(!(ldat$`Stream (from Site)` %in% c("Kalihi")))

# Format the date column in ldat
ldat$Date <- as.Date(ldat$Date)

# Add year column to ldat
ldat$Year <- format(ldat$Date, "%Y")

# Remove surveys from Ala Wai Canal, Pauoa, and Nuuanu
ldat <- ldat[!(ldat$`Stream (from Site)` %in% c("Ala Wai Canal", "Pauoa", "Nuuanu")), ]

# Retrive species names from column names
species <- colnames(ldat)[grep("(count)", colnames(ldat))]
speciesl <- gsub(" \\(count\\)", "", species)
speciesl <- speciesl[speciesl != "Native" & speciesl != "Non-native" & speciesl != "Total"]
speciesl <- sort(speciesl)

# Remove organizations with no abbreviation
odat <- odat[is.na(odat$Abbreviation) == FALSE, ]