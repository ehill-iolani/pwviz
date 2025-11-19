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

# Manually specify the order of streams for mapping and plotting
sdat$Stream <- factor(sdat$Stream, levels = c("Makiki", "Manoa", "Palolo", "Manoa-Palolo"))

# Manually specify the order of sites for mapping and plotting
sdat$Site <- factor(sdat$Site, levels = c("Kanealole", "Halau Ku Mana", "Baker Park", "Washington Middle School",
                                          "Lyon Arboretum", "Waihi (USGS Gage)", "Waiakeakua (USGS Gage)", "Waakaua", "Manoa Valley District Park", "Manoa Marketplace", "Woodlawn Bridge (Noelani Elementary)", "Kanewai Loi", "Kanewai Field",
                                          "Anuenue School", "Palolo Elementary", "Jarrett Middle School", "Saint Louis Field", "Chaminade",
                                          "Manoa-Palolo Confluence", "Kaimuki  High School"))

# Define color palette for streams
pwpalette <- c("Makiki" = "#2962FF",
               "Manoa" = "green",
               "Manoa-Palolo" = "orange",
               "Palolo" = "#FFDE21")
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

# Coerce ldat$`Site (from Site)` to factor with levels in desired order
ldat$`Site (from Site)` <- factor(ldat$`Site (from Site)`, levels = levels(sdat$Site))

# Species cleanup - remove selected species columns
ldat <- ldat %>%
  select(-c(`Aurelia aurita (moon jelly) (count)`,
            `Bufo marinus (cane toad) (count)`, `Bufo marinus (cane toad) (size)`,
            `Caranx sexfasciatus (pake ulua/bigeye jack) (count)`, `Caranx sexfasciatus (pake ulua/bigeye jack) (size)`,
            `Unknown (count)`, `Unknown (size)`))

# Retrive species names from column names
species <- colnames(ldat)[grep("(count)", colnames(ldat))]
speciesl <- gsub(" \\(count\\)", "", species)
speciesl <- speciesl[speciesl != "Native" & speciesl != "Non-native" & speciesl != "Total"]
speciesl <- sort(speciesl)

# Remove organizations with no abbreviation
odat <- odat[is.na(odat$Abbreviation) == FALSE, ]