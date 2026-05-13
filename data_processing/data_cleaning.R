#####################
### DATA CLEANING ###
#####################

# Select relevant columns from site data
sdat <- sdat[, c("Stream", "Site",
                 "Latitude Top", "Longitude Top",
                 "Latitude Bottom", "Longitude Bottom",
                 "Elevation Bottom"
)]

# Remove alaw ai canal sites from site data
sdat <- sdat[!grepl("NA", sdat$Stream), ]

# Coerce all coordinate columns to numeric
sdat$`Latitude Top`     <- as.numeric(sdat$`Latitude Top`)
sdat$`Longitude Top`    <- as.numeric(sdat$`Longitude Top`)
sdat$`Latitude Bottom`  <- as.numeric(sdat$`Latitude Bottom`)
sdat$`Longitude Bottom` <- as.numeric(sdat$`Longitude Bottom`)

# Remove streams not used in this dashboard
sdat <- sdat[!(sdat$Stream %in% c(
  "Ala Wai Canal", "Pauoa", "Nuuanu", "Waihee",
  "Kaaawa", "Hakipuu", "Heeia", "Punaluu", "Waimanalo", "Kalihi"
)), ]

# Manually specify the order of streams for mapping and plotting
sdat$Stream <- factor(sdat$Stream,
  levels = c("Makiki", "Manoa", "Palolo", "Manoa-Palolo"))

# Manually specify the order of sites for mapping and plotting
sdat$Site <- factor(sdat$Site, levels = c(
  "Kanealole", "Halau Ku Mana", "Baker Park", "Washington Middle School",
  "Lyon Arboretum", "Waihi (USGS Gage)", "Waiakeakua (USGS Gage)",
  "Waakaua", "Manoa Valley District Park", "Manoa Marketplace",
  "Woodlawn Bridge (Noelani Elementary)", "Kanewai Loi", "Kanewai Field",
  "Anuenue School", "Palolo Elementary", "Jarrett Middle School",
  "Saint Louis Field", "Chaminade",
  "Manoa-Palolo Confluence", "Kaimuki  High School"
))

# Define color palette for streams
pwpalette <- c(
  "Makiki"       = "#2962FF",
  "Manoa"        = "green",
  "Manoa-Palolo" = "orange",
  "Palolo"       = "#FFDE21"
)
color_palette <- colorFactor(palette = pwpalette, domain = sdat$Stream)

# Restrict survey data to Paepae surveys only
ldat <- ldat %>%
  filter(`Survey type` == "Paepae")

# Remove streams not used in this dashboard
ldat <- ldat %>%
  filter(!(`Stream (from Site)` %in% c(
    "Kalihi", "Ala Wai Canal", "Pauoa", "Nuuanu"
  )))

# Format the date column and derive year
ldat$Date <- as.Date(ldat$Date)
ldat$Year <- format(ldat$Date, "%Y")

# Coerce site to factor with levels matching sdat
ldat$`Site (from Site)` <- factor(
  ldat$`Site (from Site)`,
  levels = levels(sdat$Site)
)

# Remove species columns that are excluded from analysis
ldat <- ldat %>%
  select(-c(
    `Aurelia aurita (moon jelly) (count)`,
    `Bufo marinus (cane toad) (count)`,
    `Bufo marinus (cane toad) (size)`,
    `Caranx sexfasciatus (pake ulua/bigeye jack) (count)`,
    `Caranx sexfasciatus (pake ulua/bigeye jack) (size)`,
    `Unknown (count)`,
    `Unknown (size)`
  ))

# Build species list from column names
species  <- colnames(ldat)[grep("(count)", colnames(ldat), fixed = TRUE)]
speciesl <- gsub(" \\(count\\)", "", species)
speciesl <- speciesl[!speciesl %in% c("Native", "Non-native", "Total")]
speciesl <- sort(speciesl)

# Remove organizations with no abbreviation
odat <- odat[!is.na(odat$Abbreviation), ]
