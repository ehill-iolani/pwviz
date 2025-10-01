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

####################
### Pulling data ###
####################
# Read in the API key, basename, and table name from the .Renviron file
readRenviron("~")

# Source the helper functions
source("get_airtable_records.R")
source("./pages/summary_tab/summary_map.R")

# Verifies the all the necessary environment variables are set
if (Sys.getenv("AIRTABLE_API_KEY") == "")
  print("API key not found") else print("API key found")

if (Sys.getenv("AIRTABLE_BASE_NAME") == "")
  print("Base name not found") else print("Base name found")

if (Sys.getenv("AIRTABLE_L3DB_NAME") == "")
  print("Lesson 3 table name not found") else print("Lesson 3 table name found")

if (Sys.getenv("AIRTABLE_SITEDB_NAME") == "")
  print("Site table name not found") else print("Site table name found")

if (Sys.getenv("AIRTABLE_ORGANIZATIONDB_NAME") == "")
  print("Organization table name not found") else print("Organization table name found")

if (Sys.getenv("AIRTABLE_FISHDB_NAME") == "")
  print("Fish table name not found") else print("Fish table name found")

# Pulls data from Airtable
key <- Sys.getenv("AIRTABLE_API_KEY")
base <- Sys.getenv("AIRTABLE_BASE_NAME")
l3_table_name <- Sys.getenv("AIRTABLE_L3DB_NAME")
site_table_name <- Sys.getenv("AIRTABLE_SITEDB_NAME")
organziation_table_name <- Sys.getenv("AIRTABLE_ORGANIZATIONDB_NAME")
fish_table_name <- Sys.getenv("AIRTABLE_FISHDB_NAME")
key <- Sys.getenv("AIRTABLE_API_KEY")
record_id <- NULL

ldat <- get_airtable_records(base, l3_table_name, key, record_id)
sdat <- get_airtable_records(base, site_table_name, key, record_id)
odat <- get_airtable_records(base, organziation_table_name, key, record_id)
fdat <- get_airtable_records(base, fish_table_name, key, record_id)

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
pwpalette <- c("Makiki" = "blue", "Manoa" = "green", "Manoa-Palolo" = "orange", "Palolo" = "#FFDE21")
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

#################
### APP START ###
#################

# Source the summary tab module
source("modules/summary_tab.R")
# Source the species analysis tab module
source("modules/speciesa_tab.R")
# Source the sites through time tab module
source("modules/sitesthrutime_tab.R")
# Source the organization analysis tab module
source("modules/organa_tab.R")

# Define UI for application, homepage is map of sites
ui <- dashboardPage(
  dashboardHeader(title = "Paepae O Waikolu Stream Survey Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Paepae O Waikolu at a glance", tabName = "summary"),
      menuItem("Species analysis", tabName = "speciesa"),
      menuItem("Sites through time", tabName = "sitethrutime"),
      menuItem("Organization analysis", tabName = "organa")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
        mod_summary_ui("summary")
      ),
      tabItem(tabName = "speciesa",
        mod_species_ui("speciesa")
      ),
      tabItem(tabName = "sitethrutime",
        mod_sitesthrutime_ui("sitethrutime")
      ),
      tabItem(tabName = "organa",
        mod_organa_ui("organa")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  mod_summary_server("summary", ldat, sdat, color_palette)
  mod_species_server("speciesa", ldat, sdat, speciesl, pwpalette, color_palette)
  mod_sitesthrutime_server("sitethrutime", ldat, sdat, color_palette)
  mod_organa_server("organa", ldat, odat, pwpalette)
}

# Run the application
shinyApp(ui = ui, server = server)
