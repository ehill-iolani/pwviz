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
source("functions/get_airtable_records.R", local = TRUE)

# Source the data ingest script to pull data from Airtable
source("data_processing/data_ingest.R", local = TRUE)

#####################
### DATA CLEANING ###
#####################

# Source the data cleaning script to clean the pulled data
source("data_processing/data_cleaning.R", local = TRUE)

#################
### APP START ###
#################

# Source the summary tab module
source("modules/summary_tab.R", local = TRUE)
# Source the species analysis tab module
source("modules/speciesa_tab.R", local = TRUE)
# Source the sites through time tab module
source("modules/sitesthrutime_tab.R", local = TRUE)
# Source the organization analysis tab module
source("modules/organa_tab.R", local = TRUE)

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
