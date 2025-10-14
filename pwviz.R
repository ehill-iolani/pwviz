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
library(shinyjs)

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
# Source the meet the fish tab module
source("modules/meetfish_TEMP_tab.R", local = TRUE)

# Define UI for application, homepage is map of sites
ui <- dashboardPage(
  dashboardHeader(
    title = "Paepae O Waikolu Stream Survey Dashboard"
  ),
  dashboardSidebar(
    div(
      style = "display: flex; justify-content: center; align-items: center; gap: 10px; margin: 20px 0;",
      tags$img(src = "IS_Logo_Vertical_CommunityScience.png", height = "80px"),
      tags$img(src = "PoWLogoFinal.png", height = "80px")
    ),
    sidebarMenu(
      menuItem("Paepae O Waikolu at a glance", tabName = "summary"),
      menuItem("Meet the Fish", tabName = "meetfish"),
      menuItem("Species analysis", tabName = "speciesa"),
      menuItem("Sites through time", tabName = "sitethrutime"),
      menuItem("Organization analysis", tabName = "organa")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$title("Paepae O Waikolu Stream Survey Dashboard"),
      tags$link(rel = "icon",
                type = "image/png",
                href = "favicon-32x32.png"),
      tags$link(rel = "icon",
                type = "image/png",
                href = "favicon-16x16.png"),
      tags$link(rel = "apple-touch-icon",
                type = "image/png",
                href = "apple-touch-icon.png")
    ),
    tabItems(
      tabItem(tabName = "summary",
        mod_summary_ui("summary")
      ),
      tabItem(tabName = "meetfish",
        mod_meetfish_ui("meetfish")
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

  ###############################
  ### LOGIN + Fair use module ###
  ###############################
  # Login modal logic - uncomment to enable login
  # login_success <- reactiveVal(FALSE)
  # login_error <- reactiveVal("")

  # observe({
  #   if (!login_success()) {
  #     showModal(
  #       modalDialog(
  #         title = "Login Required",
  #         tagList(
  #           textInput("login_user", "Username"),
  #           passwordInput("login_pass", "Password"),
  #           div(id = "login_error_msg", style = "color: red;", textOutput("login_error")),
  #           actionButton("login_btn", "Login", class = "btn-primary")
  #         ),
  #         easyClose = FALSE,
  #         footer = NULL
  #       )
  #     )
  #   }
  # })

  # output$login_error <- renderText({ login_error() })

  # observeEvent(input$login_btn, {
  #   user <- Sys.getenv("DASHBOARD_USER")
  #   pass <- Sys.getenv("DASHBOARD_PASS")
  #   if (nzchar(user) && nzchar(pass) &&
  #       identical(input$login_user, user) && identical(input$login_pass, pass)) {
  #     login_success(TRUE)
  #     login_error("")
  #     removeModal()
  #     # Show fair use modal after successful login
  #     showModal(
  #       modalDialog(
  #         title = "Welcome to the Paepae O Waikolu Stream Survey Dashboard",
  #         tagList(
  #           p("This dashboard provides insights into the stream survey data collected by various organizations.",
  #             "Navigate through the tabs to explore different analyses and visualizations."),
  #           p("By continuing, you acknowledge and agree to use the data fairly and responsibly as outlined by Paepae O Waikolu."),
  #           checkboxInput("fair_use_ack", "I acknowledge and agree to fair data use.", value = FALSE),
  #           actionButton("continue_btn", "Continue", class = "btn-primary")
  #         ),
  #         easyClose = FALSE,
  #         footer = NULL
  #       )
  #     )
  #     shinyjs::disable("continue_btn")
  #   } else {
  #     login_error("Invalid username or password.")
  #   }
  # })

  # # Fair use modal logic (enable/disable button)
  # observe({
  #   if (login_success()) {
  #     if (isTRUE(input$fair_use_ack)) {
  #       shinyjs::enable("continue_btn")
  #     } else {
  #       shinyjs::disable("continue_btn")
  #     }
  #   }
  # })

  # observeEvent(input$continue_btn, {
  #   if (login_success()) {
  #     removeModal()
  #   }
  # })

  ############################
  ### ONLY Fair use module ###
  ############################
  # Only show the fair use modal at app start - delete when login is enabled
  observe({
    if (TRUE) {
      showModal(
        modalDialog(
          title = "Welcome to the Paepae O Waikolu Stream Survey Dashboard",
          tagList(
            p("This dashboard provides insights into the stream survey data collected by various organizations in partnership with Paepae O Waikolu.",
              "Navigate through the tabs to explore different analyses and visualizations."),
            p("By continuing, you acknowledge and agree to use the data fairly and responsibly as outlined by Paepae O Waikolu."),
            checkboxInput("fair_use_ack", "I acknowledge and agree to fair data use.", value = FALSE),
            actionButton("continue_btn", "Continue", class = "btn-primary")
          ),
          easyClose = FALSE,
          footer = NULL
        )
      )
      shinyjs::disable("continue_btn")
    }
  })

  observe({
    if (TRUE) {
      if (isTRUE(input$fair_use_ack)) {
        shinyjs::enable("continue_btn")
      } else {
        shinyjs::disable("continue_btn")
      }
    }
  })

  observeEvent(input$continue_btn, {
    if (TRUE) {
      removeModal()
    }
  })

  #########################
  ### PAGE SERVER CALLS ###
  #########################
  # Server calls for each module
  mod_summary_server("summary", ldat, sdat, color_palette)
  mod_species_server("speciesa", ldat, sdat, speciesl, pwpalette, color_palette)
  mod_sitesthrutime_server("sitethrutime", ldat, sdat, color_palette)
  mod_organa_server("organa", ldat, odat, pwpalette)
}

# Run the application
shinyApp(ui = ui, server = server)
