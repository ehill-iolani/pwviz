library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(plotly)
library(DT)
library(htmlwidgets)
library(leaflet)
library(httr)
library(jsonlite)

####################
### Pulling data ###
####################

readRenviron("~")

source("functions/get_airtable_records.R", local = TRUE)
source("functions/utils.R", local = TRUE)

source("data_processing/data_ingest.R", local = TRUE)

#####################
### DATA CLEANING ###
#####################

source("data_processing/data_cleaning.R", local = TRUE)

#################
### APP START ###
#################

source("modules/summary_tab.R", local = TRUE)
source("modules/speciesa_tab.R", local = TRUE)
source("modules/sitesthrutime_tab.R", local = TRUE)
source("modules/organa_tab.R", local = TRUE)
source("modules/meetfish_TEMP_tab.R", local = TRUE)

stream_choices <- as.character(sort(unique(sdat$Stream)))

ui <- dashboardPage(
  dashboardHeader(
    title = "Paepae O Waikolu",
    tags$li(class = "dropdown",
      actionButton("questions_btn", label = NULL, icon = icon("question-circle"),
                   title = "Questions / Contact", style = "margin: 10px;")
    )
  ),
  dashboardSidebar(
    div(
      style = paste("display: flex; justify-content: center;",
                    "align-items: center; gap: 10px; margin: 20px 0;"),
      tags$img(src = "IS_Logo_Vertical_CommunityScience.png", height = "80px"),
      tags$img(src = "PoWLogoFinal.png", height = "80px")
    ),
    sidebarMenu(
      menuItem("At a glance",          tabName = "summary"),
      menuItem("Meet the Fish",        tabName = "meetfish"),
      menuItem("Species analysis",     tabName = "speciesa"),
      menuItem("Sites through time",   tabName = "sitethrutime"),
      menuItem("Organization analysis", tabName = "organa")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "favicon-32x32.png"),
      tags$link(rel = "icon", type = "image/png", href = "favicon-16x16.png"),
      tags$link(rel = "apple-touch-icon", type = "image/png",
                href = "apple-touch-icon.png"),
      tags$style(HTML("
        .main-sidebar .sidebar .sidebar-menu > li > a {
          font-size: 18px !important;
          padding: 12px 10px !important;
          white-space: normal;
        }
        .main-sidebar .sidebar .sidebar-menu .treeview-menu > li > a {
          font-size: 16px !important;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "summary",
        mod_summary_ui("summary", stream_choices)
      ),
      tabItem(tabName = "meetfish",
        mod_meetfish_ui("meetfish")
      ),
      tabItem(tabName = "speciesa",
        mod_species_ui("speciesa", speciesl)
      ),
      tabItem(tabName = "sitethrutime",
        mod_sitesthrutime_ui("sitethrutime", stream_choices)
      ),
      tabItem(tabName = "organa",
        mod_organa_ui("organa")
      )
    )
  )
)

server <- function(input, output, session) {

  ############################
  ### Fair use modal       ###
  ############################

  observe({
    showModal(
      modalDialog(
        title = "Welcome to the Paepae O Waikolu Stream Survey Dashboard",
        tagList(
          p(paste(
            "This dashboard provides insights into the stream survey data",
            "collected by various organizations in partnership with",
            "Paepae O Waikolu.",
            "Navigate through the tabs to explore different analyses",
            "and visualizations."
          )),
          p(paste(
            "By continuing, you acknowledge and agree to use the data",
            "fairly and responsibly as outlined by Paepae O Waikolu."
          )),
          checkboxInput("fair_use_ack",
                        "I acknowledge and agree to fair data use.",
                        value = FALSE),
          actionButton("continue_btn", "Continue", class = "btn-primary")
        ),
        easyClose = FALSE,
        footer = NULL
      )
    )
    shinyjs::disable("continue_btn")
  })

  observe({
    if (isTRUE(input$fair_use_ack)) {
      shinyjs::enable("continue_btn")
    } else {
      shinyjs::disable("continue_btn")
    }
  })

  observeEvent(input$continue_btn, {
    removeModal()
  })

  observeEvent(input$questions_btn, {
    showModal(
      modalDialog(
        title = "Questions & Contact",
        tagList(
          p(tags$strong("For questions regarding PWViz functionality/feedback/feature requests:")),
          p("Name:", " Ethan Hill"),
          p("Email:", a("ehill@iolani.org", href = "mailto:ehill@iolani.org")),
          p(tags$strong("For questions regarding the data/data collection:")),
          p("Name:", " Cory Yap"),
          p("Email:", a("coryy@hawaii.edu", href = "mailto:coryy@hawaii.edu")),
          p(tags$strong("For general questions regarding Paepae O Waikolu:")),
          p("Name:", " Yvonne Chan"),
          p("Email:", a("ychan@iolani.org", href = "mailto:ychan@iolani.org"))
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })

  #########################
  ### PAGE SERVER CALLS ###
  #########################

  mod_summary_server("summary", ldat, sdat, color_palette)
  mod_species_server("speciesa", ldat, sdat, speciesl, pwpalette, color_palette)
  mod_sitesthrutime_server("sitethrutime", ldat, sdat, color_palette, pwpalette)
  mod_organa_server("organa", ldat, odat, pwpalette)
}

shinyApp(ui = ui, server = server)
