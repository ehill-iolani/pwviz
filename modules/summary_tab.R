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

# Summary Tab Module
mod_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Stream + Site Map",
        width = 12,
        fluidRow(
          column(
            width = 9,
            leafletOutput(ns("map"))
          ),
          column(
            width = 3,
            radioButtons(ns("stream"), "Select a stream:", c("All", unique(sdat$Stream)), selected = "All")
          )
        )
      ),
      box(
        title = "Overview Based on Stream Selection",
        width = 12,
        fluidRow(
          valueBoxOutput(ns("invasive"), width = 3),
          valueBoxOutput(ns("native"), width = 3),
          div(id = ns("hsibi_help"),
            valueBoxOutput(ns("hsibi"), width = 3)
          ),
          bsTooltip(ns("hsibi"), "Click me to learn more!", placement = "top"),
          bsModal(ns("hsibi_help_modal"), "What is HSIBI?", ns("hsibi_help"), size = "large",
            p(HTML("<b>HSIBI stands for the Hawai'i Stream Index of Biological Integrity.</b><br><br>
              The HSIBI utilizes five ecological categories(taxonomic richness, 
              sensitive species, reproductive capacity, trophic-habitat capacity, and
              tolerance capacity) and 11 metrics to distinguish a stream's biological 
              condition on a scale ranging from undisturbed to severely impaired.<br><br>
              <b>90 - 100: Excellent</b><br>
              <b>79 - 89.9: Good</b><br>
              <b>69 - 78.9: Fair</b><br>
              <b>40 - 68.9: Poor</b><br>
              <b>< 39.9: Impaired</b><br><br>
              "))
          ),
          valueBoxOutput(ns("biomass"), width = 3)
        ),
        fluidRow(
          div(id = ns("visits_help"),
            valueBoxOutput(ns("visits"), width = 4)
          ),
          bsTooltip(ns("visits"), "Click me to learn more!", placement = "top"),
          bsModal(ns("visits_help_modal"), "What is a Paepae survey?", ns("visits_help"), size = "large",
            p(HTML("<b>A visit is a field survey conducted using the Paepae method.</b><br><br>
              Paepae is a fish population survey method that utilizes sound and vibration 
              to herd animals downstream to be collected and counted by a the survey team."))
          ),
          valueBoxOutput(ns("drange"), width = 8)
        )
      )
    )
  )
}

mod_summary_server <- function(id, ldat, sdat, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addCircleMarkers(
          data = if (input$stream == "All") {
            sdat
          } else {
            sdat %>%
              filter(Stream == input$stream)
          },
          lng = ~`Longitude Top`,
          lat = ~`Latitude Top`,
          color = ~color_palette(Stream),
          popup = ~paste("Site: ", Site, "<br/>", "Stream: ", Stream, "<br/>"),
          opacity = 0.8,
          fillOpacity = 0.8
        ) %>%
        addLegend(
          data = sdat,
          position = "bottomright",
          pal = color_palette,
          values = ~Stream,
          title = "Stream",
          opacity = 0.8
        )
    })

    ldat_temp <- reactive({
      if (input$stream == "All") {
        ldat %>%
          summarise(
            Invasive = sum(`Non-native (count)`),
            Native = sum(`Native (count)`),
            HSIBI = round(mean(`HSIBI`, na.rm = TRUE), digits = 4),
            biomass = round(sum(Biomass, na.rm = TRUE), digits = 4),
            visits = nrow(ldat),
            drange = paste(range(`Date`, na.rm = TRUE)[1], range(`Date`, na.rm = TRUE)[2], sep = " to ")
          )
      } else {
        ldat %>%
          filter(`Stream (from Site)` == input$stream) %>%
          summarise(
            Invasive = sum(`Non-native (count)`),
            Native = sum(`Native (count)`),
            HSIBI = round(mean(HSIBI, na.rm = TRUE), digits = 4),
            biomass = round(sum(Biomass, na.rm = TRUE), digits = 4),
            visits = nrow(ldat[ldat$`Stream (from Site)` == input$stream, ]),
            drange = paste(range(`Date`, na.rm = TRUE)[1], range(`Date`, na.rm = TRUE)[2], sep = " to ")
          )
      }
    })

    output$invasive <- renderValueBox({
      valueBox(
        value = ldat_temp()$Invasive,
        subtitle = "Invasive organisms removed",
        color = "red",
        icon = icon("remove-circle", lib = "glyphicon")
      )
    })

    output$native <- renderValueBox({
      valueBox(
        value = ldat_temp()$Native,
        subtitle = "Native organisms observed",
        color = "green",
        icon = icon("ok-circle", lib = "glyphicon")
      )
    })

    output$hsibi <- renderValueBox({
      valueBox(
        value = ldat_temp()$HSIBI,
        subtitle = "Average HSIBI",
        color = "blue",
        icon = icon("heart-empty", lib = "glyphicon")
      )
    })

    output$biomass <- renderValueBox({
      valueBox(
        value = ldat_temp()$biomass,
        subtitle = "Total invasive biomass removed (lbs)",
        color = "purple",
        icon = icon("scale", lib = "glyphicon")
      )
    })

    output$visits <- renderValueBox({
      valueBox(
        value = ldat_temp()$visits,
        subtitle = "Number of field surveys",
        color = "orange",
        icon = icon("eye-open", lib = "glyphicon")
      )
    })

    output$drange <- renderValueBox({
      valueBox(
        value = ldat_temp()$drange,
        subtitle = "Date range",
        color = "yellow",
        icon = icon("calendar", lib = "glyphicon")
      )
    })
  })
}