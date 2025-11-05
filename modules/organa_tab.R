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

# Suppress warnings for unbound global variables
globalVariables(c(
  "Organization", "Organization Classification",
  "Stream (from Site)", "Non-native (count)", "Native (count)",
  "Biomass", "Date", "ldat", "odat"
))

# Organization Analysis Tab Module
mod_organa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = tags$div(style = "font-size: 24px; font-weight: 600;", "Organization Analysis"),
        fluidRow(
          column(
            width = 6,
            title = "Select an Organization",
            selectInput(
              inputId = ns("organ_cat"),
              label = "Select an organization category:",
              choices = c("All", sort(as.character(unique(odat$`Organization Classification`)))),
              selected = "All"
            )
          ),
          column(
            width = 6,
            title = "Select a specific organization",
            uiOutput(ns("organ_a"))
          )
        )
      ),
      box(
        title = tags$div(style = "font-size: 24px; font-weight: 600;", "Organization Summary"),
        width = 12,
        valueBoxOutput(ns("org_invasive_sum"), width = 3),
        valueBoxOutput(ns("org_native_sum"), width = 3),
        valueBoxOutput(ns("org_biomass_sum"), width = 3),
        valueBoxOutput(ns("org_visits_sum"), width = 3)
      ),
      box(
        width = 12,
        plotlyOutput(ns("org_native"))
      ),
      box(
        width = 12,
        plotlyOutput(ns("org_nonnative"))
      ),
      box(
        width = 12,
        plotlyOutput(ns("org_biomass"))
      )
    )
  )
}

mod_organa_server <- function(id, ldat, odat, pwpalette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$organ_a <- renderUI({
      selectInput(
        inputId = ns("organ_a"),
        label = "Select an organization:",
        choices = c("All", sort(as.character(unique(odat$Organization[odat$`Organization Classification` == input$organ_cat])))),
        selected = "All"
      )
    })

    ldat_org <- reactive({
      if (input$organ_cat == "All") {
        ldat
      } else if (input$organ_a == "All") {
        ldat %>%
          filter(`Organization (from Organization)` %in% odat$Organization[odat$`Organization Classification` == input$organ_cat])
      } else {
        ldat %>%
          filter(`Organization (from Organization)` == input$organ_a)
      }
    })

    output$org_invasive_sum <- renderValueBox({
      valueBox(
        value = sum(ldat_org()$`Non-native (count)`),
        subtitle = "Number of invasive organisms removed",
        color = "red",
        icon = icon("remove-circle", lib = "glyphicon")
      )
    })

    output$org_native_sum <- renderValueBox({
      valueBox(
        value = sum(ldat_org()$`Native (count)`),
        subtitle = "Number of native organisms observed",
        color = "green",
        icon = icon("ok-circle", lib = "glyphicon")
      )
    })

    output$org_biomass_sum <- renderValueBox({
      valueBox(
        value = round(sum(ldat_org()$Biomass, na.rm = TRUE), digits = 4),
        subtitle = "Mass of invasives removed (lbs)",
        color = "purple",
        icon = icon("scale", lib = "glyphicon")
      )
    })

    output$org_visits_sum <- renderValueBox({
      valueBox(
        value = nrow(ldat_org()),
        subtitle = "Number of field surveys",
        color = "orange",
        icon = icon("eye-open", lib = "glyphicon")
      )
    })

    output$org_native <- renderPlotly({
      ggplotly(ggplot(ldat_org(), aes(x = Date, y = `Native (count)`, color = as.character(`Stream (from Site)`))) +
        geom_point() +
        geom_smooth(color = "black") +
        scale_color_manual(values = pwpalette) +
        labs(title = if (input$organ_a == "All") {paste("Number of native species collected through time by", input$organ_cat, sep = " ")}
          else {paste("Number of native species collected through time by", input$organ_a, sep = " ")},
             x = "Date",
             y = "Count",
             color = "Stream") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    output$org_nonnative <- renderPlotly({
      ggplotly(ggplot(ldat_org(), aes(x = Date, y = `Non-native (count)`, color = as.character(`Stream (from Site)`))) +
        geom_point() +
        geom_smooth(color = "black") +
        scale_color_manual(values = pwpalette) +
        labs(title = if (input$organ_a == "All") {paste("Number of non-native species collected through time by", input$organ_cat, sep = " ")}
          else {paste("Number of non-native species collected through time by", input$organ_a, sep = " ")},
             x = "Date",
             y = "Count",
             color = "Stream") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    output$org_biomass <- renderPlotly({
      ggplotly(ggplot(ldat_org(), aes(x = Date, y = Biomass, color = as.character(`Stream (from Site)`))) +
        geom_point() +
        geom_smooth(color = "black") +
        scale_color_manual(values = pwpalette) +
        labs(title = if (input$organ_a == "All") {paste("Biomass collected through time by", input$organ_cat, sep = " ")}
            else {paste("Biomass collected through time by", input$organ_a, sep = " ")},
             x = "Date",
             y = "Biomass (lbs)",
             color = "Stream") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })
  })
}