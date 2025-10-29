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

# Meet the Fish Tab Module
mod_meetfish_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Use dashboard-friendly layout: fluidRow/column
    fluidRow(
      column(12,
        h2("Meet the Fish of the Ala Wai Watershed"),
        h4("Under Construction"),
        p("This section is currently under development. Please check back later for more information about the fish species of the Ala Wai Watershed."),
        # PDF embeds
        fluidRow(
          column(7,
            div(class = "pdfobject-container",
              tags$iframe(src = "Invasive_Stream_Fauna.pdf#toolbar=1", type = "application/pdf", style = "width:100%; height:1200px;"),
              tags$div(class = "pdf-fallback", style = "margin-top:8px;",
                tags$a("Open Invasive Stream Fauna PDF in a new tab", href = "Invasive_Stream_Fauna.pdf", target = "_blank", rel = "noopener"),
                tags$br(),
                tags$a("Download Invasive Stream Fauna PDF", href = "Invasive_Stream_Fauna.pdf", download = "Invasive_Stream_Fauna.pdf")
              )
            )
          )
        ),
        fluidRow(
          column(7,
            div(class = "pdf-container",
              tags$iframe(src = "Native_Stream_Fauna.pdf#toolbar=1", type = "application/pdf", style = "width:100%; height:1200px;"),
              tags$div(class = "pdf-fallback", style = "margin-top:8px;",
                tags$a("Open Native Stream Fauna PDF in a new tab", href = "Native_Stream_Fauna.pdf", target = "_blank", rel = "noopener"),
                tags$br(),
                tags$a("Download Native Stream Fauna PDF", href = "Native_Stream_Fauna.pdf", download = "Native_Stream_Fauna.pdf")
              )
            )
          )
        )
      )
    )
  )
}