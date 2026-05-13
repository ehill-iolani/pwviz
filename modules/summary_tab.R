# Summary Tab Module
mod_summary_ui <- function(id, stream_choices) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = tags$div(style = "font-size:24px; font-weight:600;",
                         "Stream + Site Map"),
        width = 12,
        fluidRow(
          column(
            width = 9,
            shinycssloaders::withSpinner(
              leafletOutput(ns("map"), height = "700px"), type = 6)
          ),
          column(
            width = 3,
            div(style = "font-size:18px;",
              radioButtons(ns("stream"), "Select a stream:",
                           c("All", stream_choices), selected = "All")
            )
          )
        )
      ),
      box(
        title = tags$div(style = "font-size:24px; font-weight:600;",
                         "Overview Based on Stream Selection"),
        width = 12,
        fluidRow(
          valueBoxOutput(ns("invasive"), width = 3),
          valueBoxOutput(ns("native"), width = 3),
          div(id = ns("hsibi_help"),
            valueBoxOutput(ns("hsibi"), width = 3)
          ),
          bsTooltip(ns("hsibi"), "Click me to learn more!",
                    placement = "top"),
          bsModal(ns("hsibi_help_modal"), "What is HSIBI?",
                  ns("hsibi_help"), size = "large",
            p(HTML(paste0(
              "<b>HSIBI stands for the Hawai'i Stream Index of ",
              "Biological Integrity.</b><br><br>",
              "The HSIBI utilizes five ecological categories ",
              "(taxonomic richness, sensitive species, reproductive ",
              "capacity, trophic-habitat capacity, and tolerance ",
              "capacity) and 11 metrics to distinguish a stream's ",
              "biological condition on a scale ranging from ",
              "undisturbed to severely impaired.<br><br>",
              "<b>90 - 100: Excellent</b><br>",
              "<b>79 - 89.9: Good</b><br>",
              "<b>69 - 78.9: Fair</b><br>",
              "<b>40 - 68.9: Poor</b><br>",
              "<b>&lt; 39.9: Impaired</b><br><br>"
            )))
          ),
          valueBoxOutput(ns("biomass"), width = 3)
        ),
        fluidRow(
          div(id = ns("visits_help"),
            valueBoxOutput(ns("visits"), width = 4)
          ),
          bsTooltip(ns("visits"), "Click me to learn more!",
                    placement = "top"),
          bsModal(ns("visits_help_modal"), "What is a Paepae survey?",
                  ns("visits_help"), size = "large",
            p(HTML(paste0(
              "<b>A visit is a field survey conducted using the ",
              "Paepae method.</b><br><br>",
              "Paepae is a fish population survey method that ",
              "utilizes sound and vibration to herd animals ",
              "downstream to be collected and counted by the ",
              "survey team."
            )))
          ),
          valueBoxOutput(ns("drange"), width = 8)
        )
      )
    )
  )
}

mod_summary_server <- function(id, ldat, sdat, color_palette) {
  moduleServer(id, function(input, output, session) {

    output$map <- renderLeaflet({
      map_data <- if (input$stream == "All") {
        sdat
      } else {
        sdat %>% filter(Stream == input$stream)
      }
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addCircleMarkers(
          data = map_data,
          lng = ~`Longitude Top`,
          lat = ~`Latitude Top`,
          color = ~color_palette(Stream),
          popup = ~paste("Site:", Site, "<br/>",
                         "Stream:", Stream, "<br/>",
                         "Elevation (ft):", `Elevation Bottom`),
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
      dat <- if (input$stream == "All") {
        ldat
      } else {
        ldat %>% filter(`Stream (from Site)` == input$stream)
      }
      dat %>%
        summarise(
          Invasive = sum(`Non-native (count)`, na.rm = TRUE),
          Native   = sum(`Native (count)`, na.rm = TRUE),
          HSIBI    = round(mean(HSIBI, na.rm = TRUE), digits = 4),
          biomass  = round(sum(Biomass, na.rm = TRUE), digits = 4),
          visits   = nrow(dat),
          drange   = paste(
            range(Date, na.rm = TRUE)[1],
            range(Date, na.rm = TRUE)[2],
            sep = " to "
          )
        )
    })

    output$invasive <- renderValueBox({
      valueBox(
        value    = ldat_temp()$Invasive,
        subtitle = "Invasive organisms removed",
        color    = "red",
        icon     = icon("remove-circle", lib = "glyphicon")
      )
    })

    output$native <- renderValueBox({
      valueBox(
        value    = ldat_temp()$Native,
        subtitle = "Native organisms observed",
        color    = "green",
        icon     = icon("ok-circle", lib = "glyphicon")
      )
    })

    output$hsibi <- renderValueBox({
      valueBox(
        value    = ldat_temp()$HSIBI,
        subtitle = "Average HSIBI",
        color    = "blue",
        icon     = icon("heart-empty", lib = "glyphicon")
      )
    })

    output$biomass <- renderValueBox({
      valueBox(
        value    = ldat_temp()$biomass,
        subtitle = "Total invasive biomass removed (lbs)",
        color    = "purple",
        icon     = icon("scale", lib = "glyphicon")
      )
    })

    output$visits <- renderValueBox({
      valueBox(
        value    = ldat_temp()$visits,
        subtitle = "Number of field surveys",
        color    = "orange",
        icon     = icon("clipboard-list", lib = "font-awesome")
      )
    })

    output$drange <- renderValueBox({
      valueBox(
        value    = ldat_temp()$drange,
        subtitle = "Date range",
        color    = "yellow",
        icon     = icon("calendar", lib = "glyphicon")
      )
    })
  })
}
