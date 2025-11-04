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
library(forcats)

# Suppress warnings for unbound global variables
utils::globalVariables(c(
  "Site", "Stream", "Date", "HSIBI", "Species", "Count",
  "Stream (from Site)", "Site (from Site)", "sdat", "ldat"
))

# Sites Through Time Tab Module

mod_sitesthrutime_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Site Trends Over Time",
        fluidRow(
          column(
            width = 9,
            title = "Site Map",
            leafletOutput(ns("site_map"))
          ),
          column(
            width = 3,
            title = "Select a stream",
            selectInput(
              inputId = ns("stream_a"),
              label = "Select a stream:",
              choices = c("All", sort(as.character(unique(sdat$Stream)))),
              selected = "All"
            )
          ),
          column(
            width = 3,
            title = "Select a site",
            uiOutput(ns("site_a"))
          )
        )
      ),
      box(
        width = 12,
        plotlyOutput(ns("site_trends"))
      ),
      box(
        width = 12,
        plotlyOutput(ns("native_non_native_trends"))
      ),
      box(
        width = 12,
        plotlyOutput(ns("hsibi_trends"))
      ),
      box(
        width = 12,
        title = "Site Data Table",
        fluidRow(
          column(
            width = 2,
            selectInput(
              inputId = ns("yearselect"),
              label = "Select a year:",
              choices = c("All", sort(as.character(unique(ldat$Year)), decreasing = TRUE))
            )
          ),
          column(
            width = 5,
            title = "Select an organization:",
            uiOutput(ns("organ_b"))
          ),
          column(
            width = 5,
            title = "Select a survey date:",
            uiOutput(ns("l3"))
          )
        ),
        fluidRow(
          column(
            width = 12,
            dataTableOutput(ns("site_data"))
          )
        ),
        fluidRow(
          column(
            width = 12,
            downloadButton(ns("downloadData"), "Download Data")
          )
        )
      )
    )
  )
}

# Update reactive expressions to handle missing or invalid inputs

mod_sitesthrutime_server <- function(id, ldat, sdat, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Debugging: Observe input values
    observe({
      print(paste("Selected stream:", input$stream_a))
      print(paste("Selected site:", input$site_a))
    })

    output$site_a <- renderUI({
      selectInput(
        inputId = ns("site_a"),
        label = "Select a site:",
        choices = c("All", as.character(sort(sdat$Site[sdat$Stream == input$stream_a]))),
        selected = "All"
      )
    })

    filtered_sdat <- reactive({

      if (is.null(input$stream_a) || is.null(input$site_a)) {
        return(sdat)
      }

      if (input$site_a == "All" && input$stream_a == "All") {
        sdat
      } else if (input$site_a != "All" && input$stream_a == "All") {
        sdat %>%
          filter(Site == input$site_a)
      } else if (input$site_a == "All" && input$stream_a != "All") {
        sdat %>%
          filter(Stream == input$stream_a)
      } else {
        sdat %>%
          filter(Site == input$site_a, Stream == input$stream_a)
      }
    })

    site_map <- reactive({
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addCircleMarkers(
          data = filtered_sdat(),
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

    output$site_map <- renderLeaflet({
      site_map()
    })

    ldat_site2 <- reactive({
      filtered_data <- ldat
      if (input$stream_a != "All") {
        filtered_data <- filtered_data %>%
          filter(`Stream (from Site)` == input$stream_a)
      }
      if (input$site_a != "All") {
        filtered_data <- filtered_data %>%
          filter(`Site (from Site)` == input$site_a)
      }

      return(filtered_data)
    })

    ldat_site_filtered <- reactive({
      data <- ldat_site2() %>%
        select(Date, HSIBI, `Site (from Site)`, `Stream (from Site)`, contains("(count)")) %>%
        pivot_longer(cols = -c(Date, `Site (from Site)`, `Stream (from Site)`, HSIBI),
                     names_to = "Species", values_to = "Count") %>%
        mutate(Species = gsub(" \\(count\\)", "", Species)) %>%
        filter(!is.na(Count) & Count > 0) %>%
        filter(!Species %in% c("Total", "Native", "Non-native"))
      
      data$`Site (from Site)` <- as.character(data$`Site (from Site)`)
      data$`Stream (from Site)` <- as.character(data$`Stream (from Site)`)
      return(data)
    })

    output$site_trends <- renderPlotly({
      ggplotly(ggplot(ldat_site_filtered(), aes(x = Date, y = Count, color = Species)) +
          geom_point() +
          geom_line(aes(group = Species), se = FALSE) +
          labs(title = "Counts by Species Through Time",
               x = "Date",
               y = "Count",
               color = "Species") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    ldat_site_filtered2 <- reactive({
      data <- ldat_site2() %>%
        select(Date, HSIBI, `Site (from Site)`, `Stream (from Site)`, contains("(count)")) %>%
        pivot_longer(cols = -c(Date, `Site (from Site)`, `Stream (from Site)`, HSIBI),
                     names_to = "Origin", values_to = "Count") %>%
        mutate(Origin = gsub(" \\(count\\)", "", Origin)) %>%
        filter(!is.na(Count) & Count > 0) %>%
        filter(Origin %in% c("Native", "Non-native")) %>%
        group_by(Date, `Site (from Site)`, `Stream (from Site)`, Origin) %>%
        summarise(Count = sum(Count, na.rm = TRUE)) %>%
        mutate(Percent = Count / sum(Count) * 100)

      return(data)
    })

    output$native_non_native_trends <- renderPlotly({
      ggplotly(ggplot(ldat_site_filtered2(), aes(x = Date, y = Percent, color = Origin)) +
          geom_point() +
          geom_line(aes(group = Origin), se = FALSE) +
          labs(title = "Native vs Non-Native Percentages Through Time",
               x = "Date",
               y = "Percentage of Total Count",
               color = "Origin") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    hdat <- reactive({
      ldat_site_filtered() %>%
        group_by(Date, HSIBI, `Site (from Site)`, `Stream (from Site)`) %>%
        distinct() %>%
        mutate(`Stream (from Site)` = as.factor(`Stream (from Site)`))
    })

    output$hsibi_trends <- renderPlotly({
      ggplotly(ggplot(hdat(), aes(x = Date, y = HSIBI, color = `Stream (from Site)`)) +
        geom_point() +
        geom_smooth(aes(group = 1), color = "black") +
        scale_color_manual(values = pwpalette) +
        labs(title = if (input$site_a == "All") {paste("HSIBI trends through time for", input$stream_a, "stream(s)", sep = " ")}
          else {paste("HSIBI trends through time for", input$site_a, sep = " ")},
             x = "Date",
             y = "HSIBI",
             color = "Stream") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    output$organ_b <- renderUI({
      selectInput(
        inputId = ns("organ_b"),
        label = "Select an organization:",
        choices = sort(as.character(unique(ldat$`Organization (from Organization)`[ldat$Year == input$yearselect]))),
        selected = NULL
      )
    })

    output$l3 <- renderUI({
      selectInput(
        inputId = ns("survey_date"),
        label = "Select a survey date:",
        choices = sort(as.character(unique(ldat$Date[ldat$Year == input$yearselect & ldat$`Organization (from Organization)` == input$organ_b]))),
        selected = NULL
      )
    })

    ldat_year_org <- reactive({
      if (input$yearselect == "All") {
        ldat
      } else {
        ldat %>%
          filter(Year == input$yearselect & `Organization (from Organization)` == input$organ_b & Date == input$survey_date)
      }
    })

    ldat_year_org_filt <- reactive({
      data <- ldat_year_org() %>%
        select(Date, HSIBI, `Site (from Site)`, `Stream (from Site)`, contains("(count)")) %>%
        pivot_longer(cols = -c(Date, `Site (from Site)`, `Stream (from Site)`, HSIBI),
                     names_to = "Species", values_to = "Count") %>%
        mutate(Species = gsub(" \\(count\\)", "", Species)) %>%
        mutate(Species = as.factor(Species)) %>%
        filter(!is.na(Count) & Count > 0) %>%
        mutate(`Stream (from Site)` = as.character(`Stream (from Site)`)) %>%
        mutate(`Stream (from Site)` = as.factor(`Stream (from Site)`)) %>%
        mutate(`Site (from Site)` = as.character(`Site (from Site)`)) %>%
        mutate(`Site (from Site)` = as.factor(`Site (from Site)`)) %>%
        # Place Native, Non-native, and Total at the bottom of the list
        mutate(Species = fct_relevel(Species, "Native", "Non-native", "Total", after = Inf)) %>%
        arrange(`Stream (from Site)`, Species)

      print(str(data))
      return(data)
    })

    output$site_data <- DT::renderDataTable({
      DT::datatable(ldat_year_org_filt(), options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
    })

    output$downloadData <- downloadHandler(filename = function() {
      paste("site_data_",
            unique(ldat_year_org_filt()$`Stream (from Site)`), "_",
            unique(ldat_year_org_filt()$`Site (from Site)`), "_",
            unique(ldat_year_org_filt()$Date),
            ".csv", sep = "")
    },
    content = function(file) {
      write.csv(as.data.frame(ldat_year_org_filt()),
                file, row.names = FALSE, quote = FALSE)
    })
  })
}