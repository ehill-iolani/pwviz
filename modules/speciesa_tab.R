# Species Analysis Tab Module

mod_species_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Select a species and site",
        fluidRow(
          column(
            width = 9,
            leafletOutput(ns("speciesmap"))
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("specieslist"),
              label = "Select a species:",
              choices = speciesl
            )
          ),
          column(
            width = 3,
            uiOutput(ns("site_ui"))
          )
        )
      ),
      tags$style(HTML(".irs-grid-text { font-size: 12px !important; } .irs-min, .irs-max, .irs-from, .irs-to, .irs-single { font-size: 12px !important; }")),
      box(
        width = 12,
        sliderInput(
          ns("yearRange"),
          "Select Year Range:",
          min = as.numeric(format(min(ldat$Date), "%Y")),
          max = as.numeric(format(max(ldat$Date), "%Y")),
          value = c(as.numeric(format(min(ldat$Date), "%Y")), as.numeric(format(max(ldat$Date), "%Y"))),
          step = 1,
          sep = ""
        )
      ),
      box(
        width = 12,
        plotlyOutput(ns("speciesplot"))
      ),
      box(
        width = 12,
        plotlyOutput(ns("speciesbarchart"))
      ),
      box(
        width = 12,
        plotlyOutput(ns("speciesbarchart2"))
      ),
      box(
        title = "Counts by Date and Organization",
        width = 12,
        dataTableOutput(ns("data"))
      )
    )
  )
}

mod_species_server <- function(id, ldat, sdat, speciesl, pwpalette, color_palette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spdat <- reactive({
      selected_columns <- ldat %>%
        select(contains(paste(input$specieslist, "(count)", sep = " ")))

      filtered_rows <- ldat %>%
        filter(rowSums(!is.na(selected_columns)) > 0)

      as.data.frame(filtered_rows)
    })

    spsitedat <- reactive({
      if (input$site == "All") {
        spdat()
      } else {
        spdat() %>%
          filter(`Site (from Site)` == input$site)
      }
    })

    output$site_ui <- renderUI({
      selectInput(
        inputId = ns("site"),
        label = "Select a site:",
        choices = c("All", sort(as.character(unique(spdat()$`Site (from Site)`)))),
        selected = "All"
      )
    })

    spyeardat <- reactive({
      spsitedat() %>%
        filter(format(Date, "%Y") >= input$yearRange[1] & format(Date, "%Y") <= input$yearRange[2])
    })

    spmap <- reactive({
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addCircleMarkers(
          data = if (input$site == "All") {
            sdat[sdat$Site %in% unique(as.character(spsitedat()$`Site (from Site)`)), ]
          } else {
            sdat %>%
              filter(Site == input$site)
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

    output$speciesmap <- renderLeaflet({
      spmap()
    })

    output$speciesplot <- renderPlotly({
      ggplotly(ggplot(spyeardat(), aes(x = Date, y = get(paste(input$specieslist, "(count)", sep = " ")), color = as.character(`Stream (from Site)`))) +
          geom_point() +
          geom_smooth(aes(group = 1), color = "black") +
          scale_color_manual(values = pwpalette) +
          labs(title = paste("Number of", input$specieslist, "collected through time"),
               x = "Date",
               y = paste(input$specieslist, "count", sep = " "),
               color = "Stream") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    spyeardat2 <- reactive({
      spyeardat() %>%
        mutate(Year = format(Date, "%Y")) %>%
        group_by(Year, `Stream (from Site)`) %>%
        summarise(Count = sum(get(paste(input$specieslist, "(count)", sep = " "))))
    })

    output$speciesbarchart <- renderPlotly({
      ggplotly(ggplot(spyeardat2(), aes(x = Year, y = Count, fill = as.character(`Stream (from Site)`))) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = pwpalette) +
          labs(title = paste("Number of", input$specieslist, "collected by year"),
               x = "Year",
               y = paste(input$specieslist, "count", sep = " "),
               fill = "Stream") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    spyeardat3 <- reactive({
      spyeardat() %>%
        mutate(Year = format(Date, "%Y")) %>%
        group_by(Year, `Stream (from Site)`) %>%
        summarise(Count = mean(get(paste(input$specieslist, "(count)", sep = " "))))
    })

    output$speciesbarchart2 <- renderPlotly({
      ggplotly(ggplot(spyeardat3(), aes(x = Year, y = round(Count, digits = 0), fill = as.character(`Stream (from Site)`))) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = pwpalette) +
          labs(title = paste("Average number of", input$specieslist, "collected per survey"),
               x = "Year",
               y = paste(input$specieslist, "count", sep = " "),
               fill = "Stream") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    dcsdat <- reactive({
      temp <- spyeardat()[grep(input$specieslist, colnames(spyeardat()))]
      temp <- temp[, grep("count", colnames(temp))]
      yuh <- data.frame(
              Date = spyeardat()$Date,
              Count = temp,
              Organization = as.character(spyeardat()$`Organization (from Organization)`))
      yuh <- yuh[order(yuh$Date), ]
      row.names(yuh) <- 1:nrow(yuh)
      yuh
    })

    output$data <- DT::renderDataTable({
      DT::datatable(dcsdat())
    })
  })
}