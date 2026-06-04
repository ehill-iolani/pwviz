# Sites Through Time Tab Module
mod_sitesthrutime_ui <- function(id, stream_choices) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = tags$div(style = "font-size: 24px; font-weight: 600;",
                         "Site Trends Over Time"),
        fluidRow(
          column(
            width = 9,
            shinycssloaders::withSpinner(
              leafletOutput(ns("site_map"), height = "700px"), type = 6)
          ),
          column(
            width = 3,
            selectInput(
              inputId  = ns("stream_a"),
              label    = tags$div(style = "font-size: 16px;",
                                  "Select a stream:"),
              choices  = c("All", stream_choices),
              selected = "All"
            )
          ),
          column(
            width = 3,
            uiOutput(ns("site_a"))
          )
        )
      ),
      box(width = 12,
          shinycssloaders::withSpinner(
            plotlyOutput(ns("site_trends")), type = 6)),
      box(width = 12,
          shinycssloaders::withSpinner(
            plotlyOutput(ns("native_non_native_trends")), type = 6)),
      box(width = 12,
          shinycssloaders::withSpinner(
            plotlyOutput(ns("hsibi_trends")), type = 6)),
      box(
        width = 12,
        title = tags$div(style = "font-size: 24px; font-weight: 600;",
                         "Site Data Table"),
        fluidRow(
          column(
            width = 2,
            # Safe default; server populates actual year choices
            selectInput(ns("yearselect"), "Select a year:",
                        choices = c("All"))
          ),
          column(width = 5, uiOutput(ns("organ_b"))),
          column(width = 5, uiOutput(ns("l3")))
        ),
        fluidRow(
          column(
            width = 12,
            shinycssloaders::withSpinner(
              dataTableOutput(ns("site_data")), type = 6)
          )
        ),
        fluidRow(
          column(width = 12,
                 downloadButton(ns("downloadData"), "Download Data"))
        )
      )
    )
  )
}

mod_sitesthrutime_server <- function(id, ldat, sdat,
                                     color_palette, pwpalette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate year choices from ldat
    if (!is.null(ldat) && "Year" %in% colnames(ldat)) {
      year_choices <- tryCatch(
        sort(as.character(unique(ldat$Year)), decreasing = TRUE),
        error = function(e) character(0))
      if (length(year_choices) > 0) {
        updateSelectInput(session, "yearselect",
                          choices  = c("All", year_choices),
                          selected = "All")
      }
    }

    output$site_a <- renderUI({
      site_choices <- if (is.null(input$stream_a) ||
                          input$stream_a == "All") {
        as.character(sort(unique(sdat$Site)))
      } else {
        as.character(sort(unique(
          sdat$Site[sdat$Stream == input$stream_a])))
      }
      selectInput(
        inputId  = ns("site_a"),
        label    = tags$div(style = "font-size: 16px;",
                            "Select a site:"),
        choices  = c("All", site_choices),
        selected = "All"
      )
    })

    filtered_sdat <- reactive({
      if (is.null(input$stream_a) || is.null(input$site_a)) return(sdat)
      stream_all <- input$stream_a == "All"
      site_all   <- input$site_a   == "All"
      if (stream_all && site_all) {
        sdat
      } else if (!site_all && stream_all) {
        sdat %>% filter(Site == input$site_a)
      } else if (site_all && !stream_all) {
        sdat %>% filter(Stream == input$stream_a)
      } else {
        sdat %>% filter(Site == input$site_a,
                        Stream == input$stream_a)
      }
    })

    output$site_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addCircleMarkers(
          data        = filtered_sdat(),
          lng         = ~`Longitude Top`,
          lat         = ~`Latitude Top`,
          color       = ~color_palette(Stream),
          popup       = ~paste("Site:", Site,
                               "<br/>Stream:", Stream,
                               "<br/>Elevation (ft):", `Elevation Bottom`),
          opacity     = 0.8,
          fillOpacity = 0.8
        ) %>%
        addLegend(
          data     = sdat,
          position = "bottomright",
          pal      = color_palette,
          values   = ~Stream,
          title    = "Stream",
          opacity  = 0.8
        )
    })

    ldat_site <- reactive({
      dat <- ldat
      if (!is.null(input$stream_a) && input$stream_a != "All") {
        dat <- dat %>% filter(`Stream (from Site)` == input$stream_a)
      }
      if (!is.null(input$site_a) && input$site_a != "All") {
        dat <- dat %>% filter(`Site (from Site)` == input$site_a)
      }
      dat
    })

    # Long format: one row per species per survey (excludes totals)
    ldat_site_long <- reactive({
      data <- ldat_site() %>%
        select(Date, HSIBI, `Site (from Site)`, `Stream (from Site)`,
               contains("(count)")) %>%
        pivot_longer(
          cols      = -c(Date, `Site (from Site)`, `Stream (from Site)`,
                         HSIBI),
          names_to  = "Species",
          values_to = "Count"
        ) %>%
        mutate(Species = gsub(" \\(count\\)", "", Species)) %>%
        filter(!is.na(Count) & Count > 0,
               !Species %in% c("Total", "Native", "Non-native"))
      data$`Site (from Site)`   <- as.character(data$`Site (from Site)`)
      data$`Stream (from Site)` <- as.character(data$`Stream (from Site)`)
      data
    })

    output$site_trends <- renderPlotly({
      dat <- ldat_site_long()
      req(nrow(dat) > 0)
      ggplotly(
        ggplot(dat, aes(x = Date, y = Count, color = Species)) +
          geom_point() +
          geom_line(aes(group = Species)) +
          labs(title = "Counts by Species Through Time",
               x = "Date", y = "Count", color = "Species") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    output$native_non_native_trends <- renderPlotly({
      dat <- ldat_site() %>%
        select(Date, HSIBI, `Site (from Site)`, `Stream (from Site)`,
               contains("(count)")) %>%
        pivot_longer(
          cols      = -c(Date, `Site (from Site)`, `Stream (from Site)`,
                         HSIBI),
          names_to  = "Origin",
          values_to = "Count"
        ) %>%
        mutate(Origin = gsub(" \\(count\\)", "", Origin)) %>%
        filter(!is.na(Count) & Count > 0,
               Origin %in% c("Native", "Non-native")) %>%
        group_by(Date, `Site (from Site)`, `Stream (from Site)`,
                 Origin) %>%
        summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
        group_by(Date, `Site (from Site)`, `Stream (from Site)`) %>%
        mutate(Percent = Count / sum(Count) * 100) %>%
        ungroup()
      req(nrow(dat) > 0)
      ggplotly(
        ggplot(dat, aes(x = Date, y = Percent, color = Origin)) +
          geom_point() +
          geom_line(aes(group = Origin)) +
          labs(title = "Native vs Non-Native Percentages Through Time",
               x = "Date", y = "Percentage of Total Count",
               color = "Origin") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    output$hsibi_trends <- renderPlotly({
      dat <- ldat_site_long() %>%
        group_by(Date, HSIBI, `Site (from Site)`, `Stream (from Site)`) %>%
        distinct() %>%
        mutate(`Stream (from Site)` = as.factor(`Stream (from Site)`))
      req(nrow(dat) > 0)
      title <- if (is.null(input$site_a) || input$site_a == "All") {
        paste("HSIBI trends through time for",
              input$stream_a, "stream(s)")
      } else {
        paste("HSIBI trends through time for", input$site_a)
      }
      ggplotly(
        ggplot(dat, aes(x = Date, y = HSIBI,
                        color = `Stream (from Site)`)) +
          geom_point() +
          geom_smooth(aes(group = 1), color = "black") +
          scale_color_manual(values = pwpalette) +
          labs(title = title, x = "Date", y = "HSIBI",
               color = "Stream") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    output$organ_b <- renderUI({
      rows <- if (is.null(input$yearselect) ||
                  input$yearselect == "All") {
        ldat
      } else {
        ldat[ldat$Year == input$yearselect, ]
      }
      raw_orgs   <- as.character(rows$`Organization (from Organization)`)
      parsed     <- sort(unique(unlist(lapply(raw_orgs, parse_orgs))))
      dates_raw  <- as.character(rows$Date)
      dates_pars <- suppressWarnings(as.Date(dates_raw))
      if (!all(is.na(dates_pars))) {
        recent_rows <- rows[which(dates_pars == max(dates_pars,
                                                    na.rm = TRUE)), ]
      } else {
        latest <- if (length(dates_raw) > 0) {
          sort(dates_raw, decreasing = TRUE)[1]
        } else {
          NA_character_
        }
        recent_rows <- if (!is.na(latest)) {
          rows[which(dates_raw == latest), ]
        } else {
          rows[0, ]
        }
      }
      recent_parsed <- unique(unlist(lapply(
        as.character(recent_rows$`Organization (from Organization)`),
        parse_orgs)))
      selected_org <- if (length(recent_parsed) > 0) {
        recent_parsed[1]
      } else {
        NULL
      }
      selectInput(
        inputId  = ns("organ_b"),
        label    = "Select an organization:",
        choices  = parsed,
        selected = selected_org
      )
    })

    output$l3 <- renderUI({
      rows <- if (is.null(input$yearselect) ||
                  input$yearselect == "All") {
        ldat
      } else {
        ldat[ldat$Year == input$yearselect, ]
      }
      if (!is.null(input$organ_b) && input$organ_b != "") {
        keep <- vapply(
          as.character(rows$`Organization (from Organization)`),
          FUN.VALUE = logical(1), USE.NAMES = FALSE,
          FUN = function(x) input$organ_b %in% parse_orgs(x))
        rows <- rows[keep, ]
      }
      choices <- sort(as.character(unique(rows$Date)), decreasing = TRUE)
      selectInput(
        inputId  = ns("survey_date"),
        label    = "Select a survey date:",
        choices  = choices,
        selected = if (length(choices) > 0) choices[1] else NULL
      )
    })

    ldat_year_org <- reactive({
      data <- ldat
      if (!is.null(input$yearselect) && input$yearselect != "All") {
        data <- data %>% filter(Year == input$yearselect)
      }
      if (!is.null(input$organ_b) && input$organ_b != "") {
        keep <- vapply(
          as.character(data$`Organization (from Organization)`),
          FUN.VALUE = logical(1), USE.NAMES = FALSE,
          FUN = function(x) input$organ_b %in% parse_orgs(x))
        data <- data[keep, ]
      }
      if (!is.null(input$survey_date) && input$survey_date != "") {
        data <- data %>% filter(Date == input$survey_date)
      }
      data
    })

    ldat_table <- reactive({
      data <- ldat_year_org() %>%
        select(Date, HSIBI, `Site (from Site)`, `Stream (from Site)`,
               contains("(count)")) %>%
        pivot_longer(
          cols      = -c(Date, `Site (from Site)`, `Stream (from Site)`,
                         HSIBI),
          names_to  = "Species",
          values_to = "Count"
        ) %>%
        mutate(
          Species            = gsub(" \\(count\\)", "", Species),
          Species            = as.factor(Species),
          `Stream (from Site)` = as.factor(
            as.character(`Stream (from Site)`)),
          `Site (from Site)` = as.factor(
            as.character(`Site (from Site)`)),
          Species = fct_relevel(Species, "Native", "Non-native",
                                "Total", after = Inf)
        ) %>%
        filter(!is.na(Count) & Count > 0) %>%
        arrange(`Stream (from Site)`, Species)

      data.frame(
        Date    = data$Date,
        HSIBI   = data$HSIBI,
        Stream  = data$`Stream (from Site)`,
        Site    = data$`Site (from Site)`,
        Species = data$Species,
        Count   = data$Count
      )
    })

    output$site_data <- DT::renderDataTable({
      tbl <- ldat_table()
      DT::datatable(tbl,
                    options  = list(pageLength = 10, autoWidth = TRUE),
                    rownames = FALSE)
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("site_data_",
               paste(unique(ldat_table()$Stream), collapse = "-"), "_",
               paste(unique(ldat_table()$Site),   collapse = "-"), "_",
               paste(unique(ldat_table()$Date),   collapse = "-"),
               ".csv")
      },
      content = function(file) {
        write.csv(ldat_table(), file, row.names = FALSE, quote = FALSE)
      }
    )
  })
}
