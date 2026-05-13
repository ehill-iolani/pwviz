# Species Analysis Tab Module
mod_species_ui <- function(id, speciesl) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      ".irs-grid-text { font-size: 16px !important; } ",
      ".irs-min, .irs-max, .irs-from, .irs-to, .irs-single ",
      "{ font-size: 16px !important; }"
    ))),
    fluidRow(
      box(
        width = 12,
        title = tags$div(style = "font-size: 24px; font-weight: 600;",
                         "Select a species and site"),
        fluidRow(
          column(
            width = 9,
            shinycssloaders::withSpinner(
              leafletOutput(ns("speciesmap"), height = "700px"), type = 6)
          ),
          column(
            width = 3,
            selectInput(
              inputId  = ns("species1"),
              label    = tags$div(style = "font-size: 16px;",
                                  "Select species 1:"),
              choices  = speciesl,
              selected = "Awaous stamineus"
            ),
            selectInput(
              inputId  = ns("species2"),
              label    = tags$div(style = "font-size: 16px;",
                                  "Select species 2:"),
              choices  = c("None", speciesl),
              selected = "None"
            )
          ),
          column(
            width = 3,
            uiOutput(ns("site_ui"))
          )
        )
      ),
      box(
        width = 12,
        # Safe defaults; server updates bounds from actual data
        sliderInput(
          ns("yearRange"),
          "Select Year Range:",
          min   = 1900,
          max   = as.numeric(format(Sys.Date(), "%Y")),
          value = c(1900, as.numeric(format(Sys.Date(), "%Y"))),
          step  = 1,
          sep   = ""
        )
      ),
      box(width = 12,
          shinycssloaders::withSpinner(
            plotlyOutput(ns("speciesplot")), type = 6)),
      box(width = 12,
          shinycssloaders::withSpinner(
            plotlyOutput(ns("speciesbarchart")), type = 6)),
      box(width = 12,
          shinycssloaders::withSpinner(
            plotlyOutput(ns("speciesbarchart2")), type = 6)),
      box(
        title = tags$div(style = "font-size: 24px; font-weight: 600;",
                         "Counts by Date and Organization"),
        width = 12,
        shinycssloaders::withSpinner(
          dataTableOutput(ns("data")), type = 6)
      )
    )
  )
}

mod_species_server <- function(id, ldat, sdat, speciesl,
                               pwpalette, color_palette) {
  moduleServer(id, function(input, output, session) {

    # Update year slider bounds from actual data
    if (!is.null(ldat) && "Date" %in% colnames(ldat)) {
      minYear <- tryCatch(
        as.numeric(format(min(ldat$Date, na.rm = TRUE), "%Y")),
        error = function(e) NA)
      maxYear <- tryCatch(
        as.numeric(format(max(ldat$Date, na.rm = TRUE), "%Y")),
        error = function(e) NA)
      if (!is.na(minYear) && !is.na(maxYear) && minYear <= maxYear) {
        updateSliderInput(session, "yearRange",
                          min = minYear, max = maxYear,
                          value = c(minYear, maxYear))
      }
    }

    # Selected species (excluding "None")
    spec_selected <- reactive({
      s <- c(input$species1, input$species2)
      s[s != "None"]
    })

    # Rows where at least one selected species has a count
    spdat <- reactive({
      selected <- spec_selected()
      if (length(selected) == 0) return(ldat[0, ])
      cols <- paste(selected, "(count)")
      ldat %>%
        filter(rowSums(!is.na(select(., all_of(cols)))) > 0)
    })

    output$site_ui <- renderUI({
      selectInput(
        inputId  = session$ns("site"),
        label    = tags$div(style = "font-size: 16px;", "Select a site:"),
        choices  = c("All", as.character(sort(unique(
          spdat()$`Site (from Site)`)))),
        selected = "All"
      )
    })

    # Filter by selected site
    spsitedat <- reactive({
      if (is.null(input$site) || input$site == "All") {
        spdat()
      } else {
        spdat() %>% filter(`Site (from Site)` == input$site)
      }
    })

    # Filter by year range
    spyeardat <- reactive({
      spsitedat() %>%
        filter(
          format(Date, "%Y") >= input$yearRange[1] &
          format(Date, "%Y") <= input$yearRange[2]
        )
    })

    # Pivot selected species to long format (shared base for all charts)
    splong <- reactive({
      selected <- spec_selected()
      dat <- spyeardat()
      if (nrow(dat) == 0 || length(selected) == 0) return(dat[0, ])
      if ("Species" %in% colnames(dat)) {
        dat <- dat %>% rename(Species_existing = Species)
      }
      dat_long <- tidyr::pivot_longer(
        dat,
        cols      = tidyselect::matches("\\(count\\)"),
        names_to  = "Species",
        values_to = "Count"
      )
      dat_long <- dat_long %>%
        filter(Species %in% paste(selected, "(count)"))
      dat_long$Species <- gsub(" \\(.+\\)", "", dat_long$Species)
      dat_long
    })

    # Map reactive
    spmap <- reactive({
      selected <- spec_selected()
      colors     <- c("#ff9b9b", "#a5d4f5")
      both_color <- "#dfc5fe"
      site_rows  <- sdat[sdat$Site %in%
                           unique(as.character(
                             spsitedat()$`Site (from Site)`)), ]
      map <- leaflet() %>% addProviderTiles("Esri.WorldImagery")

      if (length(selected) == 2) {
        col1 <- paste(selected[1], "(count)")
        col2 <- paste(selected[2], "(count)")
        both_sites <- spsitedat() %>%
          filter(!is.na(.data[[col1]]) & .data[[col1]] > 0 &
                   !is.na(.data[[col2]]) & .data[[col2]] > 0) %>%
          pull(`Site (from Site)`) %>% unique()
        both_rows <- site_rows[site_rows$Site %in% both_sites, ]
        if (nrow(both_rows) > 0) {
          map <- map %>% addCircleMarkers(
            data        = both_rows,
            lng         = ~`Longitude Top`,
            lat         = ~`Latitude Top`,
            color       = both_color,
            fillColor   = both_color,
            radius      = 10,
            popup       = ~paste("Site:", Site,
                                 "<br/>Species: Both",
                                 "<br/>Stream:", Stream),
            opacity     = 0.8,
            fillOpacity = 0.8,
            group       = "Both"
          )
        }
      } else {
        both_sites <- character(0)
      }

      for (i in seq_along(selected)) {
        col <- paste(selected[i], "(count)")
        sp_sites <- spsitedat() %>%
          filter(!is.na(.data[[col]]) & .data[[col]] > 0) %>%
          pull(`Site (from Site)`) %>% unique()
        sp_sites <- setdiff(sp_sites, both_sites)
        sp_rows  <- site_rows[site_rows$Site %in% sp_sites, ]
        if (nrow(sp_rows) > 0) {
          map <- map %>% addCircleMarkers(
            data        = sp_rows,
            lng         = ~`Longitude Top`,
            lat         = ~`Latitude Top`,
            color       = colors[i],
            fillColor   = colors[i],
            radius      = 10,
            popup       = ~paste("Site:", Site,
                                 "<br/>Species:", selected[i],
                                 "<br/>Stream:", Stream,
                                 "<br/>Elevation (ft):",
                                 `Elevation Bottom`),
            opacity     = 0.8,
            fillOpacity = 0.8,
            group       = selected[i]
          )
        }
      }

      legend_colors <- colors[seq_along(selected)]
      legend_labels <- selected
      if (length(selected) == 2) {
        legend_colors <- c(legend_colors, both_color)
        legend_labels <- c(legend_labels, "Both")
      }
      map %>% addLegend(
        position = "bottomright",
        colors   = legend_colors,
        labels   = legend_labels,
        title    = "Species",
        opacity  = 1
      )
    })

    output$speciesmap <- renderLeaflet({ spmap() })

    output$speciesplot <- renderPlotly({
      dat_long <- splong()
      selected <- spec_selected()
      req(nrow(dat_long) > 0)
      ggplotly(
        ggplot(dat_long, aes(x = Date, y = Count, color = Species)) +
          geom_point() +
          geom_smooth(aes(group = Species), color = "black") +
          scale_color_manual(values = c("#ff9b9b", "#a5d4f5")) +
          labs(
            title = paste("Number of", paste(selected, collapse = " & "),
                          "collected through time"),
            x = "Date", y = "Count", color = "Species"
          ) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    output$speciesbarchart <- renderPlotly({
      dat <- splong() %>%
        mutate(Year = format(Date, "%Y")) %>%
        group_by(Year, `Stream (from Site)`, Species) %>%
        summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")
      req(nrow(dat) > 0)
      ggplotly(
        ggplot(dat, aes(x = Year, y = Count, fill = Species)) +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_manual(values = c("#ff9b9b", "#a5d4f5")) +
          labs(title = "Number collected by year",
               x = "Year", y = "Count", fill = "Species") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    output$speciesbarchart2 <- renderPlotly({
      dat <- splong() %>%
        mutate(Year = format(Date, "%Y")) %>%
        group_by(Year, `Stream (from Site)`, Species) %>%
        summarise(Count = mean(Count, na.rm = TRUE), .groups = "drop")
      req(nrow(dat) > 0)
      ggplotly(
        ggplot(dat, aes(x = Year, y = round(Count, 0), fill = Species)) +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_manual(values = c("#ff9b9b", "#a5d4f5")) +
          labs(title = "Average number collected per survey",
               x = "Year", y = "Count", fill = "Species") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    output$data <- DT::renderDataTable({
      dat_long <- splong()
      req(nrow(dat_long) > 0)
      tbl <- data.frame(
        Date         = dat_long$Date,
        Species      = dat_long$Species,
        Count        = dat_long$Count,
        Organization = vapply(
          as.character(dat_long$`Organization (from Organization)`),
          format_org_html, character(1), USE.NAMES = FALSE),
        Site         = dat_long$`Site (from Site)`,
        Stream       = as.character(dat_long$`Stream (from Site)`),
        stringsAsFactors = FALSE
      )
      tbl <- tbl[order(tbl$Date, decreasing = TRUE), ]
      row.names(tbl) <- NULL
      DT::datatable(tbl, escape = FALSE, options = list(pageLength = 25))
    })
  })
}
