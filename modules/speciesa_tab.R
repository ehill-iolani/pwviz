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
              inputId = ns("species1"),
              label = "Select species 1:",
              choices = c(speciesl),
              selected = "Awaous stamineus"
            ),
            selectInput(
              inputId = ns("species2"),
              label = "Select species 2:",
              choices = c("None", speciesl),
              selected = "None"
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
      # Use two separate species inputs
      selected <- c(input$species1, input$species2)
      selected <- selected[selected != "None"]
      if (length(selected) == 0) return(ldat[0,])
      selected_columns <- paste(selected, "(count)", sep = " ")
      filtered <- ldat %>%
        filter(rowSums(!is.na(select(., all_of(selected_columns)))) > 0)
      filtered
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
        choices = c("All", as.character(sort(unique(spdat()$`Site (from Site)`)))),
        selected = "All"
      )
    })

    spyeardat <- reactive({
      spsitedat() %>%
        filter(format(Date, "%Y") >= input$yearRange[1] & format(Date, "%Y") <= input$yearRange[2])
    })

    spmap <- reactive({
      selected <- c(input$species1, input$species2)
      selected <- selected[selected != "None"]
      map <- leaflet() %>% addProviderTiles("Esri.WorldImagery")
      colors <- c("#ff9b9b", "#a5d4f5")
      both_color <- "#dfc5fe"
      site_rows <- sdat[sdat$Site %in% unique(as.character(spsitedat()$`Site (from Site)`)), ]

      # If two species are selected, find sites where both exist
      if (length(selected) == 2) {
        col1 <- paste(selected[1], "(count)", sep = " ")
        col2 <- paste(selected[2], "(count)", sep = " ")
        both_sites <- spsitedat() %>%
          filter(!is.na(.data[[col1]]) & .data[[col1]] > 0 & !is.na(.data[[col2]]) & .data[[col2]] > 0) %>%
          pull(`Site (from Site)`) %>% unique()
        both_site_rows <- site_rows[site_rows$Site %in% both_sites, ]
        # Add markers for sites with both species
        if (nrow(both_site_rows) > 0) {
          map <- map %>% addCircleMarkers(
            data = both_site_rows,
            lng = ~`Longitude Top`,
            lat = ~`Latitude Top`,
            color = both_color,
            fillColor = both_color,
            radius = 10,
            popup = ~paste("Site: ", Site, "<br/>Species: Both", "<br/>Stream: ", Stream, "<br/>"),
            opacity = 0.8,
            fillOpacity = 0.8,
            group = "Both"
          )
        }
      }
      # Add markers for each species
      for (i in seq_along(selected)) {
        col <- paste(selected[i], "(count)", sep = " ")
        species_sites <- spsitedat() %>%
          filter(!is.na(.data[[col]]) & .data[[col]] > 0) %>%
          pull(`Site (from Site)`) %>% unique()
        # Exclude sites already marked as both
        if (length(selected) == 2) {
          species_sites <- setdiff(species_sites, if (exists("both_sites")) both_sites else character(0))
        }
        species_site_rows <- site_rows[site_rows$Site %in% species_sites, ]
        if (nrow(species_site_rows) > 0) {
          map <- map %>% addCircleMarkers(
            data = species_site_rows,
            lng = ~`Longitude Top`,
            lat = ~`Latitude Top`,
            color = colors[i],
            fillColor = colors[i],
            radius = 10,
            popup = ~paste("Site: ", Site, "<br/>Species: ", selected[i], "<br/>Stream: ", Stream, "<br/>"),
            opacity = 0.8,
            fillOpacity = 0.8,
            group = selected[i]
          )
        }
      }
      # Legend
      legend_colors <- colors[1:length(selected)]
      legend_labels <- selected
      if (length(selected) == 2) {
        legend_colors <- c(legend_colors, both_color)
        legend_labels <- c(legend_labels, "Both")
      }
      map %>% addLegend(position = "bottomright", colors = legend_colors, labels = legend_labels, title = "Species", opacity = 1)
    })

    output$speciesmap <- renderLeaflet({
      spmap()
    })

    output$speciesplot <- renderPlotly({
      selected <- c(input$species1, input$species2)
      selected <- selected[selected != "None"]
      dat <- spyeardat()
      # Remove/rename any existing 'Species' column before pivot_longer
      if ("Species" %in% colnames(dat)) {
        dat <- dat %>% rename(Species_existing = Species)
      }
      dat_long <- tidyr::pivot_longer(dat, cols = tidyselect::matches("\\(count\\)"), names_to = "Species", values_to = "Count")
      dat_long <- dat_long %>% filter(Species %in% paste(selected, "(count)", sep = " "))
      dat_long$Species <- gsub(" \\(.+\\)", "", dat_long$Species)
      ggplotly(ggplot(dat_long, aes(x = Date, y = Count, color = Species)) +
        geom_point() +
        geom_smooth(aes(group = Species), color = "black") +
        scale_color_manual(values = c("#ff9b9b", "#a5d4f5")) +
        labs(title = paste("Number of", paste(selected, collapse = " & "), "collected through time"),
             x = "Date",
             y = "Count",
             color = "Species") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    spyeardat2 <- reactive({
      dat <- spyeardat()
      selected <- c(input$species1, input$species2)
      selected <- selected[selected != "None"]
      if ("Species" %in% colnames(dat)) {
        dat <- dat %>% rename(Species_existing = Species)
      }
      dat_long <- tidyr::pivot_longer(dat, cols = tidyselect::matches("\\(count\\)"), names_to = "Species", values_to = "Count")
      dat_long <- dat_long %>% filter(Species %in% paste(selected, "(count)", sep = " "))
      dat_long$Species <- gsub(" \\(.+\\)", "", dat_long$Species)
      dat_long %>%
        mutate(Year = format(Date, "%Y")) %>%
        group_by(Year, `Stream (from Site)`, Species) %>%
        summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")
    })

    output$speciesbarchart <- renderPlotly({
      dat <- spyeardat2()
      ggplotly(ggplot(dat, aes(x = Year, y = Count, fill = Species)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("#ff9b9b", "#a5d4f5")) +
        labs(title = "Number collected by year",
             x = "Year",
             y = "Count",
             fill = "Species") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    spyeardat3 <- reactive({
      dat <- spyeardat()
      selected <- c(input$species1, input$species2)
      selected <- selected[selected != "None"]
      if ("Species" %in% colnames(dat)) {
        dat <- dat %>% rename(Species_existing = Species)
      }
      dat_long <- tidyr::pivot_longer(dat, cols = tidyselect::matches("\\(count\\)"), names_to = "Species", values_to = "Count")
      dat_long <- dat_long %>% filter(Species %in% paste(selected, "(count)", sep = " "))
      dat_long$Species <- gsub(" \\(.+\\)", "", dat_long$Species)
      dat_long %>%
        mutate(Year = format(Date, "%Y")) %>%
        group_by(Year, `Stream (from Site)`, Species) %>%
        summarise(Count = mean(Count, na.rm = TRUE), .groups = "drop")
    })

    output$speciesbarchart2 <- renderPlotly({
      dat <- spyeardat3()
      ggplotly(ggplot(dat, aes(x = Year, y = round(Count, digits = 0), fill = Species)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("#ff9b9b", "#a5d4f5")) +
        labs(title = "Average number collected per survey",
             x = "Year",
             y = "Count",
             fill = "Species") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    dcsdat <- reactive({
      dat <- spyeardat()
      selected <- c(input$species1, input$species2)
      selected <- selected[selected != "None"]
      if ("Species" %in% colnames(dat)) {
        dat <- dat %>% rename(Species_existing = Species)
      }
      dat_long <- tidyr::pivot_longer(dat, cols = tidyselect::matches("\\(count\\)"), names_to = "Species", values_to = "Count")
      dat_long <- dat_long %>% filter(Species %in% paste(selected, "(count)", sep = " "))
      dat_long$Species <- gsub(" \\(.+\\)", "", dat_long$Species)
      # Clean up Organization field so entries like
      # c("Org A", "Org B") become a readable HTML string with line breaks
      org_raw <- as.character(dat_long$`Organization (from Organization)`)
      format_org <- vapply(org_raw, FUN.VALUE = character(1), USE.NAMES = FALSE, FUN = function(x) {
        if (is.na(x) || x == "") return(NA_character_)
        # If string looks like an R vector: c("A", "B"), extract quoted items
        if (grepl('^\\s*c\\s*\\(', x)) {
          # find all quoted pieces
          matches <- regmatches(x, gregexpr('"([^"\\\\]*(?:\\\\.[^"\\\\]*)*)"', x, perl = TRUE))
          if (length(matches) && length(matches[[1]]) > 0) {
            # remove surrounding quotes and join with HTML line breaks
            clean <- gsub('^"|"$', '', matches[[1]])
            return(paste(clean, collapse = '<br/>'))
          }
          # fallback: remove c( ) and any quotes, then replace commas with breaks
          tmp <- gsub('^\\s*c\\s*\\(|\\)\\s*$', '', x)
          tmp <- gsub('"', '', tmp)
          tmp <- gsub('\\s*,\\s*', '<br/>', tmp)
          tmp <- gsub('^\\s+|\\s+$', '', tmp)
          return(tmp)
        }
        # otherwise leave as-is
        x
      })

      yuh <- data.frame(
        Date = dat_long$Date,
        Species = dat_long$Species,
        Count = dat_long$Count,
        Organization = as.character(format_org),
        stringsAsFactors = FALSE
      )
      yuh <- yuh[order(yuh$Date, decreasing = TRUE), ]
      row.names(yuh) <- seq_len(nrow(yuh))
      yuh
    })

    output$data <- DT::renderDataTable({
      # allow HTML in the Organization column (we insert <br/> for multi-org rows)
      DT::datatable(dcsdat(), escape = FALSE, options = list(pageLength = 25))
    })
  })
}