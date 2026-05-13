# Organization Analysis Tab Module
mod_organa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = tags$div(style = "font-size: 24px; font-weight: 600;",
                         "Organization Analysis"),
        fluidRow(
          column(
            width = 6,
            # Safe default; server populates from odat
            selectInput(
              inputId  = ns("organ_cat"),
              label    = tags$div(style = "font-size: 16px;",
                                  "Select an organization category:"),
              choices  = c("All"),
              selected = "All"
            )
          ),
          column(
            width = 6,
            uiOutput(ns("organ_a"))
          )
        )
      ),
      box(
        title = tags$div(style = "font-size: 24px; font-weight: 600;",
                         "Organization Summary"),
        width = 12,
        shinycssloaders::withSpinner(
          valueBoxOutput(ns("org_invasive_sum"), width = 3), type = 6),
        shinycssloaders::withSpinner(
          valueBoxOutput(ns("org_native_sum"), width = 3), type = 6),
        shinycssloaders::withSpinner(
          valueBoxOutput(ns("org_biomass_sum"), width = 3), type = 6),
        shinycssloaders::withSpinner(
          valueBoxOutput(ns("org_visits_sum"), width = 3), type = 6)
      ),
      box(width = 12,
          shinycssloaders::withSpinner(
            plotlyOutput(ns("org_native")), type = 6)),
      box(width = 12,
          shinycssloaders::withSpinner(
            plotlyOutput(ns("org_nonnative")), type = 6)),
      box(width = 12,
          shinycssloaders::withSpinner(
            plotlyOutput(ns("org_biomass")), type = 6))
    )
  )
}

mod_organa_server <- function(id, ldat, odat, pwpalette) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate category choices from odat
    if (!is.null(odat) && "Organization Classification" %in% colnames(odat)) {
      cats <- tryCatch(
        sort(unique(as.character(odat$`Organization Classification`))),
        error = function(e) character(0))
      if (length(cats) > 0) {
        updateSelectInput(session, "organ_cat",
                          choices  = c("All", cats),
                          selected = "All")
      }
    }

    output$organ_a <- renderUI({
      if (is.null(odat) || nrow(odat) == 0) {
        return(selectInput(ns("organ_a"),
                           tags$div(style = "font-size: 16px;",
                                    "Select an organization:"),
                           choices = "All", selected = "All"))
      }
      chosen_cat <- if (is.null(input$organ_cat)) "All" else input$organ_cat
      org_choices <- tryCatch(
        sort(as.character(unique(
          odat$Organization[odat$`Organization Classification` ==
                              chosen_cat]))),
        error = function(e) character(0))
      selectInput(
        inputId  = ns("organ_a"),
        label    = tags$div(style = "font-size: 16px;",
                            "Select an organization:"),
        choices  = if (length(org_choices) > 0) c("All", org_choices) else "All",
        selected = "All"
      )
    })

    ldat_org <- reactive({
      if (is.null(ldat) || nrow(ldat) == 0) return(ldat)
      chosen_cat <- if (is.null(input$organ_cat)) "All" else input$organ_cat
      chosen_org <- if (is.null(input$organ_a))   "All" else input$organ_a
      if (chosen_cat == "All") {
        ldat
      } else if (chosen_org == "All") {
        orgs <- tryCatch(
          as.character(odat$Organization[
            odat$`Organization Classification` == chosen_cat]),
          error = function(e) character(0))
        if (length(orgs) == 0) return(ldat[0, ])
        ldat %>% filter(`Organization (from Organization)` %in% orgs)
      } else {
        ldat %>% filter(`Organization (from Organization)` == chosen_org)
      }
    })

    org_title <- reactive({
      chosen_org <- if (is.null(input$organ_a)) "All" else input$organ_a
      chosen_cat <- if (is.null(input$organ_cat)) "All" else input$organ_cat
      if (chosen_org == "All") chosen_cat else chosen_org
    })

    output$org_invasive_sum <- renderValueBox({
      valueBox(
        value    = sum(ldat_org()$`Non-native (count)`, na.rm = TRUE),
        subtitle = "Number of invasive organisms removed",
        color    = "red",
        icon     = icon("remove-circle", lib = "glyphicon")
      )
    })

    output$org_native_sum <- renderValueBox({
      valueBox(
        value    = sum(ldat_org()$`Native (count)`, na.rm = TRUE),
        subtitle = "Number of native organisms observed",
        color    = "green",
        icon     = icon("ok-circle", lib = "glyphicon")
      )
    })

    output$org_biomass_sum <- renderValueBox({
      valueBox(
        value    = round(sum(ldat_org()$Biomass, na.rm = TRUE), digits = 4),
        subtitle = "Mass of invasives removed (lbs)",
        color    = "purple",
        icon     = icon("scale", lib = "glyphicon")
      )
    })

    output$org_visits_sum <- renderValueBox({
      valueBox(
        value    = nrow(ldat_org()),
        subtitle = "Number of field surveys",
        color    = "orange",
        icon     = icon("eye-open", lib = "glyphicon")
      )
    })

    output$org_native <- renderPlotly({
      dat <- ldat_org()
      req(nrow(dat) > 0)
      ggplotly(
        ggplot(dat, aes(x = Date,
                        y = `Native (count)`,
                        color = as.character(`Stream (from Site)`))) +
          geom_point() +
          geom_smooth(color = "black") +
          scale_color_manual(values = pwpalette) +
          labs(
            title = paste("Native species collected through time by",
                          org_title()),
            x = "Date", y = "Count", color = "Stream") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    output$org_nonnative <- renderPlotly({
      dat <- ldat_org()
      req(nrow(dat) > 0)
      ggplotly(
        ggplot(dat, aes(x = Date,
                        y = `Non-native (count)`,
                        color = as.character(`Stream (from Site)`))) +
          geom_point() +
          geom_smooth(color = "black") +
          scale_color_manual(values = pwpalette) +
          labs(
            title = paste("Non-native species collected through time by",
                          org_title()),
            x = "Date", y = "Count", color = "Stream") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })

    output$org_biomass <- renderPlotly({
      dat <- ldat_org()
      req(nrow(dat) > 0)
      ggplotly(
        ggplot(dat, aes(x = Date, y = Biomass,
                        color = as.character(`Stream (from Site)`))) +
          geom_point() +
          geom_smooth(color = "black") +
          scale_color_manual(values = pwpalette) +
          labs(
            title = paste("Biomass collected through time by", org_title()),
            x = "Date", y = "Biomass (lbs)", color = "Stream") +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    })
  })
}
