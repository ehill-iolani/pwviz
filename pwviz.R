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

####################
### Pulling data ###
####################
# Read in the API key, basename, and table name from the .Renviron file
readRenviron("~")

# Source the helper functions
source("get_airtable_records.R")

# Verifies the all the necessary environment variables are set
if (Sys.getenv("AIRTABLE_API_KEY") == "")
  print("API key not found") else print("API key found")

if (Sys.getenv("AIRTABLE_BASE_NAME") == "")
  print("Base name not found") else print("Base name found")

if (Sys.getenv("AIRTABLE_L3DB_NAME") == "")
  print("Lesson 3 table name not found") else print("Lesson 3 table name found")

if (Sys.getenv("AIRTABLE_SITEDB_NAME") == "")
  print("Site table name not found") else print("Site table name found")

if (Sys.getenv("AIRTABLE_ORGANIZATIONDB_NAME") == "")
  print("Organization table name not found") else print("Organization table name found")

# Pulls data from Airtable
key <- Sys.getenv("AIRTABLE_API_KEY")
base <- Sys.getenv("AIRTABLE_BASE_NAME")
l3_table_name <- Sys.getenv("AIRTABLE_L3DB_NAME")
site_table_name <- Sys.getenv("AIRTABLE_SITEDB_NAME")
organziation_table_name <- Sys.getenv("AIRTABLE_ORGANIZATIONDB_NAME")
key <- Sys.getenv("AIRTABLE_API_KEY")
record_id <- NULL

ldat <- get_airtable_records(base, l3_table_name, key, record_id)
sdat <- get_airtable_records(base, site_table_name, key, record_id)
odat <- get_airtable_records(base, organziation_table_name, key, record_id)

#####################
### DATA CLEANING ###
#####################
# Clean the imported site data
sdat <- sdat[, c(2, 3, 5, 7, 8)]
sdat[1, 2] <- "Ala Wai Canal"
sdat$`Longitude Bottom` <- as.numeric(sdat$`Latitude Top`)
sdat$`Longitude Top` <- as.numeric(sdat$`Longitude Top`)
sdat <- sdat[!(sdat$Stream %in% c("Ala Wai Canal", "Pauoa", "Nuuanu")), ]
pwpalette <- c("Makiki" = "blue", "Manoa" = "green", "Manoa-Palolo" = "orange", "Palolo" = "yellow")
color_palette <- colorFactor(palette = pwpalette, domain = sdat$Stream)

# Restrict survey data to only paepae
ldat <- ldat %>%
  filter(ldat$`Survey type` == "Paepae")

# Format the date column in ldat
ldat$Date <- as.Date(ldat$Date)

# Remove surveys from Ala Wai Canal, Pauoa, and Nuuanu
ldat <- ldat[!(ldat$`Stream (from Site)` %in% c("Ala Wai Canal", "Pauoa", "Nuuanu")), ]

# Retrive species names from column names
species <- colnames(ldat)[grep("(count)", colnames(ldat))]
speciesl <- gsub(" \\(count\\)", "", species)
speciesl <- speciesl[speciesl != "Native" & speciesl != "Non-native" & speciesl != "Total"]
speciesl <- sort(speciesl)

# Remove organizations with no abbreviation
odat <- odat[is.na(odat$Abbreviation) == FALSE, ]

#################
### APP START ###
#################

# Define UI for application, homepage is map of sites
ui <- dashboardPage(
  dashboardHeader(title = "Paepae O Waikolu Stream Survey Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Paepae O Waikolu at a glance", tabName = "summary"),
      menuItem("Species analysis", tabName = "speciesa"),
      menuItem("Site analysis", tabName = "sitea"),
      menuItem("Organization analysis", tabName = "organa")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
        fluidRow(
          box(
            title = "Stream + Site Map",
            width = 12,
            fluidRow(
              column(
                width = 9,
                leafletOutput("map")
              ),
              column(
                width = 3,
                radioButtons("stream", "Select a stream:", c("All", unique(sdat$Stream)), selected = "All")
              )
            )
          ),
          box(
            title = "Overview Based on Stream Selection",
            width = 12,
            fluidRow(
              valueBoxOutput("invasive", width = 3),
              valueBoxOutput("native", width = 3),
              div(id = "hsibi_help",
                valueBoxOutput("hsibi", width = 3)
              ),
              bsTooltip("hsibi", "Click me to learn more!", placement = "top"),
              bsModal("hsibi_help_modal", "What is HSIBI?", "hsibi_help", size = "large",
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
              valueBoxOutput("biomass", width = 3)
            ),
            fluidRow(
              div(id = "visits_help",
                valueBoxOutput("visits", width = 4)
              ),
              bsTooltip("visits", "Click me to learn more!", placement = "top"),
              bsModal("visits_help_modal", "What is a Paepae survey?", "visits_help", size = "large",
                p(HTML("<b>A visit is a field survey conducted using the Paepae method.</b><br><br>
                  Paepae is a fish population survey method that utilizes sound and vibration 
                  to herd animals downstream to be collected and counted by a the survey team."))
              ),
              valueBoxOutput("drange", width = 8)
            )
          )
        )
      ),
      tabItem(tabName = "speciesa",
        fluidRow(
          box(
            width = 12,
            title = "Select a species and site",
            fluidRow(
              column(
                width = 9,
                leafletOutput("speciesmap")
              ),
              column(
                width = 3,
                selectInput(
                  inputId = "specieslist",
                  label = "Select a species:",
                  choices = speciesl
                )
              ),
              column(
                width = 3,
                uiOutput("site_ui")
              )
            )
          ),
          box(
            width = 12,
            plotlyOutput("speciesplot"),
            sliderInput("yearRange", "Select Year Range:",
              min = as.numeric(format(min(ldat$Date), "%Y")),
              max = as.numeric(format(max(ldat$Date), "%Y")),
              value = c(as.numeric(format(min(ldat$Date), "%Y")), as.numeric(format(max(ldat$Date), "%Y"))),
              step = 1,
              sep = ""
            )
          ),
          box(
            width = 12,
            plotlyOutput("speciesbarchart")
          ),
          box(
            title = "Counts by Date and Organization",
            width = 12,
            dataTableOutput("data")
          )
        )
      ),
      tabItem(tabName = "sitea",
        fluidRow(
          box(
            width = 12,
            title = "Site Analysis",
            fluidRow(
              column(
                width = 9,
                title = "Site Map",
                leafletOutput("site_map")
              ),
              column(
                width = 3,
                title = "Select a stream",
                selectInput(
                  inputId = "stream_a",
                  label = "Select a stream:",
                  choices = c("All", sort(as.character(unique(sdat$Stream)))),
                  selected = "All"
                )
              ),
              column(
                width = 3,
                title = "Select a site",
                uiOutput("site_a")
              )
            )
          ),
          box(
            width = 12,
            plotlyOutput("hisibi_plot")
          ),
          box(
            width = 12,
            plotlyOutput("native_plot")
          ),
          box(
            width = 12,
            plotlyOutput("nonnative_plot")
          )
        )
      ),
      tabItem(tabName = "organa",
        fluidRow(
          box(
            width = 12,
            title = "Organization Analysis",
            fluidRow(
              column(
                width = 6,
                title = "Select an Organization",
                selectInput(
                  inputId = "organ_cat",
                  label = "Select an organization catagory:",
                  choices = c("All", sort(as.character(unique(odat$`Organization Classification`)))),
                  selected = "All"
                )
              ),
              column(
                width = 6,
                title = "Select a specific organization",
                uiOutput("organ_a")
              )
            )
          ),
          box(
            title = "Organization Summary",
            width = 12,
            valueBoxOutput("org_invasive_sum", width = 3),
            valueBoxOutput("org_native_sum", width = 3),
            valueBoxOutput("org_biomass_sum", width = 3),
            valueBoxOutput("org_visits_sum", width = 3)
          ),
          box(
            width = 12,
            plotlyOutput("org_native")
          ),
          box(
            width = 12,
            plotlyOutput("org_nonnative")
          ),
          box(
            width = 12,
            plotlyOutput("org_biomass")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  ###################
  ### Summary tab ###
  ###################
  # Render the map
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

  # Calculate summary stats for the selected site
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
          biomass = round(mean(Biomass, na.rm = TRUE), digits = 4),
          visits = nrow(ldat[ldat$`Stream (from Site)` == input$stream, ]),
          drange = paste(range(`Date`, na.rm = TRUE)[1], range(`Date`, na.rm = TRUE)[2], sep = " to ")
        )
    }
  })

  # Render the value boxes for the summary stats
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

  ####################
  ### Species page ###
  ####################
  # Save the filtered rows into spdat
  spdat <- reactive({
    selected_columns <- ldat %>%
      select(contains(paste(input$specieslist, "(count)", sep = " ")))

    filtered_rows <- ldat %>%
      filter(rowSums(!is.na(selected_columns)) > 0)

    filtered_rows <- as.data.frame(filtered_rows)

    return(filtered_rows)
  })

  # Modify spdat based on site selection
  spsitedat <- reactive({
    if (input$site == "All") {
      spdat()
    } else {
      spdat() %>%
        filter(`Site (from Site)` == input$site)
    }
  })

  # Render the site selection dropdown
  output$site_ui <- renderUI({
    selectInput(
      inputId = "site",
      label = "Select a site:",
      choices = c("All", sort(as.character(unique(spdat()$`Site (from Site)`)))),
      selected = "All"
    )
  })

  # Modify spdat based on year range
  spyeardat <- reactive({
    spsitedat() %>%
      filter(format(Date, "%Y") >= input$yearRange[1] & format(Date, "%Y") <= input$yearRange[2])
  })

  # Create map where the species are found
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

  # Render the map where the species are found
  output$speciesmap <- renderLeaflet({
    spmap()
  })

  # Render plot to show number collected through time
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

  # Aggregate spyeardat by year
  spyeardat2 <- reactive({
    spyeardat() %>%
      mutate(Year = format(Date, "%Y")) %>%
      group_by(Year) %>%
      summarise(Count = sum(get(paste(input$specieslist, "(count)", sep = " "))))
  })

  # Render barchart of counts by year
  output$speciesbarchart <- renderPlotly({
    ggplotly(ggplot(spyeardat2(), aes(x = Year, y = Count)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Number of", input$specieslist, "collected by year"),
             x = "Year",
             y = paste(input$specieslist, "count", sep = " ")) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })

  # Create table of counts by date and organization
  dcsdat <- reactive({
    temp <- spyeardat()[grep(input$specieslist, colnames(spyeardat()))]
    temp <- temp[, grep("count", colnames(temp))]
    yuh <- data.frame(
            Date = spyeardat()$Date,
            Count = temp,
            Organization = as.character(spyeardat()$`Organization (from Organization)`))
    yuh <- yuh[order(yuh$Date), ]
    row.names(yuh) <- 1:nrow(yuh)
    return(yuh)
  })

  # PLACEHOLDER Render the data table
  output$data <- DT::renderDataTable({
    DT::datatable(dcsdat())
  })

  #################
  ### Site page ###
  #################
  # Create site selection based on stream selection
  output$site_a <- renderUI({
    selectInput(
      inputId = "site_a",
      label = "Select a site:",
      choices = c("All", sort(as.character(unique(sdat$Site[sdat$Stream == input$stream_a])))),
      selected = "All"
    )
  })

  # Filter the site data based on stream selection
  filtered_sdat <- reactive({
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

  # Create site map
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

  # Render the site map
  output$site_map <- renderLeaflet({
    site_map()
  })

  # Filter ldat based on site selection
  ldat_site <- reactive({
    if (input$stream_a == "All") {
      ldat
    } else if (input$site_a == "All") {
      ldat %>%
        filter(`Stream (from Site)` %in% sdat$Stream[sdat$Stream == input$stream_a])
    } else {
      ldat %>%
        filter(`Site (from Site)` == input$site_a, `Stream (from Site)` == input$stream_a)
    }
  })

  # Plot the HISIBI values for the selected site through time
  output$hisibi_plot <- renderPlotly({
    ggplotly(ggplot(ldat_site(), aes(x = Date, y = HSIBI, color = as.character(`Stream (from Site)`))) +
        geom_point() +
        geom_smooth(aes(group = 1), color = "black") +
        scale_color_manual(values = pwpalette) +
        labs(title = if (input$site_a == "All") {paste("HSIBI values through time of", input$stream_a, "stream(s)", sep = " ")}
          else {paste("HSIBI values through time of", input$site_a, sep = " ")},
             x = "Date",
             y = "HSIBI",
             color = "Stream") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })

  # Plot trends of native species through time
  output$native_plot <- renderPlotly({
    ggplotly(ggplot(ldat_site(), aes(x = Date, y = `Native (count)`)) +
        geom_point(color = "cyan") +
        geom_smooth(color = "black") +
        scale_color_manual(values = pwpalette) +
        labs(title = if (input$site_a == "All") {paste("Number of native species collected through time in", input$stream_a, "stream(s)", sep = " ")}
          else {paste("Number of native species collected through time at", input$site_a, sep = " ")},
             x = "Date",
             y = "Count",
             color = "Stream") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })

  # Plot trends of non-native species through time
  output$nonnative_plot <- renderPlotly({
    ggplotly(ggplot(ldat_site(), aes(x = Date, y = `Non-native (count)`)) +
        geom_point(color = "red") +
        geom_smooth(color = "black") +
        scale_color_manual(values = pwpalette) +
        labs(title = if (input$site_a == "All") {paste("Number of non-native species collected through time in", input$stream_a, "stream(s)", sep = " ")}
          else {paste("Number of non-native species collected through time at", input$site_a, sep = " ")},
             x = "Date",
             y = "Count",
             color = "Stream") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })

  #########################
  ### Organization page ###
  #########################
  # Create organization selection based on organization category selection
  output$organ_a <- renderUI({
    selectInput(
      inputId = "organ_a",
      label = "Select an organization:",
      choices = c("All", sort(as.character(unique(odat$Organization[odat$`Organization Classification` == input$organ_cat])))),
      selected = "All"
    )
  })

  # Filter ldat based on organization selection
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

  # outputBox for the number of invasive species
  output$org_invasive_sum <- renderValueBox({
    valueBox(
      value = sum(ldat_org()$`Non-native (count)`),
      subtitle = "Number of invasive organisms removed",
      color = "red",
      icon = icon("remove-circle", lib = "glyphicon")
    )
  })

  # outputBox for the number of native species
  output$org_native_sum <- renderValueBox({
    valueBox(
      value = sum(ldat_org()$`Native (count)`),
      subtitle = "Number of native organisms observed",
      color = "green",
      icon = icon("ok-circle", lib = "glyphicon")
    )
  })

  # outputBox for the biomass
  output$org_biomass_sum <- renderValueBox({
    valueBox(
      value = round(sum(ldat_org()$Biomass, na.rm = TRUE), digits = 4),
      subtitle = "Mass of invasives removed (lbs)",
      color = "purple",
      icon = icon("scale", lib = "glyphicon")
    )
  })

  # outputBox for the number of visits
  output$org_visits_sum <- renderValueBox({
    valueBox(
      value = nrow(ldat_org()),
      subtitle = "Number of field surveys",
      color = "orange",
      icon = icon("eye-open", lib = "glyphicon")
    )
  })

  # Scatterplot to the number of native and non-native species
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

  # Scatterplot to the number of non-native species
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

  # Scatterplot of biomass through time
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

}

# Run the application
shinyApp(ui = ui, server = server)
