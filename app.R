library(shiny)
library(DT)
library(leaflet)

# Define the population data
population_data <- data.frame(
  id = 1:4,  # Add a unique identifier for each row
  name = c("Population A", "Population B", "Population C", "Population D"),
  species = c("Species 1", "Species 2", "Species 3", "Species 4"),
  longitude = c(-110.95, -110.97, -110.92, -110.93),
  latitude = c(32.23, 32.24, 32.25, 32.26),
  behavior = c("Behavior 1", "Behavior 2", "Behavior 3", "Behavior 4"),
  location_name = c("Location A", "Location B", "Location C", "Location D")
)

# UI
ui <- 
  navbarPage(
    theme = bs_theme(bootswatch = "minty"),
    title = "The ACDB",
    
    # Landing page
    tabPanel(
      "Home",
      fluidPage(
        titlePanel("Welcome to the Animal Culture Database"),
        fluidRow(
          column(6, h3("Text Placeholder"), p("This is placeholder text for the landing page."))
        )
      )
    ),
    
    # Landing page
    tabPanel(
      "Populations",
      fluidPage(
        titlePanel("Population-level data"),
        leafletOutput("population_map", height = 400),
        
        dataTableOutput("population_table"),
        uiOutput("modal_content")  # Placeholder for modal content
      )
    )
  )



# Server Logic
server <- function(input, output, session) {
  
  # Render leaflet map
  output$population_map <- renderLeaflet({
    leaflet(data = population_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        popup = ~paste("<b>Name:</b>", name, "<br>",
                       "<b>Species:</b>", species, "<br>",
                       "<b>Behavior:</b>", behavior)
      )
  })
  
  # Render the main table
  output$population_table <- renderDataTable({
    datatable(
      population_data[, c("name", "species", "longitude", "latitude", "behavior", "location_name")],
      selection = "single",
      options = list(pageLength = 5)
    )
  })
  
  # Observe row selection to trigger the modal
  observeEvent(input$population_table_rows_selected, {
    selected_row <- population_data[input$population_table_rows_selected, ]
    
    # Show the modal
    showModal(modalDialog(
      title = paste("Details for", selected_row$name),
      
      # Table of behavior details
      h3("Behavior Details"),
      tableOutput("details_table"),
      
      # Map showing the location of the species
      h3("Location on Map"),
      leafletOutput("details_map", height = 400),
      
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    # Render the behavior details table
    output$details_table <- renderTable({
      selected_row[, c("behavior", "location_name")]
    })
    
    # Render the map with species location
    output$details_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addMarkers(
          lng = selected_row$longitude,
          lat = selected_row$latitude,
          popup = paste0(
            "<b>", selected_row$name, "</b><br>",
            "Species: ", selected_row$species, "<br>",
            "Location: ", selected_row$location_name
          )
        ) %>%
        setView(lng = selected_row$longitude, lat = selected_row$latitude, zoom = 12)
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
