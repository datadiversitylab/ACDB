###################
# server.R
# 
# Server controller. 
# Used to define the back-end aspects of the app.
###################

library(RSQLite)


server <- function(input, output, session) {
  # Connect to SQLite database
  db_path <- "db/ACDB_dev.sql"
  conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  
  # Read tables from the SQLite database
  groups_table <- dbReadTable(conn, "groups")
  species_table <- dbReadTable(conn, "species")
  behaviors_table <- dbReadTable(conn, "behaviors")
  sources_table <- dbReadTable(conn, "sources")
  
  # Ensures that the connection is closed when the session ends
  on.exit(dbDisconnect(conn))
  
  # Compute stats for the UI
  species_count <- length(unique(species_table$species_id))
  behavior_count <- nrow(behaviors_table)
  population_count <- nrow(groups_table)
  
  # Send stats to the UI
  output$species_count <- renderText({ paste0(species_count, "+") })
  output$behavior_count <- renderText({ paste0(behavior_count, "+") })
  output$population_count <- renderText({ paste0(population_count, "+") })
  
  # Navigate to Populations tab when button in landing page is clicked
  observeEvent(input$explore_button, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Populations_tab")
  })
  
  # Render leaflet map
  # Calculate the number of recorded behaviors per group
  behavior_counts <- aggregate(behavior_id ~ group_id, data = behaviors_table, FUN = length)
  colnames(behavior_counts)[2] <- "behavior_count"  # Rename column for clarity
  
  # Join groups_table with species_table to get species and common names
  groups_with_species <- merge(groups_table, species_table, by.x = "species_id", by.y = "species_id", all.x = TRUE)
  
  # Join the behavior counts with the group data
  groups_with_species <- merge(groups_with_species, behavior_counts, by.x = "group_id", by.y = "group_id", all.x = TRUE)
  
  # Replace NA behavior counts with 0 (for groups with no behaviors)
  groups_with_species$behavior_count[is.na(groups_with_species$behavior_count)] <- 0
  
  # Render leaflet map
  output$population_map <- renderLeaflet({
    leaflet(data = groups_with_details) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%  # Light-themed tiles
      addCircleMarkers(
        lng = ~long, lat = ~lat,
        radius = 6,  # Marker size
        color = "#4CAF50",  # Outline color
        fillColor = "#2E7D32",  # Fill color
        fillOpacity = 0.8,  # Transparency
        weight = 2,  # Border weight
        label = ~paste0(group_name, " (", behavior_count, " behaviors)"),  # Quick hover label
        popup = ~paste0(
          "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.6; color: #333;'>",
          "<b style='font-size: 16px; color: #4CAF50;'>", group_name, "</b><br>",
          "<b>Species:</b> ", canonicalName, "<br>",
          "<b>Common Name:</b> ", common_name, "<br>",
          "<b>Recorded Behaviors:</b> ", behavior_count, "<br>",
          "<b>Location Evidence:</b> ", location_evidence, "<br>",
          "<a href='#' style='color: #2E7D32; text-decoration: underline;' ",
          "onclick='Shiny.setInputValue(\"map_click\", \"", group_id, "\", {priority: \"event\"}); return false;'>More...</a>",
          "</div>"
        )
        , clusterOptions = markerClusterOptions()
      )
  })
  
  #Include counts per group in the table
  # Join groups_table with species_table to get species and common names
  groups_with_details <- merge(groups_table, species_table, by.x = "species_id", by.y = "species_id", all.x = TRUE)
  
  # Join the behavior counts with the group data
  groups_with_details <- merge(groups_with_details, behavior_counts, by.x = "group_id", by.y = "group_id", all.x = TRUE)
  
  # Replace NA behavior counts with 0 (for groups with no behaviors)
  groups_with_details$behavior_count[is.na(groups_with_details$behavior_count)] <- 0
  
  # Render the main population table
  output$groups_table <- renderDataTable({
    groups_with_details$common_name <- tools::toTitleCase(groups_with_details$common_name)
    datatable(
      groups_with_details[, c("group_name", "canonicalName", "common_name", "behavior_count", "lat", "long", "location_evidence")],
      colnames = c("Group Name", "Species Name", "Common Name", "Number of Behaviors", "Latitude", "Longitude", "Location Evidence"),
      selection = "single",
      options = list(pageLength = 10),
      extensions = 'Buttons',
      rownames = FALSE
    )%>%
      formatStyle(
        "canonicalName",
        target = "cell",
        fontStyle = "italic"
      )
  })
  
  # Function to display modal based on selected group_id
  showGroupModal <- function(selected_id) {
    # Filter relevant data
    species_id <- groups_table$species_id[groups_table$group_id == selected_id]
    selected_species <- species_table[species_table$species_id == species_id, ]
    selected_behaviors <- behaviors_table[behaviors_table$group_id == selected_id, ]
    selected_references <- sources_table[sources_table$behavior_id %in% selected_behaviors$behavior_id, ]
    selected_group <- groups_table[groups_table$group_id == selected_id, ]
    
    # Show modal with tabs
    showModal(modalDialog(
      size = "xl",
      title = div(style = "font-size: 24px; font-weight: bold; color: #4CAF50; text-align: center;",
                  paste("Details for", selected_group$group_name)),
      div(
        style = "background-color: #f9f9f9; padding: 20px;",
        tabsetPanel(
          id = "modal_tabs",
          type = "tabs",
          tabPanel("General information", uiOutput("species_details_text")),
          tabPanel("Behavior details", uiOutput("behavior_details_text")),
          tabPanel("Location map", leafletOutput("modal_map", height = 400))
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    # Helper function for citation
    getCitation <- function(source_id) {
      source_row <- sources_table[sources_table$source_id == source_id, ]
      
      if (nrow(source_row) > 0) {
        # Split authors by comma
        author_list <- strsplit(source_row$authors, ",")[[1]]
        
        # Apply et al. if more than 3 authors
        if (length(author_list) > 3) {
          authors <- paste0(trimws(author_list[1]), " et al.")
        } else {
          authors <- paste(author_list, collapse = ", ")
        }
        
        # Create author-year format
        author_year <- paste0(authors, " (", source_row$year, ")")
        
        # Return as a hyperlink if DOI exists
        if (!is.na(source_row$doi)) {
          return(tags$a(href = paste0("https://doi.org/", source_row$doi),
                        target = "_blank", author_year))
        } else {
          return(author_year)
        }
      } else {
        return(NULL)
      }
    }
    
    
    # Render species details
    output$species_details_text <- renderUI({
      tagList(
        tags$div(
          style = "margin-bottom: 20px; padding: 10px; border: 1px solid #ccc; border-radius: 5px; background-color: #ffffff;",
          
          # Taxonomy Section
          tags$h4("Taxonomy"),
          lapply(seq_len(nrow(selected_species)), function(i) {
            species <- selected_species[i, ]
            tagList(
              tags$p(tags$b("Common name: "), 
                     tools::toTitleCase(as.character(species$common_name))),
              tags$p(tags$b("Species: "), 
                     tags$i(as.character(species$canonicalName))),
              tags$p(tags$b("Genus: "), 
                     tags$i(as.character(species$Genus))),
              tags$p(tags$b("Family: "), 
                     as.character(species$Family)),
              tags$p(tags$b("Order: "), 
                     as.character(species$Ordr)),
              tags$p(tags$b("Class: "), 
                     as.character(species$Class)),
              tags$p(tags$b("Phylum: "), 
                     as.character(species$Phylum)),
              tags$p(tags$b("GBIF: "), 
                     tags$a(href = paste0("https://www.gbif.org/species/", 
                                          as.character(species$GBIF)), 
                            as.character(species$GBIF), target = "_blank"))
            )
          }),
          
          tags$hr(),  # Horizontal line separator
          
          # Data sources section
          tags$h4("Data sources"),
          lapply(seq_len(nrow(selected_species)), function(i) {
            species <- selected_species[i, ]
            tagList(
              tags$p(tags$b("Primary social unit: "), 
                     as.character(species$primary_social_unit)),
              tags$p(tags$b("Unit evidence: "), 
                     as.character(species$unit_evidence)),
              tags$p(tags$b("Unit source: "), 
                     getCitation(species$unit_source))
            )
          }),
          
          tags$hr(),  # Horizontal line separator
          
          # Conservation section
          tags$h4("Conservation"),
          lapply(seq_len(nrow(selected_species)), function(i) {
            species <- selected_species[i, ]
            
            # IUCN status mapping
            iucn_mapping <- list(
              "LC" = "Least Concern (LC)",
              "NT" = "Near Threatened (NT)",
              "VU" = "Vulnerable (VU)",
              "EN" = "Endangered (EN)",
              "CR" = "Critically Endangered (CR)",
              "EW" = "Extinct in the Wild (EW)",
              "EX" = "Extinct (EX)"
            )
            iucn_status <- iucn_mapping[[as.character(species$IUCN)]]
            
            tagList(
              tags$p(tags$b("IUCN Status: "), 
                     ifelse(!is.null(iucn_status), iucn_status, "Not Evaluated"))
            )
          })
        )
      )
    })
    
    
    
    # Render behavior details
    output$behavior_details_text <- renderUI({
      tagList(
        lapply(seq_len(nrow(selected_behaviors)), function(i) {
          behavior <- selected_behaviors[i, ]
          
          # Capitalize domains
          capitalizeDomains <- function(domains) {
            domains_list <- unlist(strsplit(domains, ";"))
            capitalized <- sapply(domains_list, function(d) {
              paste(toupper(substring(d, 1, 1)), substring(d, 2), sep = "")
            })
            return(paste(capitalized, collapse = "; "))
          }
          
          # Helper function to format text with first letter capitalized
          capitalizeFirstLetter <- function(text) {
            if (!is.na(text) && text != "") {
              return(paste0(toupper(substring(text, 1, 1)), substring(text, 2)))
            } else {
              return("No data available or not coded.")
            }
          }
          
          tags$div(
            style = "margin-bottom: 20px; padding: 10px; border: 1px solid #ccc; border-radius: 5px; background-color: #ffffff;",
            
            # Behavior Details
            tags$h3(style = "color: #4CAF50; text-align: center; margin-bottom: 10px;",
                    paste("Behavior:", as.character(behavior$behavior))),
            
            tags$p(tags$b("Behavior description: "), capitalizeFirstLetter(behavior$behavior_description)),
            
            # Behavior Source (in APA)
            if (!is.na(behavior$behavior_source) && behavior$behavior_source != "") {
              tags$p(tags$b("Behavior source: "), getCitation(behavior$behavior_source))
            },
            
            # Start Date and End Date
            tags$p(tags$b("Start date: "), 
                   ifelse(is.na(behavior$start_date) || behavior$start_date == "" || behavior$start_date == 0, 
                          "No data available or not coded.", as.character(behavior$start_date))),
            tags$p(tags$b("End date: "), 
                   ifelse(is.na(behavior$end_date) || behavior$end_date == "" || behavior$end_date == 0, 
                          "No data available or not coded.", as.character(behavior$end_date))),
            
            tags$hr(),
            tags$h4(style = "color: #4CAF50; margin-bottom: 10px;", "Transmission Types"),
            
            # Vertical Transmission
            if (!is.na(behavior$vertical) && behavior$vertical != "") {
              tags$p(tags$b("Vertical transmission?: "), 
                     capitalizeFirstLetter(behavior$vertical))
            },
            
            if (!is.na(behavior$vertical_evidence) && behavior$vertical_evidence != "" && behavior$vertical != "suspected unknown") {
              tags$p(tags$b("Vertical evidence: "), as.character(behavior$vertical_evidence))
            }
            ,
            if (!is.na(behavior$vertical_source) && behavior$vertical_source != "" && behavior$vertical != "suspected unknown") {
              tags$p(tags$b("Vertical source: "), getCitation(behavior$vertical_source))
            },
            
            # Horizontal Transmission
            if (!is.na(behavior$horizontal) && behavior$horizontal != "") {
              tags$p(tags$b("Horizontal transmission?: "), 
                     capitalizeFirstLetter(behavior$horizontal))
            },
            
            if (!is.na(behavior$horizontal_evidence) && behavior$horizontal_evidence != "" && behavior$horizontal != "suspected unknown") {
              tags$p(tags$b("Horizontal evidence: "), as.character(behavior$horizontal_evidence))
            }
            ,
            if (!is.na(behavior$horizontal_source) && behavior$horizontal_source != "" && behavior$horizontal != "suspected unknown") {
              tags$p(tags$b("Horizontal source: "), getCitation(behavior$horizontal_source))
            },
            
            # Oblique Transmission
            if (!is.na(behavior$oblique) && behavior$oblique != "") {
              tags$p(tags$b("Oblique transmission?: "), 
                     capitalizeFirstLetter(behavior$oblique))
            },
            
            if (!is.na(behavior$oblique_evidence) && behavior$oblique_evidence != "" && behavior$oblique != "suspected unknown") {
              tags$p(tags$b("Oblique evidence: "), as.character(behavior$oblique_evidence))
            }
            ,
            if (!is.na(behavior$oblique_source) && behavior$oblique_source != "" && behavior$oblique != "suspected unknown") {
              tags$p(tags$b("Oblique source: "), getCitation(behavior$oblique_source))
            },
            
            tags$hr(),
            tags$h4(style = "color: #4CAF50; margin-bottom: 10px;", "Domains"),
            
            # Domains and Evidence
            tags$p(tags$b("Domains: "), capitalizeDomains(as.character(behavior$domains))),
            tags$p(tags$b("Domains evidence: "), 
                   capitalizeFirstLetter(behavior$domains_evidence)),
            
            # Domains Source (APA Style)
            if (!is.na(behavior$domains_source) && behavior$domains_source != "") {
              tags$p(tags$b("Domains source: "), getCitation(behavior$domains_source))
            },
            
            tags$hr(),
            tags$h4(style = "color: #4CAF50; margin-bottom: 10px;", "Anthropogenic Effects"),
            
            # Anthropogenic Effects and Source
            tags$p(tags$b("Anthropogenic effects: "), 
                   ifelse(is.na(behavior$anth_effects) || behavior$anth_effects == "" || behavior$anth_effects == "not searched", 
                          "No data available or not coded.", as.character(behavior$anth_effects))),
            if (!is.na(behavior$anth_source) && behavior$anth_source != "" && behavior$anth_source != "not searched") {
              tags$p(tags$b("Anthropogenic effects source: "), getCitation(behavior$anth_source))
            }
          )
        })
      )
    })
    
    
    
    # Render map in modal
    output$modal_map <- renderLeaflet({
      
      # Define the bounds for the red rectangle (zoom area)
      bounds <- list(
        lng1 = selected_group$long - 1,
        lat1 = selected_group$lat - 1,
        lng2 = selected_group$long + 1,
        lat2 = selected_group$lat + 1
      )
      
      leaflet() %>%
        addTiles() %>%
        addMarkers(
          lng = selected_group$long,
          lat = selected_group$lat,
          popup = paste0(
            "<b>Group:</b> ", selected_group$group_name, "<br>",
            "<b>Location Evidence:</b> ", selected_group$location_evidence
          )
        ) %>%
        setView(lng = selected_group$long, lat = selected_group$lat, zoom = 12) %>%
        
        # Add red rectangle to indicate zoom area
        addRectangles(
          lng1 = bounds$lng1, lat1 = bounds$lat1,
          lng2 = bounds$lng2, lat2 = bounds$lat2,
          color = "red",
          fill = FALSE,
          weight = 2
        ) %>%
        
        # Add Inset MiniMap
        addMiniMap(
          tiles = providers$Esri.WorldStreetMap,
          toggleDisplay = TRUE,
          minimized = FALSE,
          position = "bottomright",
          aimingRectOptions = list(color = "red", weight = 2, fillOpacity = 0),
          zoomLevelOffset = -6
        )
    })
    
  }
  
  # Trigger modal via table row selection
  observeEvent(input$groups_table_rows_selected, {
    selected_id <- groups_table$group_id[input$groups_table_rows_selected]
    showGroupModal(selected_id)
  })
  
  # Trigger modal via map marker click
  observeEvent(input$map_click, {
    selected_id <- input$map_click
    showGroupModal(selected_id)
  })
  
  
  output$citations_table <- renderDataTable({
    req(sources_table, behaviors_table, species_table)  # Ensure all tables are loaded
    
    # Merge sources_table with behaviors_table to get evidence types
    evidence_data <- merge(sources_table, behaviors_table, 
                           by.x = "source_id", by.y = "behavior_source", 
                           all.x = TRUE)
    
    # Check for Unit Source in species_table
    unit_source_check <- species_table$unit_source
    evidence_data$unit_source <- ifelse(evidence_data$source_id %in% unit_source_check, "Unit source", "")
    
    # Check for Behavior Source in behaviors_table
    behavior_source_check <- behaviors_table$behavior_source
    evidence_data$behavior_source <- ifelse(evidence_data$source_id %in% behavior_source_check, "Behavior source", "")
    
    # Generate Evidence Tags
    evidence_tags <- apply(evidence_data, 1, function(row) {
      tags_list <- c()
      
      # Add Transmission Types
      if (!is.na(row["vertical"]) && row["vertical"] != "suspected unknown") {
        tags_list <- c(tags_list, "Vertical transmission")
      }
      if (!is.na(row["horizontal"]) && row["horizontal"] != "suspected unknown") {
        tags_list <- c(tags_list, "Horizontal transmission")
      }
      if (!is.na(row["oblique"]) && row["oblique"] != "suspected unknown") {
        tags_list <- c(tags_list, "Oblique transmission")
      }
      
      # Add Other Evidence Types
      if (!is.na(row["domains"]) && row["domains"] != "") {
        tags_list <- c(tags_list, "Domains evidence")
      }
      if (!is.na(row["anth_effects"]) && row["anth_effects"] != "not searched") {
        tags_list <- c(tags_list, "Anthropogenic effects")
      }
      
      # Add Unit Source or Behavior Source
      if (row["unit_source"] != "") {
        tags_list <- c(tags_list, row["unit_source"])
      }
      if (row["behavior_source"] != "") {
        tags_list <- c(tags_list, row["behavior_source"])
      }
      
      paste(tags_list, collapse = ", ")
    })
    
    # Create Data Frame for Display
    citations_df <- data.frame(
      Authors = evidence_data$authors,
      Year = evidence_data$year,
      Title = evidence_data$title,
      DOI = ifelse(!is.na(evidence_data$doi), 
                   paste0('<a href="https://doi.org/', evidence_data$doi, 
                          '" target="_blank">', evidence_data$doi, '</a>'), 
                   "No DOI"),
      Information = evidence_tags,
      stringsAsFactors = FALSE
    )
    
    # Remove Duplicate DOIs
    citations_df <- citations_df[!duplicated(citations_df$DOI), ]
    
    # Replace Empty Information with "Other information"
    citations_df$Information <- ifelse(citations_df$Information == "", "Other information", citations_df$Information)
    
    # Render as a DataTable
    datatable(
      citations_df,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
  })
  
  
  
}
