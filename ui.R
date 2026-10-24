###################
# ui.R
# 
# UI controller. 
# Used to define the graphical aspects of the app.
###################

library(bslib)
library(leaflet)
library(DT)

# Version shown in the navbar and on the landing page
acdb_version <- if (file.exists("VERSION")) trimws(readLines("VERSION", warn = FALSE)[1]) else ""

# Files in www/ with their modification time, so that browsers load the new version after an update
asset <- function(f) {
  path <- file.path("www", f)
  if (file.exists(path)) paste0(f, "?v=", as.integer(file.mtime(path))) else f
}

ui <- 
  navbarPage(
    id = "tabs",
    theme = bs_theme(bootswatch = "cosmo",
                     primary = "#1B7F79",
                     secondary = "#555555",
                     base_font = font_google("Roboto")
    ),
    div(
      tags$img(src = "DDLLogo_white_small.png", style = "height: 40px; margin-right: 10px;"),
      style = "font-size: 20px; font-weight: bold; padding: 0 20px;",
      "Animal Culture Database",
      tags$span(class = "acdb-ver", paste0("v", acdb_version))
    ),    
    header = tagList(
      tags$link(rel = "stylesheet", href = asset("acdb.css")),
      tags$link(rel = "stylesheet", href = asset("card.css")),
      tags$script(src = asset("photos.js"), defer = NA),
      tags$script(src = asset("tree.js"), defer = NA),
      tags$script(src = asset("print.js"), defer = NA),
      tags$script(defer = NA,
        src = "https://umami.datadiversitylab.synology.me/script.js",
        `data-website-id` = "543cffef-a100-47af-9dd9-2cf39517077b",
        `data-domains` = "datadiversitylab.github.io",
        `data-tag` = "acdb"
      )
    ),
    
    # Landing page
    tabPanel(
      value = "Explore_tab",
      tags$div(icon("globe"), "Explore"),
      
      tags$div(
        class = "acdb-hero",
        tags$div(
          class = "acdb-pills",
          tags$a(class = "acdb-pill", href = "https://github.com/datadiversitylab/ACDB/releases", target = "_blank",
                 tags$span(class = "dot"), paste("Version", acdb_version)),
          tags$a(class = "acdb-pill", href = "https://doi.org/10.1038/s41597-025-05315-y", target = "_blank", "Read the data paper")
        ),
        tags$h1("Animal Culture Database"),
        tags$p(
          class = "acdb-lead",
          "Wild animals learn from each other. Dialects, foraging methods, migration routes, and mating displays spread through groups and last across generations. The ACDB brings these cases together."
        ),
        uiOutput("hero_chips"),
        tags$div(
          class = "acdb-actions",
          actionButton("go_map", "Explore the map", class = "acdb-btn-amber"),
          actionButton("go_table", "Browse populations", class = "acdb-btn-ghost"),
          actionButton("go_tree", "Tree of life", class = "acdb-btn-ghost"),
          actionButton("go_contribute", "Contribute data", class = "acdb-btn-ghost")
        )
      ),
      
      # Example population
      tags$div(
        class = "acdb-wrap",
        tags$div(
          class = "acdb-example",
          tags$div(
            class = "acdb-ex-top",
            tags$span(class = "acdb-eyebrow", "Meet a population"),
            actionLink("another", tagList(icon("sync"), " Show another"))
          ),
          uiOutput("featured")
        )
      ),
      
      # Footer
      tags$div(
        class = "acdb-foot",
        paste("Version", acdb_version), " | ",
        tags$a(href = "https://doi.org/10.1038/s41597-025-05315-y", target = "_blank", "Data paper"), " | ",
        tags$a(href = "https://github.com/datadiversitylab/ACDB", target = "_blank", "GitHub")
      )
    ),
    
    # Map, with the species in view
    tabPanel(
      value = "Map_tab",
      tags$div(icon("map"), "Map"),
      tags$div(
        class = "acdb-section",
        tags$h3("Map"),
        tags$p("Points show the approximate location of each population. For species with wide ranges (e.g. whales), the point is somewhere in the middle of the range. Move the map and the list on the right follows. Click a species or a point for its card."),
        tags$div(
          class = "acdb-mapsplit",
          tags$div(
            class = "acdb-mapbox",
            tags$div(
              class = "acdb-maptools",
              selectInput("map_class", NULL, choices = c("All taxonomic groups" = "All"), width = "220px"),
              selectInput("map_domain", NULL, choices = c("All behavior domains" = "All"), width = "220px")
            ),
            leafletOutput("population_map", height = 600)
          ),
          tags$div(
            class = "acdb-side",
            tags$div(class = "acdb-side-head", tags$h4("In view"), textOutput("side_note")),
            tags$div(class = "acdb-side-list", uiOutput("map_species"))
          )
        )
      )
    ),
    
    # Populations: gallery and table
    tabPanel(
      value = "Populations_tab",
      tags$div(icon("th-large"), "Populations"),
      tags$div(
        class = "acdb-section",
        tags$h3("Populations"),
        tags$details(
          tags$summary("What is a group?"),
          tags$p(
            "A \u2018group\u2019 in this database is a population of animals within a species that displays one or more socially transmitted behaviors forming a tradition. There can be multiple groups within a species, and groups nested within other groups in species with multilevel social structures such as killer whales and elephants. For instance, there may be multiple pods of killer whales in a clan, and they may share a whistle dialect at the pod level and a pulsed call dialect at the clan level."
          )
        ),
        tags$div(
          class = "acdb-filters",
          textInput("pop_q", NULL, placeholder = "Search groups, species, or places", width = "100%"),
          selectInput("pop_class", NULL, choices = c("All taxonomic groups" = "All"), width = "100%"),
          selectInput("pop_domain", NULL, choices = c("All behavior domains" = "All"), width = "100%"),
          selectInput("pop_sort", NULL, choices = c("Name (A to Z)" = "name", "Most behaviors" = "behaviors"), width = "100%")
        ),
        tags$div(class = "acdb-count", textOutput("pop_note"), downloadLink("pop_csv", tagList(icon("download"), " Download these populations (CSV)"))),
        tabsetPanel(
          id = "pop_view",
          type = "pills",
          tabPanel("Gallery", uiOutput("pop_cards"), uiOutput("pop_more")),
          tabPanel("Table",
            tags$p(class = "acdb-hint", "Click on a row to display taxonomic information, a list of behaviors and descriptions, and a focused map of the group location."),
            DTOutput("groups_table"))
        )
      )
    ),
    
    # Tree of life
    tabPanel(
      value = "Tree_tab",
      tags$div(icon("sitemap"), "Tree of life"),
      tags$div(
        class = "acdb-section",
        tags$h3("Tree of life"),
        tags$p("Every species in the database, placed by taxonomy (phylum, class, order, family, genus). Branch lengths do not show evolutionary time. Each ring is a behavior domain, and a colored dot means the domain has been recorded for that species. Highlight a domain to see where it occurs. Click a species to open its card."),
        tags$div(
          class = "acdb-treecard",
          tags$div(id = "acdb_legend", class = "acdb-legend"),
          tags$div(id = "acdb_tree")
        ),
        tags$div(id = "acdb_tip")
      )
    ),
    
    # Species cards
    tabPanel(
      value = "Cards_tab",
      tags$div(icon("id-card"), "Cards"),
      tags$div(
        class = "acdb-section",
        tags$h3("Species cards"),
        tags$p("A one-page card for each species, with the photo, taxonomy, behaviors, sources, and the version of the database. Pick a species, then download it as a PDF to print or share."),
        tags$div(
          class = "acdb-cardtools",
          selectizeInput(
            "card_species", NULL, choices = NULL, width = "420px",
            options = list(
              placeholder = "Search by common or scientific name",
              render = I("{
                option: function(item, escape) {
                  var m = item.label.match(/^(.*) \\(([^()]*)\\)$/);
                  return '<div>' + (m ? escape(m[1]) + ' <i>' + escape(m[2]) + '</i>' : escape(item.label)) + '</div>';
                },
                item: function(item, escape) {
                  var m = item.label.match(/^(.*) \\(([^()]*)\\)$/);
                  return '<div>' + (m ? escape(m[1]) + ' <i>' + escape(m[2]) + '</i>' : escape(item.label)) + '</div>';
                }
              }")
            )
          ),
          actionButton("card_print", "Download PDF", icon = icon("download"), class = "btn-primary"),
          actionButton("card_random", "Surprise me", icon = icon("random"), class = "btn-default")
        ),
        tags$div(class = "acdb-paper", uiOutput("card_preview"))
      )
    ),
    
    # Statistics
    tabPanel(
      value = "Stats_tab",
      tags$div(icon("chart-bar"), "Statistics"),
      tags$div(
        class = "acdb-section",
        tags$h3("Statistics"),
        tags$p("What is in the database today. Counts are populations unless noted."),
        uiOutput("stats_kpis"),
        tags$div(
          class = "acdb-statgrid",
          tags$div(
            class = "acdb-panel",
            tags$div(class = "top", tags$h4("By taxonomic group"),
                     selectInput("stats_rank", NULL, choices = c("Class" = "Class", "Order" = "Ordr", "Family" = "Family"), width = "130px")),
            uiOutput("stats_taxa")
          ),
          tags$div(class = "acdb-panel", tags$div(class = "top", tags$h4("By region")), uiOutput("stats_region")),
          tags$div(class = "acdb-panel", tags$div(class = "top", tags$h4("By behavior domain")), uiOutput("stats_domain")),
          tags$div(class = "acdb-panel", tags$div(class = "top", tags$h4("By conservation status (species)")), uiOutput("stats_iucn"))
        ),
        tags$div(class = "acdb-panel", tags$div(class = "top", tags$h4("Regions and taxonomic groups")), uiOutput("stats_cross")),
        tags$div(class = "acdb-panel", style = "margin-top: 16px;", tags$div(class = "top", tags$h4("Behavior domains across the animal tree (species)")), uiOutput("stats_domain_class"))
      )
    ),
    
    # Literature
    navbarMenu(
      "Literature",
      icon = icon("book"),
      tabPanel(
        value = "Citations_tab",
        tags$div(icon("book"), "Citations"),
        tags$div(
          class = "acdb-section",
          tags$h3("Citations"),
          tags$p("Every paper behind the data. The table shows the species coded from each paper and what the paper informs."),
          DTOutput("citations_table")
        )
      ),
      # New papers, not coded yet
      tabPanel(
        value = "Papers_tab",
        tags$div(icon("newspaper"), "New papers"),
        tags$div(
          class = "acdb-section",
          tags$h3("New papers"),
          tags$p(
            "These papers are not coded yet. They will be added to the database in a future release. If you know of a paper that is missing, please send it to us through the Contributions tab."
          ),
          DTOutput("papers_table")
        )
      )
    ),
    
    # About
    tabPanel(
      value = "About_tab",
      tags$div(icon("info-circle"), "About"),
      tags$div(
        class = "acdb-section acdb-narrow",
        tags$h3("About the ACDB"),
        tags$p(
          "The ACDB (Animal Culture Database) is a database of variables on socially learned behavioral traditions in wild animal populations worldwide. It brings together the current literature on behavioral variation within species, in contexts such as communication, foraging, and migration. The data are open access and can be explored in this app."
        ),
        tags$div(
          class = "acdb-aims",
          tags$div(tags$h5("Document"), tags$p("Record socially transmitted behaviors observed in wild animal populations.")),
          tags$div(tags$h5("Analyze"), tags$p("Examine patterns of cultural diversity across the globe and across species.")),
          tags$div(tags$h5("Inform"), tags$p("Use what we know about animal culture to support biodiversity conservation."))
        ),
        tags$h4("How the data are organized"),
        tags$p(
          "The database has four tables: groups, species, behaviors, and sources. A group is a population of one species that shares one or more traditions. Each behavior belongs to a group and cites the sources that support it. The ",
          tags$a(href = "https://github.com/datadiversitylab/ACDB/blob/main/ACDB_codebook_2026.pdf", target = "_blank", "codebook"),
          " defines every variable."
        ),
        tags$h4("Versions and updates"),
        tags$p(
          "Major releases (e.g. v1.0.0, v2.0.0) are based on the addition of 100 or more populations. Minor releases (e.g. v1.1.0) are based on the addition of fewer than 100 populations. Changes to the contents of the database, such as a missing paper, a taxonomy fix, or a new location, are patches (e.g. v1.1.1). We plan to cover most of the existing animal behavior literature in version 2.0, and we are building a pipeline for external contributions."
        ),
        tags$h4("Who we are"),
        tags$p("The ACDB was developed at the University of Arizona. Contact Dr. Basava about the data and Dr. Román-Palacios about the app."),
        tags$div(
          class = "acdb-team",
          tags$div(class = "acdb-person", tags$div(class = "dot chip-1", "KB"), tags$div(tags$b("Kiran Basava"), tags$small(tags$a(href = "mailto:kcb7@arizona.edu", "kcb7@arizona.edu")))),
          tags$div(class = "acdb-person", tags$div(class = "dot chip-2", "MA"), tags$div(tags$b("Md Nafis Ul Alam"))),
          tags$div(class = "acdb-person", tags$div(class = "dot chip-3", "LR"), tags$div(tags$b("Liam Roberts"))),
          tags$div(class = "acdb-person", tags$div(class = "dot chip-4", "KM"), tags$div(tags$b("Kristen Martinet"))),
          tags$div(class = "acdb-person", tags$div(class = "dot chip-5", "PC"), tags$div(tags$b("Paige Cherry"))),
          tags$div(class = "acdb-person", tags$div(class = "dot chip-6", "HG"), tags$div(tags$b("Héctor García-Verdugo"))),
          tags$div(class = "acdb-person", tags$div(class = "dot chip-1", "CR"), tags$div(tags$b("Cristian Román-Palacios"), tags$small(tags$a(href = "mailto:cromanpa@arizona.edu", "cromanpa@arizona.edu"))))
        ),
        tags$h4("How to cite"),
        tags$div(
          class = "acdb-cite",
          "Basava K, Alam MNU, Roberts L, Martinet K, Cherry P, García-Verdugo H, Román-Palacios C (2025). Mapping nonhuman cultures with the Animal Culture Database. ",
          tags$i("Scientific Data"), " 12. ",
          tags$a(href = "https://doi.org/10.1038/s41597-025-05315-y", target = "_blank", "https://doi.org/10.1038/s41597-025-05315-y")
        ),
        tags$p(
          style = "margin-top: 16px;",
          "The code and data are on ", tags$a(href = "https://github.com/datadiversitylab/ACDB", target = "_blank", "GitHub"),
          " under the MIT license."
        )
      )
    ),
    
    # More
    navbarMenu(
      "More",
      icon = icon("ellipsis-h"),
      # Contributions
      tabPanel(
        value = "Contributions_tab",
        tags$div(icon("envelope"), "Contributions"),
        tags$div(
          class = "acdb-section acdb-narrow",
          tags$h3("Contribute to the ACDB"),
          tags$p(
            "The database grows with help from researchers. If you have data on a socially learned behavior in a wild animal population, we would like to hear from you."
          ),
          tags$p(
            "We are still building a submission pipeline. For now, please send your contribution by email to ",
            tags$a(href = "mailto:kcb7@arizona.edu", "kcb7@arizona.edu"), "."
          ),
          tags$h4("What to include"),
          tags$ul(
            tags$li("The reference, with the DOI if there is one."),
            tags$li("The species and the population."),
            tags$li("The behavior, and the evidence that it is socially learned."),
            tags$li("Where the population lives.")
          ),
          tags$p(
            "A missing paper, a taxonomy fix, or a better location is also welcome. These are released as patches."
          ),
          tags$a(class = "btn btn-primary", href = "mailto:kcb7@arizona.edu?subject=ACDB%20contribution", "Send an email")
        )
      ),
    
      # Help tab
      tabPanel(
        tags$div(icon("question-circle"), "Help"),
        fluidPage(
          tags$div(
            style = "max-width: 900px; margin: 0 auto; padding: 40px 20px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; color: #333333; line-height: 1.8;",
          
            # Help introduction
            tags$div(
              style = "background-color: #f9f9f9; padding: 30px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
              tags$h1(
                style = "font-size: 36px; font-weight: bold; text-align: center; color: #1B7F79; margin-bottom: 20px;",
                "Help and FAQs"
              ),
              tags$p(
                style = "text-align: center; color: #555555; margin-bottom: 40px;",
                "Please contact ",
                tags$a(href = "mailto:kcb7@arizona.edu", "Dr. Kiran Basava"),
                " if your questions are not answered below!"
              )
            
            ),
          
            # FAQ Section
            tags$div(
              style = "margin-top: 20px;",
            
              # Question 1
              tags$div(
                style = "margin-bottom: 30px; background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                tags$h4(
                  style = "font-size: 20px; font-weight: bold; color: #1B7F79; margin-bottom: 10px;",
                  "How do I search for populations?"
                ),
                tags$p(
                  style = "text-align: justify; margin: 0;",
                  "Use the ‘search’ bar in the population data table to search for specific groups, species, or locations."
                )
              ),
            
              # Question 2
              tags$div(
                style = "margin-bottom: 30px; background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                tags$h4(
                  style = "font-size: 20px; font-weight: bold; color: #1B7F79; margin-bottom: 10px;",
                  "How can I download data?"
                ),
                tags$p(
                  style = "text-align: justify; margin: 0;",
                  "Please visit our ",
                  tags$a(href = "https://github.com/datadiversitylab/ACDB_datarelease", 
                         "GitHub repository"),
                  " for the current release of the SQLite database, or you can download separate CSV files for each table (in the table_csvs folder)."
                )
              
              ),
            
              # Question 3
              tags$div(
                style = "margin-bottom: 30px; background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                tags$h4(
                  style = "font-size: 20px; font-weight: bold; color: #1B7F79; margin-bottom: 10px;",
                  "How can I contribute data to the database?"
                ),
                tags$p(
                  style = "text-align: justify; margin: 0;",
                  "We are currently working on a pipeline for contributions from researchers. For now, please see the Contributions tab, or email ",
                  tags$a(href = "mailto:kcb7@arizona.edu", "Dr. Basava"),
                  " with a description of the sort of data you would like to be added. Thank you for your interest in contributing data!"
                )
              
              ),
            
              # Question: species cards
              tags$div(
                style = "margin-bottom: 30px; background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                tags$h4(
                  style = "font-size: 20px; font-weight: bold; color: #1B7F79; margin-bottom: 10px;",
                  "How do I get a printable card for a species?"
                ),
                tags$p(
                  style = "text-align: justify; margin: 0;",
                  "Open the Cards tab, pick a species, and click Download PDF. You can also download the card from the species window that opens when you click a population on the map or in the gallery. The card has the photo, taxonomy, behaviors, sources, and the version of the database."
                )
              ),
            
              # Question: tree
              tags$div(
                style = "margin-bottom: 30px; background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                tags$h4(
                  style = "font-size: 20px; font-weight: bold; color: #1B7F79; margin-bottom: 10px;",
                  "What does the tree of life show?"
                ),
                tags$p(
                  style = "text-align: justify; margin: 0;",
                  "It places every species in the database on a tree built from the GBIF taxonomy (phylum, class, order, family, genus, species). The branch lengths do not represent evolutionary time. The colored rings show which behavior domains have been recorded for each species. Click a domain at the top to highlight it, and click a species to open its card."
                )
              ),
            
              # Question 4
              tags$div(
                style = "margin-bottom: 30px; background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                tags$h4(
                  style = "font-size: 20px; font-weight: bold; color: #1B7F79; margin-bottom: 10px;",
                  "Where can I learn more about animal culture?"
                ),
                tags$p(
                  "Some recent and accessible reviews of the research on animal culture include:", 
                  tags$br(),
                  tags$a(href = "https://onlinelibrary.wiley.com/doi/10.1002/bies.201900060", 
                         "Allen, Jenny A. (2019). Community through Culture: From Insects to Whales: How Social Learning and Culture Manifest across Diverse Animal Communities."),
                  tags$br(),
                  tags$a(href = "https://www.science.org/doi/10.1126/science.abe6514", 
                         "Whiten, Andrew. (2021). The Burgeoning Reach of Animal Culture."),
                  tags$br(), tags$br(),
                  "For the relevance of animal culture to conservation:",
                  tags$br(),
                  tags$a(href = "https://www.science.org/doi/abs/10.1126/science.aaw3557", 
                         "Brakes, Philippa, et al. (2019). Animal Cultures Matter for Conservation.")
                )
              
              ),
            
              # Question 5
              tags$div(
                style = "margin-bottom: 30px; background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                tags$h4(
                  style = "font-size: 20px; font-weight: bold; color: #1B7F79; margin-bottom: 10px;",
                  "Who can I contact for technical support?"
                ),
                tags$p(
                  style = "text-align: justify; margin: 0;",
                  "Please contact",
                  tags$a(href = "mailto:cromanpa@arizona.edu", "Dr. Román-Palacios.")
                )
              )
            )
          )
        )
      )
    )
  )
