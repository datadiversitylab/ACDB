###################
# ui.R
# 
# UI controller. 
# Used to define the graphical aspects of the app.
###################

library(bslib)
library(leaflet)
library(DT)

ui <- 
  navbarPage(
    id = "tabs",
    theme = bs_theme(bootswatch = "cosmo",
                     primary = "#4CAF50",
                     secondary = "#555555",
                     base_font = font_google("Roboto")
    ),
    div(
      tags$img(src = "DDLLogo_white.png", style = "height: 40px; margin-right: 10px;"),
      style = "font-size: 24px; font-weight: bold; padding: 0 20px;",
      "The Animal Culture Database"
    ),    
    header = tagList(
  tags$style(
    HTML("
    .navbar-nav {
      margin: 0 auto;
      display: flex;
      justify-content: center;
    }
    .navbar-nav > li > a {
      font-size: 16px;
      font-weight: 500;
      padding: 15px 20px;
    }
    .navbar { position: sticky; top: 0; z-index: 1020; }
  ")
  ),
  tags$script(defer = NA,
    src = "https://umami.datadiversitylab.synology.me/script.js",
    `data-website-id` = "543cffef-a100-47af-9dd9-2cf39517077b",
    `data-domains` = "datadiversitylab.github.io",
    `data-tag` = "acdb"
  )
),
    # Landing page
    tabPanel(
      tags$div(icon("home"), "Home"),
      fluidPage(
        tags$div(
          style = "max-width: 900px; margin: 0 auto; padding: 40px 20px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; color: #333333; line-height: 1.8;",
          
          tags$div(
            style = "background-color: #f9f9f9; padding: 30px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
            
            # Title and subtitle
            tags$h1(
              style = "font-size: 48px; font-weight: 300; text-align: center; color: #4CAF50; margin-bottom: 20px;",
              "Animal Culture Database (ACDB)"
            ),
            tags$h2(
              style = "font-size: 24px; font-weight: 300; text-align: center; color: #555555; margin-bottom: 40px;",
              "Exploring the World’s Diversity of Nonhuman Animal Traditions"
            ),
            #br(),
            tags$div(
              style = "text-align: center; margin-top: 30px;",
              tags$img(
                src = "globe_animals.png", 
                style = "max-width: 40%; height: auto; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); border-radius: 8px;"
              )
            ),
            br(),
            
            # Introduction text
            tags$p(
              style = "text-align: justify; margin-bottom: 15px;",
              "The ACDB (Animal Culture Database) is a database containing variables on socially learned behavioral traditions in wild animal populations worldwide. At present, the database contains data on 102 populations of birds and mammals across six continents, with descriptions of behaviors including migration, vocal dialects, foraging methods, and mating displays."
            ),
            
            # Explore button
            tags$div(
              style = "text-align: center;",
              actionButton(inputId = "explore_button", 
                           label = "Explore the Database",
                           style = "display:inline-block; padding: 15px 30px; border-radius: 5px; background: #4CAF50; color: #ffffff; text-decoration: none; font-size: 18px; font-weight: 400; transition: background 0.3s ease;")
            )
          ),
          
          # Quick stats section 
          tags$div(
            style = "margin-top: 40px; background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
            tags$h3(
              style = "font-size: 24px; font-weight: bold; color: #4CAF50; text-align: center; margin-bottom: 20px;",
              "Quick Stats"
            ),
            tags$div(
              style = "display: flex; justify-content: space-around; text-align: center;",
              
              # Species covered
              tags$div(
                tags$h4(style = "font-size: 20px; font-weight: bold; color: #333;", uiOutput("species_count")),
                tags$p(style = "font-size: 16px; color: #555;", "Species covered")
              ),
              
              # Behaviors documented
              tags$div(
                tags$h4(style = "font-size: 20px; font-weight: bold; color: #333;", uiOutput("behavior_count")),
                tags$p(style = "font-size: 16px; color: #555;", "Behaviors documented")
              ),
              
              # Populations cataloged
              tags$div(
                tags$h4(style = "font-size: 20px; font-weight: bold; color: #333;", uiOutput("population_count")),
                tags$p(style = "font-size: 16px; color: #555;", "Populations cataloged")
              )
            )
          )
        )
      )
    )
    
    ,
    
    # Populations page
    tabPanel(
      value = "Populations_tab",
      tags$div(icon("globe"), "Populations"),
      fluidPage(
        #titlePanel("Populations"),
        tags$div(
          style = "max-width: 900px; margin: 0 auto; padding: 20px 0; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; color: #333333; line-height: 1.8;",
          
          tags$div(
            style = "background-color: #f9f9f9; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
            
            # Introduction section
            tags$h3(
              style = "font-size: 24px; font-weight: bold; color: #4CAF50; text-align: center; margin-bottom: 20px;",
              "Explore population-level data"
            ),
            tags$p(
              style = "text-align: justify; margin-bottom: 15px;",
              " A ‘group’ in this database is a population of animals within a species that displays one or more socially transmitted behaviors forming a tradition. There can be multiple groups within a species, and groups nested within other groups in species with multilevel social structures such as killer whales and elephants. For instance, there may be multiple pods of killer whales in a clan, and they may share a whistle dialect at the pod level and a pulsed call dialect at the clan level. "
            ),
            # tags$p(
            #   style = "text-align: justify; margin-bottom: 15px;",
            #   " Vestibulum et erat at mauris ornare feugiat. Vivamus vitae augue ac neque congue egestas sed nec turpis. Nam dolor nunc, aliquam venenatis interdum mattis, luctus in velit. Nulla facilisi. Nullam interdum diam vel lobortis bibendum. Nulla ut sem elit. In at sapien fringilla, auctor elit ac, vulputate ante. Maecenas viverra consectetur venenatis. Cras eu dapibus augue, ut pretium quam. Sed commodo lacinia lacus accumsan mollis. Fusce imperdiet ligula eros, vitae pellentesque mauris convallis eu. Vestibulum facilisis purus in risus cursus, vitae eleifend mi consequat. "
            # ),
            # tags$p(
            #   style = "text-align: justify; margin-bottom: 0;",
            #   " Nunc placerat blandit enim, ac convallis magna pharetra at. Donec quis ornare mi, hendrerit tristique eros. Curabitur ultrices ultricies ornare. Suspendisse potenti. Cras iaculis turpis eget arcu pretium feugiat. Pellentesque condimentum lorem quis facilisis sollicitudin. Nulla eu lacus elementum, scelerisque quam non, condimentum turpis. Nunc at lectus at quam varius feugiat vitae ac ante. In suscipit sagittis augue accumsan maximus. Sed et elit et mi laoreet efficitur id quis neque. "
            # )
          )
        ),
        
        # Map and Table Section
        tags$div(
          style = "max-width: 1100px; margin: 40px auto;",
          
          # Map with a shadow effect
          tags$div(
            style = "background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); margin-bottom: 40px;",
            tags$h4(
              style = "font-size: 20px; font-weight: bold; color: #4CAF50; margin-bottom: 15px;",
              "Interactive Map"
            ),
            tags$p(
              style = "margin-bottom: 10px;",
              "This map displays the approximate geographic location of the populations in the database. For some species with wide ranges (e.g. whales), the coordinates given are somewhere in the middle of the range. The “location source” variable in the groups table can be used to find further details on the location in the referenced publication. More detailed location and range data is a goal for future versions of the database. "
            ),
            leafletOutput("population_map", height = 400)
            
          ),
          
          # Table with a shadow effect
          tags$div(
            style = "background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
            tags$h4(
              style = "font-size: 20px; font-weight: bold; color: #4CAF50; margin-bottom: 15px;",
              "Population Data Table"
            ),
            tags$p(
              style = "margin-bottom: 10px;",
              "Click on each group to display taxonomic information, a list of behaviors and descriptions, and a focused map of the group location. The ‘search’ bar can be used to filter for specific entries. "
            ),
            DTOutput("groups_table")
          )
        )
      )
    ),
    
    # Citations
    tabPanel(
      tags$div(icon("book"), "Citations"),
      fluidPage(
        titlePanel("Citations and Evidence Types"),
        tags$div(
          style = "padding: 10px; border: 1px solid #ccc; border-radius: 5px; background-color: #f9f9f9;",
          dataTableOutput("citations_table")
        )
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
              style = "font-size: 36px; font-weight: bold; text-align: center; color: #4CAF50; margin-bottom: 20px;",
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
                style = "font-size: 20px; font-weight: bold; color: #4CAF50; margin-bottom: 10px;",
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
                style = "font-size: 20px; font-weight: bold; color: #4CAF50; margin-bottom: 10px;",
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
                style = "font-size: 20px; font-weight: bold; color: #4CAF50; margin-bottom: 10px;",
                "How can I contribute data to the database?"
              ),
              tags$p(
                style = "text-align: justify; margin: 0;",
                "We are currently working on a pipeline for contributions from researchers. For now, please email ",
                tags$a(href = "mailto:kcb7@arizona.edu", "Dr. Basava"),
                " with a description of the sort of data you would like to be added. Thank you for your interest in contributing data!"
              )
              
            ),
            
            # Question 4
            tags$div(
              style = "margin-bottom: 30px; background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
              tags$h4(
                style = "font-size: 20px; font-weight: bold; color: #4CAF50; margin-bottom: 10px;",
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
                style = "font-size: 20px; font-weight: bold; color: #4CAF50; margin-bottom: 10px;",
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
