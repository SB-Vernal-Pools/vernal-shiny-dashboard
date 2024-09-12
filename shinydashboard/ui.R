#================================================================
#                      Dashboard Header
#================================================================

# Header
header <- dashboardHeader(
  title = "SB Vernal Pool Monitoring",
  titleWidth = 420
)


#================================================================
#                        Dashboard Sidebar
#================================================================
# Changing the sidebar options and adding Tabs within the main Dashboard Sidebar

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("star")),
    menuItem("Vernal Pool Maps", tabName = "dashboard", icon = icon("gauge")),
    menuItem("Data Visualization", tabName = "dataviz", icon = icon("chart-line"))
  )
)

#================================================================
#                        Dashboard Body
#================================================================
# Changing the elements withing each Tab created from the sidebar

# Body
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "sass-styles.css"),
    tags$script(src = "https://kit.fontawesome.com/b7f4c476ba.js")
  ),
  
  #      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #                            Welcome Page
  #      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  tabItems(
    tabItem("welcome",
            fluidRow(
              box(width = 12,
                  title = tagList(icon("tint"), strong("Santa Barbara 2019 Vernal Pool Monitoring Dashboard")),
                  tags$img(src = "venal-pic-ex.jpg", height = "75%", width = "75%", align = "center",
                           style = "max-width:80%; text-align: center; display: block; margin: auto;"),
                  includeMarkdown("text/welcome.md")
              ),
              box(width = 12,
                  title = tagList(icon("tint"), strong("Background on Vernal Pools")),
                  tags$img(src = "Vernal_pool_cross_section_diagram.jpg", height = "75%", width = "75%", align = "center",
                           style = "max-width:80%; text-align: center; display: block; margin: auto;"),
                  includeMarkdown("text/usage.md")
              ),
              box(width = 12,
                  title = tagList(icon("tint"), strong("Data")),
                  tags$img(height = "75%", width = "75%", align = "center",
                           style = "max-width:80%; text-align: center; display: block; margin: auto;"),
                  includeMarkdown("text/data.md") # https://www.google.com/url?sa=i&url=https%3A%2F%2Fohv.parks.ca.gov%2F%3Fpage_id%3D27452&psig=AOvVaw3ugCtlsqzW8-tq6YoaF3RC&ust=1724806019834000&source=images&cd=vfe&opi=89978449&ved=0CBQQjRxqFwoTCNikqaX5k4gDFQAAAAAdAAAAABAE
              ),
              box(
                  width = 12,
                  title =tagList(icon("tint"), strong("Plant Species List")),
                  dataTableOutput("species_list")
              )
            )
    ),
    
    
    #      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #                           POOL MAPPING TAB                   ----
    #      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    tabItem(tabName = "dashboard",
            
            # how-to data viz box
            fluidRow(
              box(width = 12,
                  title = strong("How to Use the Mapping Dashboard"),
                  includeMarkdown("text/how-to-map.md")
              )), #END welcome fluidRow
            
            # pool-level viz box
            fluidRow(
              box(width = 12,
                  title = strong("Vernal Pool Interactive Map"),
                  
                  # filters for vernal pool map ----
                  fluidRow(
                    
                    column(width = 12,
                           leafletOutput("map"))
                  ) 
              )
            ), # END pool-level viz fluidRow
            
            
            # transect-level viz box ----
            
    ), 
    
    
    #      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #                            DATA VIZ TAB                     ----
    #      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # how-to on data viz page
    tabItem("dataviz",
            
            # how-to fluidRow ----
            fluidRow(
              box(width = 12,
                  title = strong("How to Use the Data Visualization Tab"),
                  collapsible = TRUE,
                  includeMarkdown("text/how-to-data-viz.md")
              )), #END how-to fluidRow
            
            
            
            
            
            # Pool-level data visualizations ----
            fluidRow(
              box(width = 12,
                  title = strong("Pool-Level Visualizations"),
                  
                  fluidRow(
                    
                    # pool-level viz filters ----
                    column(width = 6,
                           selectizeInput("p_viz_location_pool_id", "Select Location-Pool ID:",
                                          choices = sort(unique(c(hydro$location_pool_id, 
                                                                  percent_cover$location_pool_id))),
                                          multiple = FALSE)),
                    
                    column(width = 6,
                           selectizeInput("p_viz_water_year", "Select Water Year:",
                                          choices = unique(hydro$water_year),
                                          multiple = FALSE)),
                    
                    # pool-level visualization output ----
                    column(width = 6,
                           plotlyOutput("water_level_viz")), 
                    
                    column(width = 6,
                           plotlyOutput("spp_abundance_viz"))
                    
                    
                    
                  ))), # END pool-level fluidRow 
            
            
            
            #Transect-level data visualizations ----
            fluidRow(
              box(width = 8,
                  title = strong("Transect-Level Visualizations"),
                  fluidRow(
                    
                    # Transect-level viz filters
                    column(width = 6,
                           selectizeInput("tr_viz_location_pool_id", "Select Location-Pool ID:",
                                          choices = sort(unique(c(hydro$location_pool_id, 
                                                                  percent_cover$location_pool_id))),
                                          multiple = FALSE)),
                    
                    column(width = 6, 
                           selectizeInput("tr_viz_quadrat", "Select Transect:",
                                          choices = sort(unique(percent_cover$transect_axis)),
                                          multiple = FALSE)),
                    
                    column(width = 6,
                           selectizeInput("tr_viz_type", "Select Metric:",
                                          choices = c("Sum of Native Cover",
                                                      "Count of Native Species",
                                                      "Sum of Non-Native Cover", 
                                                      "Count of Non-Native Species",
                                                      "Percent Thatch", 
                                                      "Percent Bare Ground",
                                                      "Percent Cover Single Species"),
                                          multiple = FALSE)),
                    
                    # spp filter only appears when "Percent Cover Single Species" selected
                    
                    column(width = 6,
                           conditionalPanel(
                             condition = "input.tr_viz_type == 'Percent Cover Single Species'", 
                             selectizeInput("tr_viz_species", "Select Species:",
                                            choices = NULL,
                                            multiple = FALSE))),
                    
                    # transect-level visualization output 
                    column(width = 12,
                           plotlyOutput("transect_level_viz"))
                    
                  ) # END Transect viz fluidRow
                  
              ), # END transect-level visualization box
              
              # Images/info box on viz page ----
              box(width = 4,
                  height = 612,
                  title = strong("Additional Information"),
                  includeMarkdown("text/additional-info.md"),
                  conditionalPanel("input.tr_viz_type == 'Percent Cover Single Species'",
                                   imageOutput("tr_spp_image"),
                                   uiOutput("tr_spp_text"))
                  
              )
            ) # END main fluidRow
            
    ) 
  )
)

#================================================================
#                        Dashboard Page
#================================================================
#   Combines the elements above to create the dashboard
dashboardPage(header, sidebar, body,
              fresh::use_theme("shinydashboard-fresh-theme.css"))
