#================================================================
#                      Dashboard Header
#================================================================

header <- dashboardHeader(
  title = "SB Vernal Pool Monitoring",
  titleWidth = 420
)

#================================================================
#                        Dashboard Sidebar
#================================================================

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarMenu",  # Add this ID
              menuItem("Welcome", tabName = "welcome", icon = icon("star")),
              menuItem("Vernal Pool Maps", tabName = "dashboard", icon = icon("gauge")),
              menuItem("Data Visualization", tabName = "dataviz", icon = icon("chart-line"))
  ))

#================================================================
#                        Dashboard Body
#================================================================

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "sass-styles.css"),
    tags$script(src = "https://kit.fontawesome.com/b7f4c476ba.js")
  ),
  
  # Important: tabItems() wrapper around all tabItem elements
  tabItems(
    # Welcome Tab ----
    tabItem(
      tabName = "welcome",
      fluidRow(
        box(width = 12,
            title = tagList(icon("tint"), strong("Welcome - Santa Barbara Vernal Pool Mapping")),
            tags$img(src = "venal-pic-ex.jpg", height = "75%", width = "75%", align = "center",
                     style = "max-width:80%; text-align: center; display: block; margin: auto;"),
            includeMarkdown("text/welcome.md")
        )
      ),
      fluidRow(
        box(width = 12,
            title = tagList(icon("tint"), strong("Background on Vernal Pools")),
            includeMarkdown("text/vernalpools.md"),
            div(style = "display: flex; justify-content: center; margin: 20px; text-align: center; flex-wrap: wrap;",
                # Winter phase
                div(style = "text-align: center; flex: 1; margin: 0 10px; display: flex; flex-direction: column; align-items: center;",
                    tags$img(src = "winter-phase.png", style = "max-width: 100%; height: auto; width: 300px;"),
                    tags$p("Winter Phase", style = "text-align: center; font-weight: bold; margin-top: 10px;")
                ),
                # Spring phase
                div(style = "text-align: center; flex: 1; margin: 0 10px; display: flex; flex-direction: column; align-items: center;",
                    tags$img(src = "spring-phase.png", style = "max-width: 100%; height: auto; width: 300px;"),
                    tags$p("Spring Phase", style = "text-align: center; font-weight: bold; margin-top: 10px;")
                ),
                # Summer-Fall phase
                div(style = "text-align: center; flex: 1; margin: 0 10px; display: flex; flex-direction: column; align-items: center;",
                    tags$img(src = "summer-fall-phase.png", style = "max-width: 100%; height: auto; width: 300px;"),
                    tags$p("Summer/Fall Phase", style = "text-align: center; font-weight: bold; margin-top: 10px;")
                )
            )
        )
      ),
      fluidRow(
        box(width = 12,
            title = tagList(icon("tint"), strong("About the Data")),
            includeMarkdown("text/data.md")
        )
      )
    ),
    
    # Dashboard Tab (Pool Mapping) ----
    tabItem(
      tabName = "dashboard",
      fluidRow(
        box(width = 12,
            title = strong("Explore Santa Barbara Vernal Pools"),
            includeMarkdown("text/how-to-map.md"),
            leafletOutput("map")
        )
      ),
    ),
    
    # Data Visualization Tab ----
    tabItem(
      tabName = "dataviz",
      fluidRow(
        box(width = 12,
            title = strong("How to Use the Data Visualization Tab"),
            collapsible = TRUE,
            includeMarkdown("text/how-to-data-viz.md")
        )
      ),
      # Pool-level data visualizations
      fluidRow(
        box(width = 12,
            title = strong("Pool-Level Visualizations"),
            fluidRow(
              column(width = 6,
                     selectizeInput("p_viz_location_pool_id", "Select Location-Pool ID:",
                                    choices = sort(unique(c(hydro$location_pool_id, percent_cover$location_pool_id))),
                                    multiple = FALSE)
              ),
              column(width = 6,
                     selectizeInput("p_viz_water_year", "Select Water Year:",
                                    choices = unique(hydro$water_year),
                                    multiple = FALSE)
              ),
              column(width = 6,
                     plotlyOutput("water_level_viz")
              ),
              column(width = 6,
                     plotlyOutput("spp_abundance_viz")
              )
            )
        )
      ),
      # Transect-level data visualizations
      fluidRow(
        box(width = 8,
            title = strong("Transect-Level Visualizations"),
            fluidRow(
              column(width = 6,
                     selectizeInput("tr_viz_location_pool_id", "Select Location-Pool ID:",
                                    choices = sort(unique(c(hydro$location_pool_id, percent_cover$location_pool_id))),
                                    multiple = FALSE)
              ),
              column(width = 6,
                     selectizeInput("tr_viz_quadrat", "Select Transect:",
                                    choices = sort(unique(percent_cover$vernal_pool_axis)),
                                    multiple = FALSE)
              ),
              column(width = 6,
                     selectizeInput("tr_viz_type", "Select Metric:",
                                    choices = c("Sum of Native Cover",
                                                "Count of Native Species", 
                                                "Sum of Non-Native Cover",
                                                "Count of Non-Native Species",
                                                "Percent Thatch",
                                                "Percent Bare Ground",
                                                "Percent Cover Single Species"),
                                    multiple = FALSE)
              ),
              column(width = 6,
                     conditionalPanel(
                       condition = "input.tr_viz_type == 'Percent Cover Single Species'",
                       selectizeInput("tr_viz_species", "Select Species:",
                                      choices = NULL,
                                      multiple = FALSE)
                     )
              ),
              column(width = 12,
                     plotlyOutput("transect_level_viz")
              )
            )
        ),
        box(width = 4,
            height = 612,
            title = strong("Additional Information"),
            includeMarkdown("text/additional-info.md"),
            conditionalPanel("input.tr_viz_type == 'Percent Cover Single Species'",
                             imageOutput("tr_spp_image"),
                             uiOutput("tr_spp_text")
            )
        ), # END Additional info box
        
        box(width = 12,
            title = strong("Species Lists"),
            selectizeInput("list_location_pool_id",
                           choices = sort(unique(percent_cover$location_pool_id)),
                           label = "Select Location-Pool ID:"),
            collapsed = FALSE,
            collapsible = TRUE,
            column(width = 6, 
                   h4("Plant Species", style = "margin-bottom: 35px;"),
                   dataTableOutput("plant_spp_list")),
            column(width = 6,
                   h4("Invertebrate Species", style = "margin-bottom: 35px;"),
                   dataTableOutput("invert_spp_list"))
            
          
        ) # END species list box
      )
    )
  )
)

#================================================================
#                        Dashboard Page
#================================================================
dashboardPage(header, sidebar, body,
              fresh::use_theme("shinydashboard-fresh-theme.css"))