server <- function(input, output, session) {
  
  # ==================================================================================
  #                                Vernal Pool Map                                ----
  # ==================================================================================
  
  # Colors for research status
  vernal_map_data <- vernal_polygon_abiotic %>% 
    mutate(research_conducted_status = as.factor(research_conducted_status))
  factpal <- colorFactor(c("blue", "orange"), vernal_map_data$research_conducted_status)
  
  
  # Reactive value for link
  selected_pool <- reactiveVal(NULL)
  
  
  # Leaflet output
  output$map <- renderLeaflet({
    leaflet(vernal_map_data) %>%
      addProviderTiles(providers$CartoDB) %>%
      setView(lng = -119.8489, lat = 34.4140, zoom = 13) %>%
      
      #add clustering and labels
      addMarkers(data = centroids,
                 clusterOptions = markerClusterOptions(), # this ENABLES the marker plugin
                 popup = ~sprintf(
                   
                   paste0(
                     "Location-Pool ID: %s<br>",
                     "Research Status: %s<br>",
                     "Site Area (m²): %.2f<br>",
                     "Pool Area (m²): %.2f<br>",
                     "Pool Circumference (ft): %.2f<br>",
                     "Pool Edge Ratio: %.2f<br>",
                     "Restoration Year: %s<br>",
                     "Time Since: %s<br>",
                     "Period: %s<br>",
                     "Depth (cm): %.2f<br>",
                     "<a href='#' onclick='Shiny.setInputValue(\"goto_data\", \"%s\"); return false;'>",
                     "View Additional Data </a>"),
                   
                   location_pool_id,
                   research_conducted_status,
                   round(site_area_m2, 2),
                   round(pool_area_m2, 2),
                   round(pool_circumference_ft, 2),
                   round(pool_edge_ratio, 2),
                   restoration_year,
                   time_since,
                   period,
                   round(depth_cm, 2),
                   location_pool_id) # END sprintf for pop-up
                 
      ) %>%
      
      # add pool polygons/geometries
      addPolygons(stroke = FALSE,
                  color = ~factpal(research_conducted_status),
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.7,
                  layerId = ~location_pool_id) %>% 
      
      # add legend
      addLegend(position = 'bottomright',
                pal = factpal,
                values = c('Active Monitoring', "Non-Active Monitoring"),
                labels = c('Active Monitoring', "Non-Active Monitoring"))
    
  }) #END renderLeaflet 
  
  
  # ==================================================================================
  #                               Connect Map & Data Viz                          ----
  # ==================================================================================
  
  # Link/filter for data visualization (map connection)
  observeEvent(input$goto_data, {
    selected_pool(input$goto_data)  # Store the selected pool ID
    
    # Update all 3 inputs
    updateSelectInput(session, 
                      "p_viz_location_pool_id", 
                      selected = input$goto_data)
    
    updateSelectInput(session, 
                      "tr_viz_location_pool_id", 
                      selected = input$goto_data)
    
    updateSelectInput(session,
                      "list_location_pool_id",
                      selected = input$goto_data)
    
    # Switch to data viz tab
    updateTabItems(session, "sidebarMenu", selected = "dataviz")
    
  })
  
  # ==================================================================================
  #                               Pool Level Visualizations                       ----
  # ==================================================================================
  
  
  ## ................................ Water Level Plot ..........................
  
  output$water_level_viz <- renderPlotly({  
    
    p <- hydro %>%
      filter(location_pool_id == input$p_viz_location_pool_id & 
               water_year == input$p_viz_water_year) %>%
      ggplot(aes(x = date, y = water_level_in)) +
      geom_line(col = "dodgerblue", size = 1) +
      geom_point(col = "dodgerblue") +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%m-%d") + 
      labs(x = "Date", 
           y = "Water Level (in)", 
           title = "Weekly Water Level") +
      theme_classic() +
      theme(plot.title = element_text(size = 16, hjust = 0.5)) 
    
    ggplotly(p)
    
  }) #END water level plot renderPlotly
  
  
  ## ........................... Species Abundance Plot .........................
  
  output$spp_abundance_viz <- renderPlotly({
    
    # filter to remove NAs & select location_pool_id 
    species_abundance <- percent_cover %>%
      filter(location_pool_id == input$p_viz_location_pool_id & complete.cases(species)) %>% 
      filter(species != "unlisted") %>% 
      group_by(species, type) %>%
      summarise(percent_cover = sum(percent_cover, na.rm = TRUE)/100) %>% 
      filter(type %in% c("Non-Native", "Native"))
    
    
    # store species abundance plot
    p <- species_abundance %>%
      ggplot(aes(reorder(species, percent_cover), percent_cover, fill = type)) +
      geom_col() +
      #geom_text(aes(label = percent_cover), size = 3, hjust = -0.2) +
      scale_fill_manual(values = c("Native" = "#6B8E23", "Non-Native" = "#D2691E")) +
      labs(y = "Relative Abundance", 
           x = NULL, 
           title = "Vegetation Abundance by Species", 
           fill = "Type") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, hjust = 0.5),
            axis.text.y = element_text(size = 9)) +
      #expand_limits(y = max(species_abundance$percent_cover) + 5) +
      coord_flip()
    
    ggplotly(p)
    
  }) # END species abundance plot
  
  
  # ==================================================================================
  #                             Quadrat Level Visualizations                     ----
  # ==================================================================================
  
  # filter species available depending on input&tr_location_pool_id
  observe({
    
    filtered_species <- percent_cover %>% 
      filter(location_pool_id == input$tr_viz_location_pool_id &
               vernal_pool_axis == input$tr_viz_quadrat)
    
    filtered_species <- sort(unique(filtered_species$species))
    
    updateSelectizeInput(session, "tr_viz_species",
                         choices = filtered_species)
  }) #END observe for single species input
  
  
  
  output$transect_level_viz <- renderPlotly({
    
    y_var <- switch(input$tr_viz_type,
                    "Sum of Native Cover" = "sum_of_native_cover",
                    "Count of Native Species" = "count_of_native_species",
                    "Sum of Non-Native Cover" = "sum_of_non_native_cover",
                    "Count of Non-Native Species" = "count_of_non_native_species",
                    "Percent Thatch" = "percent_natural_thatch",
                    "Percent Bare Ground" = "percent_bare_ground",
                    "Percent Cover Single Species" = "percent_cover")
    
    
    if (y_var == "percent_cover") {
      
      # Use reactive data frame for species filtering
      df <- percent_cover %>%
        filter(location_pool_id == input$tr_viz_location_pool_id &
                 vernal_pool_axis == input$tr_viz_quadrat &
                 species == input$tr_viz_species) %>% 
      
      # use custom function to add 0s
      fill_gaps()

      tr_p <- ggplot(df, aes_string("transect_distance_of_quadrat", y_var)) +
        geom_line(alpha = 0.3, col = "blue") +
        geom_point(data = subset(df, get(y_var) > 0)) +
        theme_minimal() +
        labs(x = "Transect Distance of Quadrat",
             y = input$viz_plot_type)
      
    } else {
      
      df <- percent_cover %>%
        filter(location_pool_id == input$tr_viz_location_pool_id &
                 vernal_pool_axis == input$tr_viz_quadrat)
      
      tr_p <- ggplot(df, aes_string("transect_distance_of_quadrat", y_var)) +
        geom_line(alpha = 0.3, col = "blue") +
        geom_point() +
        theme_minimal() +
        labs(x = "Transect Distance of Quadrat",
             y = input$tr_viz_type)
      
      
    } # END ifelse for transect-level plot (single species)
    
    
    ggplotly(tr_p)
    
    
  }) # END renderPlotly transect-level viz
  
  
  # Species link output ----
  
  output$tr_spp_text <- renderUI({
    
    spp_name <- as.character(input$tr_viz_species)
    
    df_single_spp <- percent_cover %>%
      filter(location_pool_id == input$tr_viz_location_pool_id &
               vernal_pool_axis == input$tr_viz_quadrat &
               species == input$tr_viz_species)
    
    spp_link <- unique(df_single_spp$spp_cal_flora)
    
    HTML(paste0('<div style="text-align: center;">',
                '<strong>Calflora Link: </strong> ',
                '<a href="', spp_link, '" target="_blank">', spp_name, '</a><br>',
                '</div>'))
    
  }) #END renderUI for Calflora page
  
  
  # Species image output ----
  
  output$tr_spp_image <- renderImage({
    spp_name <- as.character(input$tr_viz_species)
    spp_name <- gsub(" ", "_", spp_name)
    
    # Specify the path to the image file
    image_path <- here("images", paste0(spp_name, ".jpg"))
    
    # Return a list with the image path and other attributes
    list(src = image_path,
         alt = spp_name,
         contentType = 'image/jpeg',
         width = "80%",
         height = "95%",
         style="display: block; margin-left: auto; margin-right: auto;")
  }, deleteFile = FALSE)
  
  # ==================================================================================
  #                            Invert & Plant Species List                        ----
  # ==================================================================================
  
  # plant species list
  output$plant_spp_list <- renderDataTable({
    percent_cover %>% 
      filter(location_pool_id == input$list_location_pool_id) %>% 
      select(species, type) %>% 
      distinct(species, .keep_all = TRUE) %>% 
      arrange(species) %>% 
      rename(Species = species,
             Type = type)
    
  }) # END renderDataTable for plant list
  
  
  # invert species list
  output$invert_spp_list <- renderDataTable({
    invert_species %>% 
      filter(location_pool_id == input$list_location_pool_id) %>% 
      select(genus, species) %>% 
      filter(!is.na(genus)) %>% # remove NA values
      filter(!str_detect(genus, "\\?")) %>% # remove rows with question marks in genus
      distinct(genus, species) %>% 
      arrange(genus) %>% 
      rename(Genus = genus,
             Species = species)
    
  }) # END renderDataTable for invert list
  
  
  
} # END Server function