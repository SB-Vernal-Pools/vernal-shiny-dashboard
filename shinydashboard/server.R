server <- function(input, output, session) {
  # ==================================================================================
  #                                species list                             ----
  # ==================================================================================
  
  output$species_list <- renderDataTable({
    percent_cover %>% 
      select(species) %>% 
      distinct() %>%
      arrange(species) %>%
      rename("Species" = species)
  },
  options = list(
    searching = TRUE,
    lengthChange = FALSE,
    pageLength = 10))
  
  
  
  
  
  # ==================================================================================
  #                                Vernal Pool Map                                ----
  # ==================================================================================
  
  
  #-------------------- legend options -------------------------
  # Define color palette
  status_colors <- c("Active Monitoring" = "blue", 
                     "Non-Active Monitoring" = "orange", 
                     "Unknown" = "gray")
  
  legend_colors <- c("blue", "orange")
  legend_labels <- c("Active Monitoring", "Non-Active Monitoring")
  
  output$map <- renderLeaflet({
    print("Rendering initial map")
    print("Unique research_conducted_status values:")
    print(unique(vernal_polygon_abiotic$research_conducted_status))
    
    # Assign colors based on status, handling NA values
    vernal_polygon_abiotic$fill_color <- sapply(vernal_polygon_abiotic$research_conducted_status, function(status) {
      if (is.na(status)) {
        return(status_colors["Unknown"])
      } else if (status %in% names(status_colors)) {
        return(status_colors[status])
      } else {
        return(status_colors["Unknown"])
      }
    })

    
  #------------------ Leaflet Map ----------------------------
    
    # setting up map basemap and view
    leaflet() %>%
      addProviderTiles(providers$CartoDB) %>%
      setView(lng = -119.8489, lat = 34.4140, zoom = 13) %>%
      
      #clustering of data
      addMarkers(data = centroids,
                 clusterOptions = markerClusterOptions(), # this ENABLES the marker plugin
                 popup = ~paste0("Location-Pool ID: ", location_pool_id, "<br>",
                                  "Research Status: ", ifelse(is.na(research_conducted_status), "Unknown", research_conducted_status), "<br>",
                                  "Site Area (m²): ", round(site_area_m2, 2), "<br>",
                                  "Pool Area (m²): ", round(pool_area_m2, 2), "<br>",
                                  "Pool Circumference (ft): ", round(pool_circumference_ft, 2), "<br>",
                                  "Pool Edge Ratio: ", round(pool_edge_ratio, 2), "<br>",
                                  "Restoration Year: ", restoration_year, "<br>",
                                  "Time Since: ", time_since, "<br>",
                                  "Period: ", period, "<br>",
                                  "Depth (cm): ", round(depth_cm, 2))) %>%
     
      #polygon edits and popup
      addPolygons(data = vernal_polygon_abiotic,
                  fillColor = ~fill_color,
                  color = "black",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.7,
                  layerId = ~location_pool_id,
                  popup = ~paste0("Location-Pool ID: ", location_pool_id, "<br>",
                                  "Research Status: ", ifelse(is.na(research_conducted_status), "Unknown", research_conducted_status), "<br>",
                                  "Site Area (m²): ", round(site_area_m2, 2), "<br>",
                                  "Pool Area (m²): ", round(pool_area_m2, 2), "<br>",
                                  "Pool Circumference (ft): ", round(pool_circumference_ft, 2), "<br>",
                                  "Pool Edge Ratio: ", round(pool_edge_ratio, 2), "<br>",
                                  "Restoration Year: ", restoration_year, "<br>",
                                  "Time Since: ", time_since, "<br>",
                                  "Period: ", period, "<br>",
                                  "Depth (cm): ", round(depth_cm, 2))
      ) %>%
      
      #legend addition
      addLegend(position = "bottomright",
                colors = legend_colors,
                labels = legend_labels,
                title = "Research Status",
                opacity = 1)
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
      
  }) # END species abundance plot
  
  
  # ==================================================================================
  #                             Quadrat Level Visualizations                     ----
  # ==================================================================================
  
  # filter species available depending on input&tr_location_pool_id
  observe({
    
    filtered_species <- percent_cover %>% 
      filter(location_pool_id == input$tr_viz_location_pool_id &
               transect_axis == input$tr_viz_quadrat)
    
    filtered_species <- sort(unique(filtered_species$species))
    
    updateSelectizeInput(session, "tr_viz_species",
                         choices = filtered_species)
  }) #END observe for single species input
  
  
  
  output$transect_level_viz <- renderPlotly({
    
    y_var <- switch(input$tr_viz_type,
                    "Sum of Native Cover" = "sum_of_native_cover_automatically_calculated",
                    "Count of Native Species" = "count_of_native_species_automatically_calculated",
                    "Sum of Non-Native Cover" = "sum_of_non_native_cover_automatically_calculated",
                    "Count of Non-Native Species" = "count_of_non_native_species_automatically_calculated",
                    "Percent Thatch" = "percent_thatch",
                    "Percent Bare Ground" = "percent_bare_ground",
                    "Percent Cover Single Species" = "percent_cover")
    
    
    if (y_var == "percent_cover") {
      
      # Use reactive data frame for species filtering
      df <- percent_cover %>%
        filter(location_pool_id == input$tr_viz_location_pool_id &
                 transect_axis == input$tr_viz_quadrat &
                 species == input$tr_viz_species)
      
      tr_p <- ggplot(df, aes_string("transect_distance_of_quadrat", y_var)) +
        # geom_smooth(se=FALSE) +
        geom_line(alpha = 0.3, col = "blue") +
        geom_point() +
        theme_minimal() +
        labs(x = "Transect Distance of Quadrat",
             y = input$viz_plot_type)
      
    } else {
      
      df <- percent_cover %>%
        filter(location_pool_id == input$tr_viz_location_pool_id &
                 transect_axis == input$tr_viz_quadrat)
      
      tr_p <- ggplot(df, aes_string("transect_distance_of_quadrat", y_var)) +
        # geom_smooth(se=FALSE) +
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
               transect_axis == input$tr_viz_quadrat &
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
  
  
}