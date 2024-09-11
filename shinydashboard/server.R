server <- function(input, output, session) {
  
  # ==================================================================================
  #                                Vernal Pool Map                                ----
  # ==================================================================================
  
  # Define the reactive value
  selected_polygon <- reactiveVal(NULL)
  
  # Define color palette
  status_colors <- c("Active Monitoring" = "blue", 
                     "Non-Active Monitoring" = "orange", 
                     "Unknown" = "gray")
  
  legend_colors <- c("blue", "orange")
  legend_labels <- c("Active Monitoring", "Non-Active Monitoring")
  
  # Create a color mapping function with debugging
  colorMapping <- function(status) {
    colors <- sapply(status, function(s) {
      color <- status_colors[s]
      if (is.null(color) || is.na(color)) {
        print(paste("Unexpected status:", s))
        return("gray")  # Default color for unexpected status
      }
      return(color)
    })
    print("Unique statuses:")
    print(unique(status))
    print("Mapped colors:")
    print(unique(colors))
    return(colors)
  } # END colorMapping function
  
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
    
    print("Unique fill_color values:")
    print(unique(vernal_polygon_abiotic$fill_color))
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -119.8489, lat = 34.4140, zoom = 15) %>%
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
                                  "Edge Distance Max: ", round(edge_distance_max, 2), "<br>",
                                  "Restoration Year: ", restoration_year, "<br>",
                                  "Time Since: ", time_since, "<br>",
                                  "Period: ", period, "<br>",
                                  "Depth (cm): ", round(depth_cm, 2))
      ) %>%
      addLegend(position = "bottomright",
                colors = legend_colors,
                labels = legend_labels,
                title = "Research Status",
                opacity = 1)
  }) # END renderLeaflet for pool map
  
  # Reactive function for filtered data
  filtered_data <- reactive({
    print("Filtering data...")
    print(paste("Pool size:", input$location_pool_size_select))
    print(paste("Month:", paste(input$month_select, collapse = ", ")))
    print(paste("Year:", paste(input$year_select, collapse = ", ")))
    print(paste("Location-Pool ID:", paste(input$location_pool_id_select, collapse = ", ")))
    
    data <- vernal_polygon_abiotic
    
    # Filter by pool size
    if (!is.null(input$location_pool_size_select) && input$location_pool_size_select != "All") {
      selected_interval <- which(jenks_labels == input$location_pool_size_select)
      lower_bound <- jenks_breaks[selected_interval]
      upper_bound <- jenks_breaks[selected_interval + 1]
      data <- data[data$pool_area_m2 >= lower_bound & data$pool_area_m2 < upper_bound, ]
    }
    
    # Filter by month
    if (!is.null(input$month_select) && length(input$month_select) > 0) {
      data <- data[data$month %in% input$month_select, ]
    }
    
    # Filter by year
    if (!is.null(input$year_select) && length(input$year_select) > 0) {
      print("Filtering by year...")
      print(paste("Years selected:", paste(input$year_select, collapse = ", ")))
      print(paste("Unique years in data before filtering:", paste(unique(data$year), collapse = ", ")))
      
      data <- data[data$year %in% input$year_select, ]
      
      print(paste("Rows after year filtering:", nrow(data)))
      print(paste("Unique years in data after filtering:", paste(unique(data$year), collapse = ", ")))
    }
    
    # Filter by location_pool_id
    if (!is.null(input$location_pool_id_select) && length(input$location_pool_id_select) > 0) {
      data <- data[data$location_pool_id %in% input$location_pool_id_select, ]
    }
    
    print(paste("Filtered data rows:", nrow(data)))
    data
  }) # END reactive filtered data
  
  # Update the map
  observe({
    filtered <- filtered_data()
    
    print("Updating map...")
    print(paste("Number of polygons to display:", nrow(filtered)))
    print("Unique research_conducted_status values in filtered data:")
    print(unique(filtered$research_conducted_status))
    
    # Assign colors based on status, handling NA values
    filtered$fill_color <- sapply(filtered$research_conducted_status, function(status) {
      if (is.na(status)) {
        return(status_colors["Unknown"])
      } else if (status %in% names(status_colors)) {
        return(status_colors[status])
      } else {
        return(status_colors["Unknown"])
      }
    })
    
    print("Unique fill_color values in filtered data:")
    print(unique(filtered$fill_color))
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = filtered,
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
                                  "Edge Distance Max: ", round(edge_distance_max, 2), "<br>",
                                  "Restoration Year: ", restoration_year, "<br>",
                                  "Time Since: ", time_since, "<br>",
                                  "Period: ", period, "<br>",
                                  "Depth (cm): ", round(depth_cm, 2))
      )
  })
  
  # ==================================================================================
  #                               Pool Level Visualizations                       ----
  # ==================================================================================
  
  output$pool_level_viz <- renderPlotly({
    
    
    ## ................................ Water Level Plot ..........................
    if (input$p_viz_type == "Water Level") {
      
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
        theme_classic()
      
      
      ## ........................... Species Abundance Plot .........................
    } else {
      
      # filter to remove NAs & select location_pool_id 
      species_abundance <- percent_cover %>%
        filter(location_pool_id == input$p_viz_location_pool_id & complete.cases(species) & 
                 species != "unlisted") %>%
        group_by(species, type) %>%
        summarise(percent_cover = sum(percent_cover, na.rm = TRUE)) %>% 
        filter(type %in% c("Non-Native", "Native"))
      
      # store species abundance plot
      p <- species_abundance %>%
        ggplot(aes(reorder(species, percent_cover), percent_cover, fill = type)) +
        geom_col() +
        #geom_text(aes(label = percent_cover), size = 3, hjust = -0.2) +
        scale_fill_manual(values = c("Native" = "#6B8E23", "Non-Native" = "#D2691E")) +
        labs(y = "Percent Cover", 
             x = "Species", 
             title = "Vegetation Abundance by Species", 
             fill = "Type") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, hjust = 0.5, vjust = 1)) +
        #expand_limits(y = max(species_abundance$percent_cover) + 5) +
        coord_flip()  
      
    }
    
    ## ........................... Output Selected Plot ...........................
    
    ggplotly(p)
    
  }) # END renderPlotly plot-level viz
  
  
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
        geom_line(alpha = 0.4, col = "blue") +
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
        geom_line(alpha = 0.4, col = "blue") +
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