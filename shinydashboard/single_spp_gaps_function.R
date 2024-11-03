fill_gaps <- function(df) {
  
  # create sequence of values from min to max transect distance (by 1)
  all_distances <- min(df$transect_distance_of_quadrat):max(df$transect_distance_of_quadrat)
  
  # create df for all distances, percent_cover = 0 for all distances
  result <- data.frame(
    transect_distance_of_quadrat = all_distances,
    percent_cover = 0)
  
  # replace 0s with true percent_cover from original data
  result$percent_cover[result$transect_distance_of_quadrat %in% df$transect_distance_of_quadrat] <- 
    df$percent_cover
  
  return(result)
}