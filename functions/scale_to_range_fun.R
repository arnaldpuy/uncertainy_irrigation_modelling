
# FUNCTION TO SCALE UNCERTAINTY TO RANGE OF ESTIMATES ##########################

scale_to_range_fun <- function(data,column, ref_data, ref_column) {
  
  # Extract the column values to be scaled--------------------------------------
  
  x <- data[[column]]
  
  # Determine new_min and new_max based on the reference column ----------------
  
  new_min <- min(ref_data[[ref_column]], na.rm = TRUE)
  new_max <- max(ref_data[[ref_column]], na.rm = TRUE)
  
  # Scale the column values ----------------------------------------------------
  
  scaled_values <- (x - min(x)) / (max(x) - min(x)) * (new_max - new_min) + new_min
  
  return(scaled_values)
}