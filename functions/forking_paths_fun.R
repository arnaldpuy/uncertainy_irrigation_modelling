
# FORKING PATHS FUNCTIONS ######################################################

# Function to create periods ---------------------------------------------------

create_periods_fun <- function(start_year, end_year, interval, rolling_window_factor) {
  
  if (!rolling_window_factor %in% c(1, 0.5)) {
    stop("rolling_window_factor must be either 1 (fixed intervals) or 0.5 (50% overlap).")
  }
  
  # Define step size based on rolling window factor ----------------------------
  
  step <- round(interval * rolling_window_factor)  # Round to avoid decimals
  
  # Generate breaks based on step size -----------------------------------------
  
  breaks <- seq(start_year, end_year, by = step)
  breaks <- unique(round(breaks))  # Ensure unique integer values
  
  # Ensure 2025 is included in breaks ------------------------------------------
  
  if (2025 > max(breaks)) {
    breaks <- unique(c(breaks, 2025))  # Ensure breaks remain unique
  }
  
  # Define end year for each period based on the "interval" factor -------------
  
  end_dates <- breaks[-1] - 1  # The last break is NOT an interval start
  start_dates <- breaks[-length(breaks)]  # Remove last break from starts
  
  # Generate labels (ensure they match number of intervals) --------------------
  
  labels <- paste(start_dates, end_dates, sep = "–")
  
  return(list(breaks = breaks, labels = labels))
}


# Function to calculate entropy ------------------------------------------------

calculate_entropy_fun <- function(values) {
  
  if (length(values) == 1) return(0)  # If only one value, entropy is zero
  value_counts <- table(values) / length(values)  # Probability distribution
  entropy <- -sum(value_counts * log2(value_counts))  # Shannon entropy
  return(entropy)
}

# Function to account for metric uncertainty -----------------------------------

calculate_uncertainty_fun <- function(data, metric) {
  
  metric <- as.character(metric)
  
  # Basic statistics -----------------------------------------------------------
  
  mean_value <- mean(data$value, na.rm = TRUE)
  sd_value <- sd(data$value, na.rm = TRUE)
  range_value <- max(data$value, na.rm = TRUE) - min(data$value, na.rm = TRUE)
  iqr_value = IQR(data$value, na.rm = TRUE)
  var_value = var(data$value, na.rm = TRUE)
  mad_value = mad(data$value, na.rm = TRUE)
  entropy_value = calculate_entropy_fun(data$value)
  study_count <- uniqueN(data$title)  
  
  # Calculate the desired uncertainty metric -----------------------------------
  
  result <- switch(metric, 
                   "cv" = (sd_value / mean_value) * 100,
                   "range" = range_value,
                   "sd" = sd_value,
                   "iqr" = iqr_value,
                   "var" = var_value,
                   "mad" = mad_value,
                   "entropy" = entropy_value,
                   "cv_normalized" = ((sd_value / mean_value) * 100) / study_count,
                   "range_normalized" = range_value / study_count,
                   "mad_normalized" = mad_value / study_count, 
                   "sd_normalized" = sd_value / study_count,
                   "iqr_normalized" = iqr_value / study_count, 
                   "var_normalized" = var_value / study_count,
                   "entropy_normalized" = entropy_value / study_count)
  
  return(result)
}

# Function to check the trend of points ----------------------------------------

check_order_fun <- function(data) {
  
  # Check if the data has only one row or all values are the same --------------
  
  if (length(data) == 1 || length(unique(data)) == 1) {
    
    return("single point")
  }
  
  # Calculate differences between points ---------------------------------------
  
  diffs <- diff(data)
  
  # Check the trend based on differences ---------------------------------------
  
  if (all(diffs > 0)) {
    
    return("Ascending")
    
  } else if (all(diffs < 0)) {
    
    return("Descending")
    
  } else {
    
    return("Random")
  }
}

# FUNCTION TO EXPLORE FORKING PATHS ############################################

forking_paths_fun <- function(dt, target_year, interval, exclude_before_1990, 
                              metric, rolling_window_factor, target_year_interval) {
  
  # Define target year and target year interval --------------------------------
  
  if (target_year_interval == "yes") {
    
    if (target_year == 2000) {
      
      target_year <- c(target_year, target_year + 10)
      
    } else if (target_year == 2100) {
      
      target_year <- c(target_year - 10, target_year)
      
    } else {
      
      target_year <- c(target_year - 10, target_year + 10)
      
    }
    
  }
  
  # Filter based on target_year ------------------------------------------------
  
  df_filtered <- dt[estimation.year %in% target_year] 
  
  # Apply inclusion criteria fork ----------------------------------------------
  
  if (exclude_before_1990 == "yes") {
    
    df_filtered <- df_filtered[publication.date >= 1990]
  }
  
  # Create periods -------------------------------------------------------------
  
  publication_periods <- create_periods_fun(1970, 2030, interval = interval, 
                                            rolling_window_factor = rolling_window_factor)
  
  df_filtered[, publication_period := cut(publication.date,
                                          breaks = publication_periods$breaks,
                                          labels = publication_periods$labels,
                                          right = FALSE)]
  
  df_filtered[, publication_period := as.character(publication_period)]
  
  # Extract midpoints from publication_period ----------------------------------
  
  df_filtered[, period_midpoint := sapply(publication_period, function(period) {
    years <- as.numeric(unlist(strsplit(period, "–")))
    mean(years)
  })]
  
  unique.studies <- df_filtered[, uniqueN(title), publication_period]
  
  dt <- df_filtered[, .(uncertainty = calculate_uncertainty_fun(.SD, metric = metric)), 
                    .(period_midpoint, publication_period)] %>%
    na.omit() %>%
    merge(., unique.studies, by = "publication_period") %>%
    setorder(., period_midpoint)
  
  # Check order of the points --------------------------------------------------
  
  output <- check_order_fun(dt$uncertainty)
  
  # Draw plots for each simulation ---------------------------------------------
  
  plot <- ggplot(dt, aes(period_midpoint, uncertainty)) +
    geom_point() 
  
  out <- list(output, plot)
  
  names(out) <- c("results", "plot")
  
  return(out)
}