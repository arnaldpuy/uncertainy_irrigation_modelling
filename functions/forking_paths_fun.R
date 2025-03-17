
# FORKING PATHS FUNCTIONS ######################################################

# Function to create periods ---------------------------------------------------

create_periods_fun <- function(start_year, end_year, interval, rolling_window_factor) {
  
  if (!rolling_window_factor %in% c(1, 0.5)) {
    stop("rolling_window_factor must be either 1 (fixed intervals) or 0.5 (50% overlap).")
  }
  
  # Define step size based on rolling window factor ----------------------------
  step <- interval * rolling_window_factor  # Ensures 50% overlap if rolling_window_factor = 0.5
  
  # Generate start years of periods --------------------------------------------
  start_dates <- seq(start_year, end_year - interval + 1, by = step)
  
  # Define end years of periods -----------------------------------------------
  end_dates <- start_dates + interval  # `cut()` needs right-open intervals
  
  # Construct breaks: Must include both first and last breakpoints ------------
  breaks <- unique(c(start_dates, max(end_dates)))  # Ensures proper binning
  
  # Generate labels (one fewer than breaks) ------------------------------------
  labels <- paste(start_dates, end_dates - 1, sep = "–")  # Left-closed, right-open format
  
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
    
    return("Increase")
    
  } else if (all(diffs < 0)) {
    
    return("Decrease")
    
  } else {
    
    return("Unstable")
  }
}

# FUNCTION TO EXPLORE FORKING PATHS ############################################

forking_paths_fun <- function(dt, target_year, interval, metric, 
                              rolling_window_factor, target_year_interval) {
  
  target_year_original <- target_year
  
  # Define target year and target year interval --------------------------------
  
  if (target_year_interval == "yes") {
    
    if (target_year == 2000) {
      
      target_year <- target_year:(target_year + 10)
      
      
    } else if (target_year == 2100) {
      
      target_year <- (target_year - 10):target_year
      
    } else {
      
      target_year <- (target_year - 10):(target_year + 10)
      
    }
    
    target_year_original <- paste(min(target_year), "-", max(target_year), sep = "")
    
  }
  
  # Filter based on target_year ------------------------------------------------
  
  df_filtered <- dt[estimation.year %in% target_year] 
  
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
  
  unique.studies <- df_filtered[, .(unique.studies = uniqueN(title)), publication_period]
  unique.models <- df_filtered[, .(unique.models = uniqueN(model)), publication_period]
  number.estimates <- df_filtered[, .(number.estimates = .N), publication_period]
  
  dt <- df_filtered[, .(uncertainty = calculate_uncertainty_fun(.SD, metric = metric)), 
                    .(period_midpoint, publication_period)] %>%
    na.omit() %>%
    merge(., unique.studies, by = "publication_period") %>%
    merge(., unique.models, by = "publication_period") %>%
    merge(., number.estimates, by = "publication_period") %>%
    setorder(., period_midpoint)
  
  # Remove those periods for which there is no spread (only one estimate) ------
  
  dt <- dt[!number.estimates == 1]
  
  # Check order of the points --------------------------------------------------
  
  output <- check_order_fun(dt$uncertainty)
  
  # Draw plots for each simulation ---------------------------------------------
  
  plot <- dt[, publication_period:= gsub("–", "-", publication_period)] %>%
    .[, publication_period := gsub("2029$", "2025", publication_period)] %>%
    ggplot(., aes(publication_period, uncertainty)) +
    geom_point(color = "red") +
    geom_line(color = "red", group = 1) +
    labs(y = metric, x = "") +
    scale_y_continuous(breaks = breaks_pretty(n = 2)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    theme_AP() +
    theme(axis.text.x = element_text(size = 4.5), 
          axis.text.y = element_text(size = 6.3),
          axis.title.y = element_text(size = 6.5),
          plot.margin = unit(c(0.05, 0.05, 0, 0.05), "cm")) + 
    annotate("text", x = 0.1 + 0.5, y = max(dt$uncertainty), 
             label = target_year_original, hjust = 0, vjust = 1, 
             size = 2)
  
  out <- list(output, df_filtered, dt, plot)
  
  names(out) <- c("results", "data", "data_aggregated", "plot")
  
  return(out)
}