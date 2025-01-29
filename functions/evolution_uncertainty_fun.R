
# FUNCTION TO CREATE PERIODS ###################################################

create_periods <- function(start_year, end_year, interval) {
  
  breaks <- seq(start_year, end_year, by = interval)
  labels <- paste(
    head(breaks, -1),
    tail(breaks, -1) - 1,
    sep = "–")
  
  list(breaks = breaks, labels = labels)
  
}

# FUNCTION TO CALCULATE EVOLUTION OF UNCERTAINTY ###############################

evolution_uncertainty_fun <- function(data, target_year) {
  
  # Filter data for the target estimation year ---------------------------------
  
  df_filtered <- data %>%
    .[variable == "iww" & region == "global" & estimation.year == target_year]
  
  # Define publication and target estimation periods ---------------------------
  
  publication_periods <- create_periods(1990, 2030, 10) # Publication periods: 10-year intervals
  
  # Add the publication_period column ------------------------------------------
  
  df_filtered <- copy(df_filtered) 
  
  # Add publication_period and target_period columns ---------------------------
  
  df_filtered[, publication_period := cut(publication.date,
                                          breaks = publication_periods$breaks,
                                          labels = publication_periods$labels,
                                          right = FALSE)]
  
  # Summarize uncertainty for each combination of publication and target periods
  
  df_summary <- df_filtered[, .(mean_estimation = mean(value, na.rm = TRUE),
                                sd_estimation = sd(value, na.rm = TRUE),
                                iqr_estimation = IQR(value, na.rm = TRUE),
                                range_estimation = max(value, na.rm = TRUE) - min(value, na.rm = TRUE),
                                study_count = uniqueN(title), # Count unique studies based on title
                                
                                # Normalized metrics
                                norm_sd_estimation = sd(value, na.rm = TRUE) / uniqueN(title),
                                norm_iqr_estimation = IQR(value, na.rm = TRUE) / uniqueN(title),
                                norm_range_estimation = (max(value, na.rm = TRUE) - 
                                                           min(value, na.rm = TRUE)) / uniqueN(title)),
                            publication_period]
  
  # Plot variability -----------------------------------------------------------
  
  output <- df_summary %>%
    .[!is.na(publication_period)] %>%
    ggplot(., aes(x = publication_period, y = norm_range_estimation)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(x = "Publication Period", y = "Normalized range of estimates") +
    theme_AP() +
    ggtitle(paste("Variability of estimates for", target_year))
  
  return(output)
  
}