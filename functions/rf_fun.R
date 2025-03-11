

# RANDOM FOREST MODEL ##########################################################

rf_fun <- function(dt, extended = FALSE) {
  
  # Choose model formula based on the 'extended' parameter ---------------------
  if (extended) {
    # Extended formula with scenario and socio.conditions
    rf_model <- randomForest(estimation ~ model + climate + scenario + socio.conditions + year,
                             data = dt, importance = TRUE)
  } else {
    # Original formula without scenario and socio.conditions
    rf_model <- randomForest(estimation ~ model + climate + year,
                             data = dt, importance = TRUE)
  }
  
  # Extract variable importance ------------------------------------------------
  
  var_importance <- importance(rf_model)
  
  # Variance for model ---------------------------------------------------------
  model_var <- if ("model" %in% rownames(var_importance)) {
    var_importance["model", "IncNodePurity"]
  } else {
    0
  }
  
  # Variance for climate -------------------------------------------------------
  climate_var <- if ("climate" %in% rownames(var_importance)) {
    var_importance["climate", "IncNodePurity"]
  } else {
    0
  }
  
  # Variance for scenario (only if extended = TRUE) -----------------------------
  scenario_var <- if (extended && "scenario" %in% rownames(var_importance)) {
    var_importance["scenario", "IncNodePurity"]
  } else {
    0
  }
  
  # Variance for socio.conditions (only if extended = TRUE) ---------------------
  socio_conditions_var <- if (extended && "socio.conditions" %in% rownames(var_importance)) {
    var_importance["socio.conditions", "IncNodePurity"]
  } else {
    0
  }
  
  # Variance for year (random effect proxy) ------------------------------------
  year_var <- if ("year" %in% rownames(var_importance)) {
    var_importance["year", "IncNodePurity"]
  } else {
    0
  }
  
  # Total variance -------------------------------------------------------------
  total_var <- sum(var_importance, na.rm = TRUE)
  
  # Output ---------------------------------------------------------------------
  if (extended) {
    output <- data.table(
      climate_variance = climate_var / total_var,
      model_variance = model_var / total_var,
      scenario_variance = scenario_var / total_var,
      socio_conditions_variance = socio_conditions_var / total_var,
      random_variance = year_var / total_var,
      residual_variance = NA  # Residual variance is not applicable for RF
    )
  } else {
    output <- data.table(
      climate_variance = climate_var / total_var,
      model_variance = model_var / total_var,
      random_variance = year_var / total_var,
      residual_variance = NA  # Residual variance is not applicable for RF
    )
  }
  
  return(output)
}