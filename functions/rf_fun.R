

# RANDOM FOREST MODEL ##########################################################

rf_fun <- function(dt) {
  
  # Random forest model --------------------------------------------------------
  
  rf_model <- randomForest(estimation ~ model + climate + year,
                           data = dt, importance = TRUE)
  
  # Extract variable importance ------------------------------------------------
  
  var_importance <- importance(rf_model)
  climate_var <- var_importance["climate", "IncNodePurity"]
  model_var <- var_importance["model", "IncNodePurity"]
  year_var <- var_importance["year", "IncNodePurity"]
  
  # Total variance -------------------------------------------------------------
  
  total_var <- sum(var_importance)
  
  # Output ---------------------------------------------------------------------
  
  output <- data.table(climate_variance = climate_var / total_var,
                       model_variance = model_var / total_var,
                       random_variance = year_var / total_var,
                       residual_variance = NA)
  
  return(output)
}