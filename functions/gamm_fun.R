
# GENERAL ADDITIVE MIXED MODEL #################################################


gamm_fun <- function(dt) {
  
  # Fit GAM model with random effects ------------------------------------------
  
  model_gamm <- gam(estimation ~ model + climate + s(year, bs = "re"), 
                    data = dt, method = "REML")
  
  # Extract fixed effect coefficients ------------------------------------------
  
  fixed_effects <- as.data.frame(summary(model_gamm)$p.table)
  fixed_effects$Variable <- rownames(fixed_effects)
  
  # Variance for model ---------------------------------------------------------
  
  model_rows <- fixed_effects[grepl("^model", fixed_effects$Variable), ]
  model_var <- sum(model_rows$Estimate^2, na.rm = TRUE)
  
  # Variance for climate -------------------------------------------------------
  
  climate_rows <- fixed_effects[grepl("^climate", fixed_effects$Variable), ]
  climate_var <- sum(climate_rows$Estimate^2, na.rm = TRUE)
  
  # Extract random effect variance --------------------------------------------
  
  random_effects <- summary(model_gamm)$s.table
  
  random_var <- if ("s(year)" %in% rownames(random_effects)) {
    
    random_effects["s(year)", "edf"]
    
  } else {
    
    0
  }
  
  # Residual variance ----------------------------------------------------------
  
  residual_var <- var(model_gamm$residuals, na.rm = TRUE)
  
  # Calculate total variance ---------------------------------------------------
  
  total_var <- model_var + climate_var + random_var + residual_var
  
  # Calculate proportions ------------------------------------------------------
  
  model_proportion <- model_var / total_var
  climate_proportion <- climate_var / total_var
  random_effect_proportion <- random_var / total_var
  residual_proportion <- residual_var / total_var
   
  # Output ---------------------------------------------------------------------
  
  output <- data.table(
    climate_variance = climate_proportion,
    model_variance = model_proportion,
    random_variance = random_effect_proportion,
    residual_variance = residual_proportion
  )
  
  return(output)
}
