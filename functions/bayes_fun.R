

# BAYESIAN MODEL ###############################################################

bayes_fun <- function(dt) {
  
  # Bayesian model -------------------------------------------------------------
  
  model_bayes <- brm(estimation ~ model + climate + (1 | year),
                     data = dt,
                     family = gaussian(),
                     chains = 4, 
                     iter = 10^3, 
                     cores = getOption("mc.cores", detectCores()-1))
  
  # Extract variance components ------------------------------------------------
  
  var_comp <- as.data.frame(VarCorr(model_bayes))
  residual_var <- var_comp$vcov[var_comp$grp == "Residual"]
  random_var <- var_comp$vcov[var_comp$grp == "year"]
  
  # Extract fixed effect coefficients ------------------------------------------
  
  fixed_effects <- as.data.frame(fixef(model_bayes))
  fixed_effects$Variable <- rownames(fixed_effects)
  
  # Variance for model ---------------------------------------------------------
  
  model_rows <- fixed_effects[grepl("^model", fixed_effects$Variable), ]
  model_var <- sum(model_rows$Estimate^2)
  
  # Variance for climate -------------------------------------------------------
  
  climate_rows <- fixed_effects[grepl("^climate", fixed_effects$Variable), ]
  climate_var <- sum(climate_rows$Estimate^2)
  
  # Extract random effect variance for year ------------------------------------
  
  var_comp <- as.data.frame(VarCorr(model_bayes))
  random_var <- var_comp$year.sd.Estimate^2  # Square the standard deviation for random effect
  
  # Extract residual variance --------------------------------------------------
  
  residual_var <- var_comp$residual__.sd.Estimate^2 
  
  # Calculate total variance ---------------------------------------------------
  
  total_var <- model_var + climate_var + random_var + residual_var
  
  # Calculate proportions ------------------------------------------------------
  
  model_proportion <- model_var / total_var
  climate_proportion <- climate_var / total_var
  random_effect_proportion <- random_var / total_var
  residual_proportion <- residual_var / total_var
  
  # Output ---------------------------------------------------------------------
  
  output <- data.table(climate_variance = climate_proportion,
                       model_variance = model_proportion,
                       random_variance = random_effect_proportion,
                       residual_variance = residual_proportion)
  
  return(output)
}