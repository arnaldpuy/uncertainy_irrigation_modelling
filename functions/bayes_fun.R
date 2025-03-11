

# BAYESIAN MODEL ###############################################################

bayes_fun <- function(dt, extended = FALSE) {
  
  # Choose model formula based on 'extended' parameter -------------------------
  formula <- if (extended) {
    estimation ~ model + climate + scenario + socio.conditions + (1 | year)
  } else {
    estimation ~ model + climate + (1 | year)
  }
  
  # Bayesian model -------------------------------------------------------------
  
  model_bayes <- brm(formula,
                     data = dt,
                     family = gaussian(),
                     chains = 4, 
                     iter = 4000,
                     warmup = 1000,
                     cores = getOption("mc.cores", detectCores() - 1))
  
  # Extract variance components ------------------------------------------------
  
  var_comp <- as.data.frame(VarCorr(model_bayes))
  residual_var <- var_comp$residual__.sd.Estimate^2
  random_var <- var_comp$year.sd.Estimate^2
  
  # Extract fixed effect coefficients ------------------------------------------
  
  fixed_effects <- as.data.frame(fixef(model_bayes))
  fixed_effects$Variable <- rownames(fixed_effects)
  
  # Function to calculate variance for a given term ----------------------------
  
  calculate_variance <- function(term) {
    rows <- fixed_effects[grepl(paste0("^", term), fixed_effects$Variable), ]
    if (nrow(rows) > 0) sum(rows$Estimate^2) else 0
  }
  
  # Variance calculations ------------------------------------------------------
  
  model_var <- calculate_variance("model")
  climate_var <- calculate_variance("climate")
  scenario_var <- if (extended) calculate_variance("scenario") else 0
  socio_var <- if (extended) calculate_variance("socio.conditions") else 0
  
  # Total variance -------------------------------------------------------------
  
  total_var <- model_var + climate_var + scenario_var + socio_var + random_var + residual_var
  
  # Proportion calculations ----------------------------------------------------
  
  proportion <- function(var) var / total_var
  model_proportion <- proportion(model_var)
  climate_proportion <- proportion(climate_var)
  scenario_proportion <- proportion(scenario_var)
  socio_proportion <- proportion(socio_var)
  random_effect_proportion <- proportion(random_var)
  residual_proportion <- proportion(residual_var)
  
  # Output ---------------------------------------------------------------------
  
  output <- if (extended) {
    data.table(
      climate_variance = climate_proportion,
      model_variance = model_proportion,
      scenario_variance = scenario_proportion,
      socio_conditions_variance = socio_proportion,
      random_variance = random_effect_proportion,
      residual_variance = residual_proportion
    )
  } else {
    data.table(
      climate_variance = climate_proportion,
      model_variance = model_proportion,
      random_variance = random_effect_proportion,
      residual_variance = residual_proportion
    )
  }
  
  return(output)
}
