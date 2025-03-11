
# LINEAR MIXED EFFECTS MODEL ###################################################

lmm_fun <- function(dt, extended = FALSE) {
  
  # Choose model formula based on the 'extended' parameter ---------------------
  if (extended) {
    # Extended formula with scenario and socio.conditions
    model_lmm <- lmer(estimation ~ model + climate + scenario + socio.conditions + (1 | year), data = dt)
  } else {
    # Original formula without scenario and socio.conditions
    model_lmm <- lmer(estimation ~ model + climate + (1 | year), data = dt)
  }
  
  # ANOVA results for fixed effects -------------------------------------------
  
  anova_res <- anova(model_lmm)
  
  # Extract variance components ------------------------------------------------
  
  var_comp <- as.data.frame(VarCorr(model_lmm))
  residual_var <- attr(VarCorr(model_lmm), "sc")^2  # Residual variance
  random_var <- var_comp$vcov[var_comp$grp == "year"]  # Year random effect variance
  
  # Calculate total variance ---------------------------------------------------
  
  fixed_effects_var <- sum(anova_res$`Sum Sq`, na.rm = TRUE)  # Sum of squares for all fixed effects
  total_var <- fixed_effects_var + random_var + residual_var
  
  # Calculate proportion of variance explained ---------------------------------
  
  # Variance for model
  model_var <- if ("model" %in% rownames(anova_res)) {
    anova_res["model", "Sum Sq"] / total_var
  } else {
    0
  }
  
  # Variance for climate
  climate_var <- if ("climate" %in% rownames(anova_res)) {
    anova_res["climate", "Sum Sq"] / total_var
  } else {
    0
  }
  
  # Variance for scenario (only if extended = TRUE)
  scenario_var <- if (extended && "scenario" %in% rownames(anova_res)) {
    anova_res["scenario", "Sum Sq"] / total_var
  } else {
    0
  }
  
  # Variance for socio.conditions (only if extended = TRUE)
  socio_conditions_var <- if (extended && "socio.conditions" %in% rownames(anova_res)) {
    anova_res["socio.conditions", "Sum Sq"] / total_var
  } else {
    0
  }
  
  # Proportion of variance due to residuals
  residual_proportion <- residual_var / total_var
  
  # Proportion of variance due to random effect (year)
  random_effect_proportion <- random_var / total_var
  
  # Return results -------------------------------------------------------------
  
  if (extended) {
    output <- data.table(
      climate_variance = climate_var,
      model_variance = model_var,
      scenario_variance = scenario_var, 
      socio_conditions_variance = socio_conditions_var,
      random_variance = random_effect_proportion,
      residual_variance = residual_proportion
    )
  } else {
    output <- data.table(
      climate_variance = climate_var,
      model_variance = model_var,
      random_variance = random_effect_proportion,
      residual_variance = residual_proportion
    )
  }
  
  return(output)
}