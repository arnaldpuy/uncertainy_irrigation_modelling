
# LINEAR MIXED EFFECTS MODEL ###################################################

lmm_fun <- function(dt) {
  
  # LMM model ------------------------------------------------------------------
  
  model_lmm <- lmer(estimation ~ model + climate + (1 | year), data = dt)
  
  anova_res <- anova(model_lmm)
  
  # Extract variance components ------------------------------------------------
  
  var_comp <- as.data.frame(VarCorr(model_lmm))
  residual_var <- attr(VarCorr(model_lmm), "sc")^2  # Residual variance
  random_var <- var_comp$vcov[var_comp$grp == "year"]  # Year random effect variance
  
  # Calculate total variance ---------------------------------------------------
  
  fixed_effects_var <- sum(anova_res$`Sum Sq`)  # Sum of squares for all fixed effects
  total_var <- fixed_effects_var + random_var + residual_var
  
  # Calculate proportion of variance explained ---------------------------------
  
  climate_var <- anova_res["climate", "Sum Sq"] / total_var  # Proportion due to climate
  model_var <- anova_res["model", "Sum Sq"] / total_var  # Proportion due to model
  residual_proportion <- residual_var / total_var  # Proportion of residual variance
  random_effect_proportion <- random_var / total_var  # Proportion of variance due to random effect (year)
  
  # Return results -------------------------------------------------------------
  
  output <- data.table(
    climate_variance = climate_var,
    model_variance = model_var,
    random_variance = random_effect_proportion,
    residual_variance = residual_proportion
  )
  
  return(output)
  
}