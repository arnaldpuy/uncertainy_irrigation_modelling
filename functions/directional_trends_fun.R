
# FUNCTION TO ANALYZE TREND BETWEEN UNCERTAINTIES AND NUMBER OF MODELS,
# STUDIES AND ESTIMATES ########################################################

directional_trends_fun <- function(dt, dataset_id) {
  
  if (nrow(dt) < 2) return(NULL)  # Need at least 2 rows to compute deltas
  
  setorder(dt, period_midpoint)
  
  dt[, `:=`(
    delta_uncertainty = uncertainty - shift(uncertainty),
    delta_studies = unique.studies - shift(unique.studies),
    delta_estimates = number.estimates - shift(number.estimates),
    delta_models = unique.models - shift(unique.models)
  )]
  
  # Remove first row (it has NA deltas)
  dt <- dt[!is.na(delta_uncertainty)]
  
  out <- data.table(
    dataset = dataset_id,
    n_deltas = nrow(dt),
    studies = mean(sign(dt$delta_uncertainty) == sign(dt$delta_studies), na.rm = TRUE) * 100,
    estimates = mean(sign(dt$delta_uncertainty) == sign(dt$delta_estimates), na.rm = TRUE) * 100,
    models = mean(sign(dt$delta_uncertainty) == sign(dt$delta_models), na.rm = TRUE) * 100
  )
  
  return(out)
}