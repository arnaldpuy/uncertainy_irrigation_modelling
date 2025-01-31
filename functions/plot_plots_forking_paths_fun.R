
# FUNCTION TO PLOT THE PLOTS OF THE FORKING PATHS ##############################

plot_plots_forking_paths_fun <- function(simulation) {
  
  out <- plots.dt[[simulation]] +
    theme_AP() + 
    scale_x_continuous(breaks = breaks_pretty(n = 3)) +
    labs(x = "Publication year", y = " + Uncertainty") +
    ggtitle(paste("Target year:", final.dt[simulation, "target_year"])) +
    theme(axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(),
          plot.title = element_text(size = 7.3), 
          plot.margin = unit(c(0, 0.8, 0, -0.4), "cm")) 
  
  return(out)
}