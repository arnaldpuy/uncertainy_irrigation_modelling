## ----setup, include=FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "pdf", cache = TRUE)


## ----warning=FALSE, message=FALSE----------------------------------------------------------------------------------

#   PRELIMINARY FUNCTIONS ######################################################

sensobol::load_packages(c("openxlsx", "data.table", "tidyverse","cowplot", 
                          "benchmarkme", "parallel", "wesanderson", "scales", "ncdf4", 
                          "countrycode", "rworldmap", "sp", "doParallel", "here", "lme4", 
                          "microbenchmark", "mgcv", "brms", "randomForest", "here", 
                          "igraph", "ggraph", "gganimate", "magick", 
                          "randomForestExplainer", "ggrepel"))

# Create custom theme -----------------------------------------------------------

theme_AP <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent",
                                           color = NA),
          legend.key = element_rect(fill = "transparent",
                                    color = NA), 
          strip.background = element_rect(fill = "white"), 
          legend.text = element_text(size = 7.3), 
          axis.title = element_text(size = 10),
          legend.key.width = unit(0.4, "cm"), 
          legend.key.height = unit(0.4, "cm"), 
          legend.key.spacing.y = unit(0, "lines"),
          legend.box.spacing = unit(0, "pt"),
          legend.title = element_text(size = 7.3), 
          axis.text.x = element_text(size = 7), 
          axis.text.y = element_text(size = 7), 
          axis.title.x = element_text(size = 7.3), 
          axis.title.y = element_text(size = 7.3),
          plot.title = element_text(size = 8),
          strip.text.x = element_text(size = 7.4), 
          strip.text.y = element_text(size = 7.4)) 
}

# Select color palette ----------------------------------------------------------

selected.palette <- "Darjeeling1"


## ----source_functions, warning=FALSE, message=FALSE, results="hide"------------------------------------------------

# SOURCE ALL R FUNCTIONS NEEDED FOR THE STUDY ##################################

# Source all .R files in the "functions" folder --------------------------------

r_functions <- list.files(path = here("functions"), pattern = "\\.R$", full.names = TRUE)
lapply(r_functions, source)



## ----isimip_data---------------------------------------------------------------------------------------------------

# RETRIEVE DATA FROM ISIMIP ####################################################

# Create vector with list of files ---------------------------------------------

list.of.files <- list.files("./files/isimip")
model.names <- sub("^(.*?)_.*", "\\1", list.of.files)
climate.scenarios <- sapply(strsplit(list.of.files, "_"), function(x) x[2])
social.scenarios <- sapply(strsplit(list.of.files, "_"), function(x) x[which(x == "co2") - 1])
files.directory <- paste("./files/isimip", list.of.files, sep = "/")
start_year <- 1971

# Create parallel cluster -------------------------------------------------------

numCores <- detectCores() * 0.75
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Run for loop -----------------------------------------------------------------

isimip.hist <- foreach(i = 1:length(files.directory),
                       .packages = c("data.table", "countrycode", "tidyverse",
                                "sp", "rworldmap", "ncdf4")) %dopar% {
                                  
                                  get_isimip_fun(nc_file = files.directory[i], 
                                                 variable = "airrww", 
                                                 start_year = start_year)
                                }

# Stop the cluster after the computation ---------------------------------------

stopCluster(cl)


## ----arrange_isimip_data, dependson="isimip_data"------------------------------------------------------------------

# ARRANGE DATA ################################################################

# Number of files --------------------------------------------------------------

list.of.files

# Name the slots ---------------------------------------------------------------

names(isimip.hist) <- paste(model.names, climate.scenarios, social.scenarios, sep = "/")

# Clean and bind dataset -------------------------------------------------------

isimip.dt <- rbindlist(isimip.hist, idcol = "model") %>%
  na.omit() %>%
  .[, model:= factor(model)] %>%
  .[, c("model", "climate", "social"):= tstrsplit(model, "/")]

fwrite(isimip.dt, "isimip.dt.csv")

# Pressoc: constant human impacts in the form of dams and reservoirs
# varsoc: variable human impacts.


## ----plot_isimip_dt_continent, dependson="arrange_isimip_data", fig.height=3.2-------------------------------------

# PLOT ISIMIP ##################################################################

# Continental level ------------------------------------------------------------

isimip.dt[, sum(V1, na.rm = TRUE), .(Continent, model, year, climate, social)] %>%
  ggplot(., aes(year, V1, group = interaction(climate, model), color = model, 
                linetype = climate)) +
  facet_wrap(social~Continent, scales = "free_y", ncol = 5) +
  geom_line() + 
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  labs(x = "Year", y = bquote("IWW (km"^3 * ")"))  +
  theme_AP() +
  guides(color = guide_legend(nrow = 2)) +
  guides(linetype = guide_legend(nrow = 2)) +
  theme(legend.position = "top")


## ----plot_isimip_dt_global, dependson="arrange_isimip_data", fig.height=2.2, fig.width=3.7-------------------------

# Global level -----------------------------------------------------------------

isimip.dt[, sum(V1, na.rm = TRUE), .(year, model, climate, social)] %>%
  ggplot(., aes(year, V1, group = interaction(climate, model), color = model)) +
  geom_line() + 
  facet_wrap(~social) +
  labs(x = "Year", y = bquote("IWW (km"^3 * ")"))  +
  theme_AP() +
  theme(legend.position = "top")


## ----isimip_data_future--------------------------------------------------------------------------------------------

# RETRIEVE PROJECTIONS FROM ISIMIP #############################################

# Create vector with list of files ---------------------------------------------

path.projections <- "./files/isimip_future"
list.of.files.projections <- list.files(path.projections)
files.directory.projections <- paste(path.projections, list.of.files.projections, sep = "/")
variable <- "airrww"
start_year <- 2006

# Create parallel cluster -------------------------------------------------------

numCores <- detectCores() * 0.75
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Run for loop -----------------------------------------------------------------

isimip.future <- foreach(i = 1:length(files.directory.projections),
                       .packages = c("data.table", "countrycode", "tidyverse",
                                     "sp", "rworldmap", "ncdf4")) %dopar% {
                                       
                                       get_isimip_fun(nc_file = files.directory.projections[i], 
                                                      variable = variable, 
                                                      start_year = start_year)
                                     }

# Stop the cluster after the computation ---------------------------------------

stopCluster(cl)


## ----arrange_isimip_dt_future, dependson="isimip_data_future"------------------------------------------------------

# ARRANGE DATA #################################################################

# Number of files --------------------------------------------------------------

list.of.files.projections

# Arrange names ----------------------------------------------------------------

model.names <- sub("^(.*?)_.*", "\\1", list.of.files.projections)
pattern <- "ewembi_(.*?)soc"
climate <- sub(".*ewembi_(.*?)soc.*", "\\1", list.of.files.projections)
names(isimip.future) <- paste(model.names, climate, sep = "/")

# Clean and bind dataset -------------------------------------------------------

isimip.future.dt <- rbindlist(isimip.future, idcol = "model") %>%
  na.omit() %>%
  .[, model:= factor(model)] %>%
  .[, year:= as.numeric(year)] 

isimip.future.dt[, c("model", "climate") := tstrsplit(model, "/")]

# Export -----------------------------------------------------------------------

fwrite(isimip.future.dt, "isimip.future.dt.csv")


## ----plot_isimip_dt_future, dependson="arrange_isimip_dt_future"---------------------------------------------------

# PLOT ISIMIP ##################################################################

# Continental level ------------------------------------------------------------

isimip.future.dt[, sum(V1, na.rm = TRUE), .(year, Continent, model, climate)] %>%
  .[, climate:= gsub("_", "\\\\_", climate)] %>%
  ggplot(., aes(year, V1, group = climate, color = climate)) +
  facet_wrap(model~Continent, scales = "free_y", ncol = 5) +
  geom_line() +
  labs(x = "Year", y = bquote("IWW (km"^3 * ")"))  +
  scale_y_continuous(breaks = breaks_pretty(n = 3)) +
  theme_AP() +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  theme(legend.position = "top")


## ----plot_isimip_dt_future_merged, dependson="arrange_isimip_dt_future", fig.height=4------------------------------

# PLOT ISIMIP MERGED ###########################################################

a <- isimip.future.dt[, sum(V1, na.rm = TRUE), .(year, Continent, model, climate)] %>%
  ggplot(., aes(year, V1, group = interaction(climate, model), color = model)) +
  facet_wrap(~Continent, scales = "free_y", ncol = 5) +
  geom_line() + 
  scale_color_manual(name = "", values = wes_palette(name = selected.palette)) +
  labs(x = "Year", y = bquote("IWW (km"^3 * ")"))  +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  theme_AP() +
  theme(legend.position = "top")

b <- isimip.future.dt[, sum(V1, na.rm = TRUE), .(year, Continent, model, climate)] %>%
  ggplot(., aes(year, V1, group = interaction(climate, model), color = climate)) +
  facet_wrap(~Continent, scales = "free_y", ncol = 5) +
  geom_line() + 
  labs(x = "Year", y = bquote("IWW (km"^3 * ")"))  +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  theme_AP() +
  theme(legend.position = "top") +
  guides(colour = guide_legend(nrow = 1))

plot_grid(a, b, ncol = 1, labels = "auto")


## ----anova_isimip, dependson=c("arrange_isimip_data", "arrange_isimip_dt_future")----------------------------------

# ANOVA #########################################################################

# Arrange ISIMIP datasets ------------------------------------------------------

isimip.full <- isimip.dt[social == "varsoc"][, context:= "historic"] %>%
  rbind(., isimip.future.dt[, context:= "prediction"], fill = TRUE) %>%
  .[, social:= NULL] 

isimip.anova <- isimip.full[, .(estimation = sum(V1)), 
                            .(Continent, climate, context, model, year)]

# ARRANGE DATA #################################################################

columns_to_factor <- c("Continent", "climate", "model")
isimip.full[, (columns_to_factor):= lapply(.SD, as.factor), .SDcols = (columns_to_factor)]
isimip.anova[, (columns_to_factor):= lapply(.SD, as.factor), .SDcols = (columns_to_factor)]

# RUN MODEL AND ANALYSIS OF VARIANCE ###########################################

# List of models ---------------------------------------------------------------

functions <- list(lmm = lmm_fun,
                  gamm = gamm_fun,
                  rf = rf_fun, 
                  bayes = bayes_fun)

# Apply each function to the data and combine results ---------------------------

results <- mclapply(names(functions), function(fun_name) {
  
  isimip.anova[, functions[[fun_name]](.SD), .(Continent, context)]
  
}, 
mc.cores = detectCores() * 0.75)


## ----plot_anova, dependson="anova_isimip", fig.height=3.2, fig.width=6---------------------------------------------

# PLOT RESULTS ##################################################################

results 

results.dt <- rbindlist(results)

a <- isimip.full[, .(estimation = sum(V1)), .(model, Continent, climate, year, context)] %>%
  ggplot(., aes(year, estimation, color = model, group = interaction(climate, model))) +
  geom_line() +
  facet_wrap(context~Continent, scale = "free", ncol = 5) +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  theme_AP() +
  guides(colour = guide_legend(nrow = 2)) +
  labs(x = "Year", y = bquote("IWW (km"^3 * ")"))  +
  theme(legend.position = "top", 
        legend.box.spacing = unit(0, "pt"))

b <- results.dt %>%
  melt(., measure.vars = c("climate_variance", "model_variance", "random_variance", 
                           "residual_variance")) %>%
  .[, .(min = min(value, na.rm = TRUE), 
        max = max(value, na.rm = TRUE)), .(Continent, context, variable)] %>%
  .[, variance:= tstrsplit(variable, "_", fixed = TRUE)[[1]]] %>%
  ggplot(., aes(x = Continent, ymin = min, ymax = max, y = (min + max) / 2, color = variance)) +
  geom_errorbar(width = 0.2) +
  geom_point(size = 1) +
  scale_color_manual(name = "", values=wes_palette(selected.palette, n = 4)) +
  labs(x = "", y = "Fraction variance") +
  facet_wrap(~context, ncol = 1) +
  theme(legend.position = "top") +
  scale_y_continuous(breaks = breaks_pretty(n = 3)) +
  theme_AP() +
  theme(legend.position = "top") + 
  guides(color = guide_legend(nrow = 2)) +
  theme(legend.position = "top") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
   
plot_grid(a, b, ncol = 2, labels = "auto", rel_widths = c(0.73, 0.27))


## ----check_combinations, dependson="anova_isimip", fig.height=2.3, fig.width=2.7-----------------------------------

# COUNT COMBINATIONS OF MODEL AND CLIMATE #######################################

unique(isimip.full[, .(model, climate, context)]) %>%
  ggplot(., aes(x = model, y = climate, fill = context)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_manual(values = c("historic" = "steelblue", "prediction" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Model", y = "Climate", fill = "Context") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_AP() +
  theme(legend.position = "top")


## ----khan_data, cache.lazy=FALSE, eval = FALSE---------------------------------------------------------------------
# 
# # KHAN ET AL 2023 DATASET ######################################################
# 
# path.projections <- "./files/khan_et_al_2023"
# list.of.files <- list.files(path.projections, pattern = "\\.csv$")
# combinations <- lapply(list.of.files, function(x) strsplit(x, "_")[[1]][1:4]) %>%
#   do.call(rbind, .) %>%
#   data.frame()
# colnames(combinations) <- c("SSP", "RCP", "Climate", "Use")
# 
# # READ FILES IN PARALLEL #######################################################
# 
# # Create parallel cluste -------------------------------------------------------
# 
# numCores <- detectCores() * 0.75
# cl <- makeCluster(numCores)
# registerDoParallel(cl)
# 
# # Run for loop -----------------------------------------------------------------
# 
# result <- foreach(i = 1:length(list.of.files),
#                   .combine = "rbind",
#                   .packages = c("data.table", "countrycode",
#                                 "sp", "rworldmap")) %dopar% {
# 
#                                   out <- fread(paste("./files/khan_et_al_2023/", list.of.files[i], sep = "/"))
#                                   out[, `:=`(SSP = combinations[i, 1],
#                                              RCP = combinations[i, 2],
#                                              Climate = combinations[i, 3],
#                                              Use = combinations[i, 4])]
# 
#                                   Country <- coords2country(out[1:nrow(out), 2:3])
# 
#                                   df <- cbind(Country, out)
# 
#                                   df[, Continent := countrycode(Country, origin = "country.name", destination = "continent")]
# 
#                                   df[, Dataset := list.of.files[i]]
# 
#                                   df
#                                 }
# 
# # Stop the cluster after the computation ---------------------------------------
# 
# stopCluster(cl)


## ----arrange_khan_data, dependson="khan_data", cache.lazy=FALSE, eval = FALSE--------------------------------------
# 
# # ARRANGE DATA #################################################################
# 
# numeric_cols <- grep("^[0-9]+$", names(result), value = TRUE)
# khan.dt <- melt(result, measure.vars = numeric_cols, variable.name = "Year") %>%
#   .[, Year:= as.numeric(as.character(Year))] %>%
#   .[, model:= "GCAM"] %>%
#   na.omit()
# 
# # EXPORT DATA ###################################################################
# 
# khan.dt.continent <- khan.dt[, .(estimation = sum(value)),
#                              .(Year, Continent, Use, RCP, SSP, Climate, Dataset, model)] %>%
#   .[, climate:= paste(Climate, RCP, SSP, sep = "_")]
# 
# fwrite(khan.dt.continent, "khan.dt.continent.csv")


## ----plot_khan_continental, dependson="arrange_khan_data", fig.height=2.3, fig.width=4, eval=FALSE-----------------
# 
# # PLOT #########################################################################
# 
# # Continental ------------------------------------------------------------------
# 
# plot.khan.continental <- khan.dt.continent %>%
#   ggplot(., aes(Year, estimation, color = Continent, group = interaction(Dataset, Continent))) +
#   geom_line(alpha = 0.3) +
#   facet_wrap(~Use) +
#   theme_AP() +
#   theme(legend.position = "top") +
#   labs(x = "", y = bquote("km"^3))
# 
# plot.khan.continental


## ----plot_khan_global, dependson="arrange_khan_data", fig.height=2.3, fig.width=4, eval = FALSE--------------------
# 
# # PLOT #########################################################################
# 
# # Global -----------------------------------------------------------------------
# 
# plot.khan.global <- khan.dt[, sum(value), .(Year, Use, Dataset)] %>%
#   ggplot(., aes(Year, V1, group = Dataset)) +
#   geom_line(alpha = 0.3) +
#   facet_wrap(~Use) +
#   theme_AP() +
#   theme(legend.position = "top") +
#   labs(x = "Year", y = bquote("km"^3))
# 
# plot.khan.global


## ----plot_khan_merged, dependson=c("plot_khan_continental", "plot_khan_global"), fig.height=3.5, fig.width=4, eval = FALSE----
# 
# # MERGE KHAN ET AL DATASETS ####################################################
# 
# plot_grid(plot.khan.continental, plot.khan.global, ncol = 1, labels = "auto",
#           rel_heights = c(0.53, 0.47))
# 


## ----plot_khan_ssp_rcp, dependson="arrange_khan_data", eval = FALSE------------------------------------------------
# 
# # PLOT SSPS VS RCPS ############################################################
# 
# khan.dt[, sum(value), .(Year, Use, Dataset, RCP, SSP)] %>%
#   ggplot(., aes(Year, V1, group = Dataset, color = Use)) +
#   geom_line() +
#   facet_grid(RCP~SSP) +
#   theme_AP() +
#   theme(legend.position = "top") +
#   labs(x = "Year", y = bquote("km"^3))
# 


## ----merge_khan_isimip, dependson="anova_isimip", fig.height=1.7, fig.width=6.5------------------------------------

# MERGE KHAN ET AL DATA WITH ISIMIP ############################################

# Arrange data -----------------------------------------------------------------

khan.dt.continent <- fread("khan.dt.continent.csv")

khan.dt2 <- khan.dt.continent[Use == "withdrawals", .(model, Continent, climate, Year, estimation)] %>%
  setnames(., "Year", "year")

# Extract prediction data from ISIMIP ------------------------------------------

isimip.full2 <- isimip.full[context == "prediction" & year >= 2010, 
            .(estimation = sum(V1)), .(model, Continent, climate, year, context)] %>%
  .[, context:= NULL]

# Merge and plot ---------------------------------------------------------------

merged.dt <- rbind(khan.dt2, isimip.full2) 

ggplot(merged.dt, aes(year, estimation, group = interaction(climate, model), color = model)) +
  geom_line(alpha = 0.4) + 
  facet_wrap(~Continent, scale = "free_y", ncol = 5) +
  theme_AP() +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  theme(legend.position = "top") +
  labs(x = "Year", y = bquote("km"^3))

# Calculate the min and max in 2030-2050 given uncertainty and the global level -----

merged.dt[year %in% c(2030, 2040, 2050), 
          .(min = min(estimation), max = max(estimation)), .(Continent, year)] %>%
  .[, .(sum_min = sum(min), sum_max = sum(max)), year]


## ----session_information-------------------------------------------------------------------------------------------

# SESSION INFORMATION ##########################################################

sessionInfo()

## Return the machine CPU ------------------------------------------------------

cat("Machine:     "); print(get_cpu()$model_name)

## Return number of true cores -------------------------------------------------

cat("Num cores:   "); print(detectCores(logical = FALSE))

## Return number of threads ---------------------------------------------------

cat("Num threads: "); print(detectCores(logical = FALSE))

