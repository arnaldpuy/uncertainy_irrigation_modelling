
#   PRELIMINARY FUNCTIONS ######################################################

sensobol::load_packages(c("openxlsx", "data.table", "tidyverse","cowplot", 
                          "benchmarkme", "parallel", "wesanderson", "scales", "ncdf4", 
                          "countrycode", "rworldmap", "sp", "doParallel", "here", "lme4"))

# Create custom theme
theme_AP <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent",
                                           color = NA),
          legend.key = element_rect(fill = "transparent",
                                    color = NA), 
          strip.background = element_rect(fill = "white"), 
          legend.margin = margin(0.5, 0.1, 0.1, 0.1),
          legend.box.margin = margin(0.2,-4,-7,-7), 
          plot.margin = margin(3, 4, 0, 4), 
          legend.text = element_text(size = 8), 
          axis.title = element_text(size = 10),
          legend.key.width = unit(0.4, "cm"), 
          legend.key.height = unit(0.4, "cm"), 
          legend.title = element_text(size = 9), 
          axis.text.x = element_text(size = 7), 
          axis.text.y = element_text(size = 7), 
          axis.title.x = element_text(size = 7.3), 
          axis.title.y = element_text(size = 7.3), 
          strip.text.x = element_text(size = 7.4)) 
}

selected.palette <- "Darjeeling1"

# SOURCE ALL R FUNCTIONS NEEDED FOR THE STUDY ##################################

# Source all .R files in the "functions" folder ------------------------------------

r_functions <- list.files(path = here("functions"), pattern = "\\.R$", 
                          full.names = TRUE)
lapply(r_functions, source)

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

# Name the slots ---------------------------------------------------------------

names(isimip.hist) <- paste(model.names, climate.scenarios, social.scenarios, sep = "/")

# Clean and bind dataset -------------------------------------------------------

isimip.dt <- rbindlist(isimip.hist, idcol = "model") %>%
  na.omit() %>%
  .[, model:= factor(model)] %>%
  .[, c("model", "climate", "social"):= tstrsplit(model, "/")] %>%
  .[, V1:= ifelse(model == "matsiro", V1 / 100, V1)]

fwrite(isimip.dt, "isimip.dt.csv")

# Pressoc: constant human impacts in the form of dams and reservoirs
# varsoc: variable human impacts.

# PLOT ISIMIP ##################################################################

# Continental level ------------------------------------------------------------

isimip.dt[, sum(V1, na.rm = TRUE), .(Continent, model, year, climate, social)] %>%
  ggplot(., aes(year, V1, group = interaction(climate, model), color = model, 
                linetype = climate)) +
  facet_wrap(social~Continent, scales = "free_y", ncol = 5) +
  geom_line() + 
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  labs(x = "", y = "IWW (km$^3$)") +
  theme_AP() +
  theme(legend.position = "top")

# Global level -----------------------------------------------------------------

isimip.dt[, sum(V1, na.rm = TRUE), .(year, model, climate, social)] %>%
  ggplot(., aes(year, V1, group = interaction(climate, model), color = model)) +
  geom_line() + 
  facet_wrap(~social) +
  labs(x = "", y = "IWW (km$^3$)") +
  theme_AP() +
  theme(legend.position = "top")

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

# ARRANGE DATA #################################################################

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

# Export
fwrite(isimip.future.dt, "isimip.future.dt.csv")

# PLOT ISIMIP ##################################################################

# Continental level ------------------------------------------------------------

isimip.future.dt[, sum(V1, na.rm = TRUE), .(year, Continent, model, climate)] %>%
  .[, climate:= gsub("_", "\\\\_", climate)] %>%
  ggplot(., aes(year, V1, group = climate, color = climate)) +
  facet_wrap(model~Continent, scales = "free_y", ncol = 5) +
  geom_line() + 
  labs(x = "", y = "IWW (km$^3$)") +
  theme_AP() +
  theme(legend.position = "top")

a <- isimip.future.dt[, sum(V1, na.rm = TRUE), .(year, Continent, model, climate)] %>%
  ggplot(., aes(year, V1, group = interaction(climate, model), color = model)) +
  facet_wrap(~Continent, scales = "free_y", ncol = 5) +
  geom_line() + 
  scale_color_manual(name = "", values = wes_palette(name = selected.palette)) +
  labs(x = "", y = "IWW (km$^3$)") +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  theme_AP() +
  theme(legend.position = "top")

b <- isimip.future.dt[, sum(V1, na.rm = TRUE), .(year, Continent, model, climate)] %>%
  ggplot(., aes(year, V1, group = interaction(climate, model), color = climate)) +
  facet_wrap(~Continent, scales = "free_y", ncol = 5) +
  geom_line() + 
  labs(x = "", y = "IWW (km$^3$)") +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  theme_AP() +
  theme(legend.position = "top")

plot_grid(a, b, ncol = 1, labels = "auto")



isimip.future.dt[, sum(V1, na.rm = TRUE), .(year, model, climate)] %>%
  ggplot(., aes(year, V1, group = interaction(climate, model), color = model)) +
  geom_line() + 
  labs(x = "", y = "IWW (km$^3$)") +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  theme_AP() +
  theme(legend.position = "top")

# KHAN ET AL 2023 DATASET ######################################################

path.projections <- "./files/khan_et_al_2023"
list.of.files <- list.files(path.projections, pattern = "\\.csv$")
combinations <- lapply(list.of.files, function(x) strsplit(x, "_")[[1]][1:4]) %>%
  do.call(rbind, .) %>%
  data.frame()
colnames(combinations) <- c("SSP", "RCP", "Climate", "Use")

# READ FILES IN PARALLEL #######################################################

# Create parallel cluste -------------------------------------------------------

numCores <- detectCores() * 0.75
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Run for loop -----------------------------------------------------------------

result <- foreach(i = 1:length(list.of.files), 
                  .combine = "rbind",
                  .packages = c("data.table", "countrycode", 
                                "sp", "rworldmap")) %dopar% {
                                  
                                  out <- fread(paste("./files/khan_et_al_2023/", list.of.files[i], sep = "/"))
                                  out[, `:=`(SSP = combinations[i, 1], 
                                             RCP = combinations[i, 2], 
                                             Climate = combinations[i, 3], 
                                             Use = combinations[i, 4])]
                                  
                                  Country <- coords2country(out[1:nrow(out), 2:3])
                                  
                                  df <- cbind(Country, out)
                                  
                                  df[, Continent := countrycode(Country, origin = "country.name", destination = "continent")]
                                  
                                  df[, Dataset := list.of.files[i]]
                                  
                                  df
                                }

# Stop the cluster after the computation ---------------------------------------

stopCluster(cl)

# ARRANGE DATA #################################################################

numeric_cols <- grep("^[0-9]+$", names(result), value = TRUE)
khan.dt <- melt(result, measure.vars = numeric_cols, variable.name = "Year") %>%
  .[, Year:= as.numeric(as.character(Year))] %>%
  .[, model:= "GCAM"] %>%
  na.omit() 

# EXPORT DATA ##################################################################

khan.dt.continent <- khan.dt[, .(estimation = sum(value)), 
                             .(Year, Continent, Use, RCP, SSP, Climate, Dataset, model)] %>%
  .[, climate:= paste(Climate, RCP, SSP, sep = "_")] 

fwrite(khan.dt.continent, "khan.dt.continent.csv")


isimip.dt <- fread("isimip.dt.csv")
isimip.future.dt <- fread("isimip.future.dt.csv")

# PLOT #########################################################################

# Continental ------------------------------------------------------------------

khan.dt.continent %>%
  ggplot(., aes(Year, estimation, color = Continent, group = interaction(Dataset, Continent))) +
  geom_line(alpha = 0.4) + 
  facet_wrap(~Use) + 
  theme_AP() + 
  theme(legend.position = "top") +
  labs(x = "Year", y = "km^3")

# Global -----------------------------------------------------------------------

khan.dt[, sum(value), .(Year, Use, Dataset)] %>%
  ggplot(., aes(Year, V1, group = Dataset)) +
  geom_line() + 
  facet_wrap(~Use) +
  theme_AP() +
  theme(legend.position = "top") +
  labs(x = "Year", y = "km^3")


khan.dt[, sum(value), .(Year, Use, Dataset, RCP, SSP)] %>%
  ggplot(., aes(Year, V1, group = Dataset, color = Use)) +
  geom_line() + 
  facet_grid(RCP~SSP) +
  theme_AP() +
  theme(legend.position = "top") +
  labs(x = "Year", y = "km^3")

# ANOVA ########################################################################

# Arrange ISIMIP datasets ------------------------------------------------------

isimip.full <- isimip.dt[social == "varsoc"][, context:= "historic"] %>%
  rbind(., isimip.future.dt[, context:= "prediction"], fill = TRUE) %>%
  .[, social:= NULL] %>%
  .[, estimation:= sum(V1), .(Continent, climate, context, model, year)]

# PLOT #########################################################################

# ARRANGE DATA #################################################################

columns_to_factor <- c("Continent", "climate", "model")
isimip.full[, (columns_to_factor):= lapply(.SD, as.factor), .SDcols = (columns_to_factor)]

# DEFINE MODEL #################################################################

# Linear mixed-effects model with random intercept for year ------------------

analysis_variance_fun <- function(dt) {
  
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

# RUN MODEL ####################################################################

results.dt <- isimip.full[, analysis_variance_fun(.SD), .(Continent, context)]

# PLOT RESULTS #################################################################

a <- isimip.full[, .(estimation = sum(V1)), .(model, Continent, climate, year, context)] %>%
  ggplot(., aes(year, estimation, color = model, group = interaction(climate, model))) +
  geom_line() +
  facet_wrap(context~Continent, scale = "free", ncol = 5) +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  theme_AP() +
  guides(colour = guide_legend(nrow = 2)) +
  labs(x = "Year", y = "IWW (km3)") +
  theme(legend.position = "top")

b <- melt(results.dt, measure.vars = paste(c("climate", "model", "random", "residual"), 
                                      "variance", sep = "_")) %>%
  .[, variance:= tstrsplit(variable, "_", fixed = TRUE)[[1]]] %>%
  ggplot(., aes(Continent, value, fill = variance)) +
  geom_bar(stat = "identity") +
  facet_wrap(~context, ncol = 1) +
  labs(x = "", y = "Variance") +
  scale_fill_discrete(name = "") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_AP() +
  theme(legend.position = "top")
   
plot_grid(a, b, ncol = 2, labels = "auto", rel_widths = c(0.75, 0.25))


################################################################################

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
  geom_line(alpha = 0.7) + 
  facet_wrap(~Continent, scale = "free_y", ncol = 5) +
  theme_AP() +
  theme(legend.position = "top") 



# Fit linear mixed-effects model
model_lmm <- lmer(estimation ~ model + climate + (1 | year), data = dt)
# Check for singularity
if (isSingular(model_lmm)) {
  message("Singular fit detected. Removing random effects.")
  model_lmm <- lm(estimation ~ model + climate, data = dt)
}

# Return results
anova_res <- anova(model_lmm)


# Extract sum of squares for each component
climate_ss <- anova_res["climate", "Sum Sq"]  # Sum of squares for climate
model_ss <- anova_res["model", "Sum Sq"]  # Sum of squares for model
residual_ss <- anova_res["Residuals", "Sum Sq"]  # Sum of squares for residuals

# Calculate total sum of squares
total_ss <- sum(anova_res$`Sum Sq`)

# Calculate proportions of variance explained
climate_var <- climate_ss / total_ss  # Proportion due to climate
model_var <- model_ss / total_ss  # Proportion due to model
residual_var <- residual_ss / total_ss  # Proportion of residual variance

# Return results as a data.table
data.table(
  continent = unique(dt$Continent),
  climate_variance = climate_var,
  model_variance = model_var,
  residual_variance = residual_var
)


khan.dt.continent <- fread("khan.dt.continent.csv")
khan.dt.continent %>%
  .[Use == "withdrawals"] %>%
  ggplot(., aes(Year, estimation, color = RCP, group = Dataset)) +
  geom_line() + 
  facet_wrap(~Continent, scale = "free_y") +
  theme_AP()


isimip.full <- isimip.dt[social == "varsoc"][, context:= "historic"] %>%
  rbind(., isimip.future.dt[, context:= "prediction"], fill = TRUE) %>%
  .[, social:= NULL] %>%
  .[, estimation:= sum(V1), .(Continent, climate, context, model, year)]

# ARRANGE DATA #################################################################

columns_to_factor <- c("Continent", "climate", "model")
isimip.full[, (columns_to_factor):= lapply(.SD, as.factor), .SDcols = (columns_to_factor)]

isimip.full[, .N, .(model, climate, context, Continent)]
unique(isimip.full[, .(model, climate, context)])


# Check for missing values
dt <- rbind(khan.dt2, isimip.full2)
da <- dt[year == 2050, .(min = min(estimation), max = max(estimation)), Continent]
# Calculate the sum of min and max at the continental level
continental_sums <- da[, .(sum_min = sum(min), sum_max = sum(max))]

unique(isimip.full[, .(model, climate, context)]) %>%
  ggplot(dt, aes(x = model, y = climate, fill = context)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_manual(values = c("historic" = "steelblue", "prediction" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Model", y = "Climate", fill = "Context") +
  theme_AP() +
  theme(legend.position = "top")



# Calculate the global min and max
continental_sums[, .(global_min = min(sum_min), global_max = max(sum_max))]






colSums(is.na(dt))
length(dt$model)
length(dt$climate)
length(dt$year)
length(dt$estimation)

--------------------------------------------------------------------------------
small_dt <- dt[1:12000]  # Take a small subset for debugging
model_test <- lmer(estimation ~ model + climate + (1 | year), data = small_dt)
summary(model_test)




# Calculate variance components ------------------------------------------------

var_comp <- as.data.frame(VarCorr(model_lmm))

var_comp <- var_comp %>%
  mutate(proportion = vcov / sum(vcov))






# Summarize data by continent

analysis_variance_fun <- function(dt) {
  
  model <- lmer(iww ~ 1 + (1 | model) + (1 | climate) + (1 | social) + 
                  (1 | year) + (1 | model:climate) + (1 | model:social) + 
                  (1 | climate:social), data = dt)

  var_comp <- as.data.frame(VarCorr(model))
  
  var_comp <- var_comp %>%
    mutate(proportion = vcov / sum(vcov))
  
  return(var_comp)
  
}

results.dt <- data[, analysis_variance_fun(.SD), Continent]
factors.ordered <- c("model", "climate", "social", "year", "residual", "model:climate", 
                     "model:social", "climate:social")
  

results.dt[, grp:= tolower(grp)]
results.dt[, grp:= factor(grp, levels = factors.ordered)]
results.dt %>%
  ggplot(., aes(grp, proportion, fill = Continent)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) + 
  theme_AP() + 
  theme(legend.position = "top") +
  labs(x = "", y = "Proportion")

results.dt[, sum(proportion), Continent]








# Output the results
print(results)






# NAOMI DATASET ################################################################

naomi.projected <- data.table(read.xlsx("naomi_projected.xlsx")) %>%
  .[, study:= paste(author, climate.scenario, sep = ".")] %>%
  .[, focus:= "projected"]

naomi.current <- data.table(read.xlsx("naomi_current.xlsx")) %>%
  .[, climate.scenario:= NA] %>%
  .[, study:= paste(author, climate.scenario, sep = ".")] %>%
  .[, focus:= "current"]

naomi.full.dt <- rbind(naomi.current, naomi.projected)

# CLEAN THE DATASET ############################################################

colnames_vector <- c("title", "author", "region")

# Remove leading and trailing spaces -------------------------------------------

naomi.full.dt[, (colnames_vector):= lapply(.SD, trimws), .SDcols = (colnames_vector)]
naomi.full.dt[, (colnames_vector):= lapply(.SD, str_squish), .SDcols = (colnames_vector)]

# Lowercaps --------------------------------------------------------------------

naomi.full.dt[, (colnames_vector):= lapply(.SD, tolower), .SDcols = (colnames_vector)]

# Remove multiple spaces -------------------------------------------------------

naomi.full.dt[, (colnames_vector):= lapply(.SD, function(x) 
  gsub("\\s+", " ", x)), .SDcols = (colnames_vector)]

# Correct America --------------------------------------------------------------

naomi.full.dt[, region:= ifelse(region == "america", "americas", region)]

# FEATURES OF THE DATASET ######################################################

naomi.full.dt[, publication.date:= str_extract(author, "\\d{4}")]
naomi.full.dt[, range.estimation:= ifelse(publication.date >= 1990 & publication.date < 2000, "1990-2000", 
                                          ifelse(publication.date >= 2000 & publication.date < 2010, "2000-2010",
                                                 "2010-2024"))]

# RANGE ESTIMATION #############################################################

naomi.full.dt[, range.estimation.year:= ifelse(estimation.year >= 1990 & estimation.year < 2010, "1990-2010", 
                                               ifelse(estimation.year >= 2010 & publication.date < 2050, "2010-2050",
                                                      "2050-2100"))]

# DESCRIPTIVE STATISTICS #######################################################

dt <- naomi.full.dt[, .(title, publication.date)] %>%
  .[!duplicated(.)] %>%
  .[, .N, publication.date] %>%
  .[order(publication.date)]

# Calculate cumulative sum
data_clean <- dt[, cumulative_N:= cumsum(N)] %>%
  .[, publication.date:= as.numeric(publication.date)]

# Plot cumulative line
ggplot(data_clean, aes(x = publication.date, y = cumulative_N)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Cumulative Nº Publications") +
  theme_AP()







                        
spread_data <- naomi.full.dt %>%
  .[variable == "iww" & region == "global"] %>%
  group_by(range.estimation, range.estimation.year) %>%
  summarize(max = max(value), 
            min = min(value), 
            n = n(), .groups = "drop") %>%
  data.table() %>%
  .[range.estimation.year > 1995 & n > 1] %>%
  na.omit()

ggplot(spread_data , aes(x = range.estimation.year, ymin = min, ymax = max, color = range.estimation)) +
  geom_pointrange(aes(y = (max + min) / 2), 
                  position = position_dodge(width = 4)) +
  labs(title = "Irrigation Water Withdrawals Over Time",
       x = "Estimation Year",
       y = "Irrigation Water Withdrawals (with min-max range)") +
  theme_AP()


# IWW AND IWC

unique(full.naomi.dt$region)


naomi.full.dt[variable %in% c("iww", "tww") & region == "global"] %>%
  .[, .(author, study, estimation.year, value, variable)] %>%
  na.omit() %>%
  ggplot(., aes(estimation.year, value, color = author, group = study)) +
  geom_point() +
  facet_wrap(~variable) +
  labs(x = "", y = "IWW (km3/yr)") +
  scale_color_discrete(name = "") +
  geom_line() +
  theme_AP()


naomi.full.dt[variable == "iww" & region == "global"] %>%
  .[, .(author, study, estimation.year, value)] %>%
  na.omit() %>%
  ggplot(., aes(estimation.year, value, color = author, group = study)) +
  geom_point() +
  labs(x = "", y = "IWW (km3/yr)") +
  scale_color_discrete(name = "") +
  geom_line() +
  theme_AP()

naomi.full.dt[variable == "tww" & region == "global"] %>%
  .[, .(author, study, estimation.year, value, model)] %>%
  ggplot(., aes(estimation.year, value, color = author, group = study)) +
  geom_point() +
  labs(x = "", y = "km3/yr") +
  scale_color_discrete(name = "") +
  geom_line() +
  theme_AP()


