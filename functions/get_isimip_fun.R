
################################################################################

get_isimip_fun <- function(nc_file, variable, start_year) {
  
  nc_data <- nc_open(nc_file)
  
  # Extract longitude, latitude, and water withdrawal data
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  aindww <- ncvar_get(nc_data, variable)
  time <- ncvar_get(nc_data, "time")
  
  # Close the NetCDF file after reading
  nc_close(nc_data)
  
  # Create a grid of lon and lat coordinates
  lon_lat_grid <- expand.grid(lon = lon, lat = lat)
  
  # Earth's radius in meters
  earth_radius <- units::set_units(6371000, "m")
  
  # Function to convert degrees to radians
  deg_to_rad <- function(deg) {
    return(deg * pi / 180)
  }
  
  # Calculate the grid area (m²) for each grid cell
  lon_lat_grid$lat_rad <- deg_to_rad(lon_lat_grid$lat)
  
  # Assume the grid cells have equal spacing (match your grid resolution)
  delta_lon <- deg_to_rad(0.5)  # Assuming 0.5-degree grid spacing in longitude
  delta_lat <- deg_to_rad(0.5)  # Assuming 0.5-degree grid spacing in latitude
  
  # Calculate the area of each grid cell (in square meters)
  lon_lat_grid$area_cell <- as.numeric(earth_radius^2 * delta_lon * delta_lat * cos(lon_lat_grid$lat_rad))
  
  # Convert aindww from kg/m²/s to m³/month
  # 1 kg of water = 1 liter = 1e-3 m³
  # Multiply by the number of seconds in a month (30.44 days average)
  seconds_per_month <- 30.44 * 24 * 3600  # Average month duration in seconds
  
  # Generate the corresponding year and month for each time step
  start_year <- start_year
  n_months <- length(time)
  
  years <- rep(start_year:(start_year + (n_months %/% 12)), each = 12, length.out = n_months)
  months <- rep(1:12, length.out = n_months)
  
  # Convert withdrawal from kg/m²/s to km³/month for each grid cell
  results <- list()
  for (month_idx in 1:n_months) {
    aindww_month <- aindww[, , month_idx]  # Extract the 2D slice (lon x lat) for the given month
    
    # Flatten the 2D data to match the grid
    aindww_month_flat <- as.numeric(aindww_month)
    
    # Calculate water withdrawal in km³/month for each grid cell
    lon_lat_grid[[paste0("year_", years[month_idx], "_month_", months[month_idx])]] <- 
      aindww_month_flat * 1e-3 * lon_lat_grid$area_cell * seconds_per_month / 1e9  # Convert to km³
  }
  
  # Create a regular expression pattern to match any of the specified years
  year_pattern <- paste(years, collapse = "|")
  
  # Identify columns that contain the specified years
  columns_years <- grep(paste0("year_", year_pattern, "_"), colnames(lon_lat_grid), value = TRUE)
  
  # Include essential columns like lon, lat, etc.
  columns_selected <- c("lon", "lat", "lat_rad", "area_cell", columns_years)
  
  lon_lat_grid <- as.data.table(lon_lat_grid)
  
  # Subset the data table with only the selected columns
  dt_selected <- lon_lat_grid[, ..columns_selected] %>%
    na.omit()
  
  countries <- coords2country(dt_selected)
  
  result <- cbind(countries, dt_selected) %>%
    data.table() %>%
    .[!is.na(countries)] %>%
    melt(., measure.vars = columns_years) %>%
    .[, year := sub("year_(\\d+)_month_\\d+", "\\1", variable)] %>%
    .[, year:= as.numeric(year)] %>%
    .[, sum(value, na.rm = TRUE), .(countries, year)] %>%
    .[, variable:= NULL] %>%
    .[order(countries)] %>%
    .[, Country := countrycode(countries, origin = "country.name", destination = "country.name")] %>%
    .[, Continent:= countrycode(Country, origin = "country.name", destination = "continent")]
  
  return(result)
}

################################################################################
