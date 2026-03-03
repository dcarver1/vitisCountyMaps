


gatherLayers <- function() {
  # Define paths
  destdir <- "data/processed"
  counties_file <- file.path(destdir, "counties", "counties.gpkg")
  states_file <- file.path(destdir, "states", "states.gpkg")
  
  # Create directories if they don't exist
  if (!dir.exists(file.path(destdir, "counties"))) {
    dir.create(file.path(destdir, "counties"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(file.path(destdir, "states"))) {
    dir.create(file.path(destdir, "states"), recursive = TRUE, showWarnings = FALSE)
  }
  
  # Download and write counties if file doesn't exist
  if (!file.exists(counties_file)) {
    counties <- tigris::counties(cb = TRUE) |>
      sf::st_transform(crs = 4326)
    sf::st_write(counties, counties_file, delete_dsn = TRUE, quiet = TRUE)
  }
  
  # Download and write states if file doesn't exist
  if (!file.exists(states_file)) {
    states <- tigris::states(cb = TRUE) |>
      sf::st_transform(crs = 4326)
    sf::st_write(states, states_file, delete_dsn = TRUE, quiet = TRUE)
  }  
}
