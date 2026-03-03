# renderCountySummaries.R
# Main execution script for generating Vitis county evaluation maps

pacman::p_load(dplyr, readr, sf, terra, htmltools, googledrive, googlesheets4, purrr, rmarkdown)

# 1. Load configuration and helper functions
list.files("R/functions", full.names = TRUE) |> purrr::walk(source)


# 2. Read base datasets
named_features    <- read_csv(config$paths$named_features, show_col_types = FALSE)
plants_data1      <- read_csv(config$paths$plants_data, show_col_types = FALSE)
bonap_data        <- read_csv(config$paths$bonap_data, show_col_types = FALSE)
nature_serve_data <- read_csv(config$paths$nature_serve, show_col_types = FALSE)
fna_data          <- read_csv(config$paths$fna_data, show_col_types = FALSE)
ns_ref_data       <- read_csv(config$paths$ns_ref_data, show_col_types = FALSE)
synData <- read_csv(config$paths$syn_vitis, show_col_types = FALSE)
originalVitis <- read_csv(config$paths$orig_vitis, show_col_types = FALSE)

observation_data  <- read_csv(config$paths$observation_data, show_col_types = FALSE) |>
  dplyr::filter(!is.na(taxon))


county_shp <- sf::st_read(config$paths$county_shp, quiet = TRUE)
county_shp <- sf::st_read(config$paths$county_shp, quiet = TRUE)
state_shp  <- sf::st_read(config$paths$state_shp, quiet = TRUE)

# 3. Process dynamic data using helper functions
full_species <- get_target_species(config$paths$taxonomy)

reviewed_lists <- fetch_reviewed_data(config$urls$reviewed_data)
reviewed_points <- reviewed_lists$points
reviewed_county <- reviewed_lists$county

# Generate and save the merged BONAP/Plants CSV
pb_combined <- process_plants_bonap(bonap_data, plants_data1, named_features, config$paths$pb_output)

# 4. Define the rendering function
# inside renderCountySummaries.R

generate_occurrence_rmd <- function(species_name) {
  message(paste("Processing and Rendering map for:", species_name))
  
  # 1. Run the geoprocessing to get perfectly clean data
  prepped_data <- prep_species_data(
    speciesName     = species_name,
    namedFeatures   = named_features,
    plantsData1     = plants_data1,
    bonapData       = bonap_data,
    natureSeverData = nature_serve_data,
    observationData = observation_data,
    synData = synData,
    origData = originalVitis,
    fnaData         = fna_data,
    countySHP       = county_shp,
    stateSHP        = state_shp,
    reviewedPoints  = reviewed_points,
    reviewedCounty  = reviewed_county,
    nsRefData       = ns_ref_data,
    export_dir      = "output/countyMapTables"
  )
  
  # 2. Render the template using ONLY the prepped data
  rmarkdown::render(
    input = config$paths$rmd_template,
    output_format = "html_document",
    output_dir = config$paths$output_dir,
    output_file = paste0(species_name, "_Evaluation.html"),
    params = list(
      data = prepped_data # One single parameter!
    )
  )
}

# 5. Execute
# purrr::walk(full_species, generate_occurrence_rmd) # Uncomment to run full batch
generate_occurrence_rmd(species_name = "Vitis baileyana") # Test single run
