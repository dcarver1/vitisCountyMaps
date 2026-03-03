# R/data_processing.R

# 1. Filter and return the list of target species
get_target_species <- function(taxonomy_path) {
  readr::read_csv(taxonomy_path, show_col_types = FALSE) |>
    dplyr::filter(countySpecies == "Y") |>
    dplyr::pull(taxon)
}

# 2. Fetch and split the reviewed data from Google Sheets
fetch_reviewed_data <- function(sheet_url) {
  reviewed_data <- googlesheets4::read_sheet(googledrive::as_id(sheet_url))
  
  # Filter for points
  points <- reviewed_data[reviewed_data$`Record ID for point` != "NA", ] |>
    tidyr::drop_na(Taxon)
  
  # Filter for counties
  county <- reviewed_data[!reviewed_data$Timestamp %in% points$Timestamp, ] |>
    tidyr::drop_na(Taxon)
  
  list(points = points, county = county)
}

# 3. Process and merge BONAP and USDA Plants data
process_plants_bonap <- function(bonap_data, plants_data, named_features, output_path) {
  b1 <- bonap_data |>
    dplyr::select(`State Abbveation` = Stateabb,
                  taxon = `Scientific Name`,
                  countyFIPS = FIPS,
                  `county name` = County) |>
    dplyr::mutate(BONAP = 1)
  
  pl1 <- plants_data |>
    dplyr::left_join(y = named_features, by = c("plant_symbol" = "Accepted Symbol")) |>
    dplyr::mutate(countyFIPS = stringr::str_sub(geoid, start = 3)) |>
    dplyr::select(taxon = `Scientific Name`,
                  state,
                  countyFIPS,
                  `county name` = county) |>
    dplyr::mutate(`USDA Plants` = 1)
  
  pb <- dplyr::bind_rows(b1, pl1) |>
    dplyr::select(taxon, countyFIPS, `county name`, `State Abbveation`, state, BONAP, `USDA Plants`)
  
  readr::write_csv(pb, file = output_path)
  
  return(pb)
}