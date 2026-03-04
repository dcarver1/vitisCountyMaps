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

# R/data_processing.R
assign_spatial_attributes <- function(obs_data, county_shp) {
  
  # Ensure target columns exist in base data so we never lose rows due to missing columns
  target_cols <- c("state", "county", "stateFIPS", "countyFIPS", "iso3", "spatial_match_type")
  for (col in target_cols) {
    if (!col %in% names(obs_data)) {
      obs_data[[col]] <- NA_character_
    }
  }

  # 1. Prep Lookup Table
  county_lookup <- county_shp |>
    sf::st_drop_geometry() |>
    dplyr::select(STATE_NAME, STUSPS, NAME, GEOID, STATEFP) |>
    dplyr::mutate(
      NAME_lower = tolower(NAME),
      STATE_lower = tolower(STATE_NAME)
    )
  
  # ==========================================
  # TIER 1: Spatial Join via Coordinates
  # ==========================================
  has_coords <- obs_data |> dplyr::filter(!is.na(longitude) & !is.na(latitude))
  no_coords  <- obs_data |> dplyr::filter(is.na(longitude) | is.na(latitude))
  
  successful_spatial <- data.frame()
  
  if (nrow(has_coords) > 0) {
    coords_sf <- sf::st_as_sf(has_coords, coords = c("longitude", "latitude"), 
                              crs = 4326, remove = FALSE)
    
    joined_sf <- sf::st_join(coords_sf, county_shp, join = sf::st_intersects) |>
      sf::st_drop_geometry() |>
      dplyr::mutate(
        state = ifelse(is.na(state), STATE_NAME, state),
        county = ifelse(is.na(county), NAME, county),
        stateFIPS = ifelse(is.na(stateFIPS), STATEFP, stateFIPS),
        countyFIPS = ifelse(is.na(countyFIPS), GEOID, countyFIPS),
        iso3 = ifelse(!is.na(GEOID), "USA", iso3),
        spatial_match_type = ifelse(!is.na(GEOID), "Coordinate Join", spatial_match_type)
      ) |>
      dplyr::select(-any_of(c("STATEFP", "COUNTYFP", "COUNTYNS", "GEOIDFQ", 
                              "NAME", "NAMELSAD", "STUSPS", "STATE_NAME", "LSAD", "ALAND", "AWATER")))
    
    # Split US matches vs non-US/ocean matches
    successful_spatial <- joined_sf |> dplyr::filter(!is.na(countyFIPS))
    failed_spatial <- joined_sf |> dplyr::filter(is.na(countyFIPS))
    
    # Pass all failed coordinates to text mining
    no_coords <- dplyr::bind_rows(no_coords, failed_spatial)
  }
  
  # ==========================================
  # TIER 2: Text Mining Locality Information
  # ==========================================
  if (nrow(no_coords) > 0) {
    mined_text <- no_coords |>
      dplyr::mutate(
        loc_clean = tolower(localityInformation),
        extracted_county = stringr::str_match(loc_clean, "([a-z\\s]+)\\s+(county|co\\.)")[,2],
        extracted_county = stringr::str_trim(extracted_county),
        extracted_state = stringr::str_extract(loc_clean, paste(unique(county_lookup$STATE_lower), collapse = "|")),
        extracted_state_abbr = stringr::str_extract(localityInformation, paste0("\\b(", paste(unique(county_lookup$STUSPS), collapse = "|"), ")\\b"))
      ) 
    
    uniqueStates <- county_lookup |> dplyr::select(STATE_lower, STUSPS, STATEFP, STATE_NAME) |> distinct()
    
    mined_text <- mined_text |>
      dplyr::left_join(uniqueStates, by = c("extracted_state_abbr" = "STUSPS")) |>
      dplyr::mutate(
        state = ifelse(is.na(state) & !is.na(STATE_NAME), STATE_NAME, state),
        stateFIPS = ifelse(is.na(stateFIPS) & !is.na(STATEFP), STATEFP, stateFIPS)
      ) |>
      dplyr::select(-STATE_NAME, -STATEFP, -STATE_lower) |>
      dplyr::left_join(uniqueStates, by = c("extracted_state" = "STATE_lower")) |>
      dplyr::mutate(
        state = ifelse(is.na(state) & !is.na(STATE_NAME), STATE_NAME, state),
        stateFIPS = ifelse(is.na(stateFIPS) & !is.na(STATEFP), STATEFP, stateFIPS)
      ) |>
      dplyr::select(-STATE_NAME, -STATEFP, -STUSPS)

    withState <- mined_text |> dplyr::filter(!is.na(stateFIPS))
    noState   <- mined_text |> dplyr::filter(is.na(stateFIPS))

    if(nrow(withState) > 0) {
      for(i in 1:nrow(withState)) {
        s_fips <- withState$stateFIPS[i]
        potential_county <- withState$extracted_county[i]
        full_text <- withState$loc_clean[i]
        
        state_counties <- county_lookup[county_lookup$STATEFP == s_fips, ]
        
        if(!is.na(potential_county)) {
          match_idx <- which(state_counties$NAME_lower == potential_county)
          if(length(match_idx) == 1) {
            withState$county[i] <- state_counties$NAME[match_idx]
            withState$countyFIPS[i] <- state_counties$GEOID[match_idx]
            withState$spatial_match_type[i] <- "Text: Explicit County in State"
            next
          }
        }
        
        state_county_names <- state_counties$NAME_lower[order(nchar(state_counties$NAME_lower), decreasing = TRUE)]
        found_name <- state_county_names[sapply(state_county_names, function(x) grepl(paste0("\\b", x, "\\b"), full_text))][1]
        
        if(!is.na(found_name)) {
          match_info <- state_counties[state_counties$NAME_lower == found_name, ]
          withState$county[i] <- match_info$NAME[1]
          withState$countyFIPS[i] <- match_info$GEOID[1]
          withState$spatial_match_type[i] <- "Text: Fuzzy County in State"
        } else {
          withState$spatial_match_type[i] <- "Text: State Only"
        }
      }
    }
    
    countyMatches <- dplyr::bind_rows(withState, noState) |>
      dplyr::mutate(iso3 = ifelse(!is.na(stateFIPS), "USA", iso3)) |>
      # Clean up the regex columns so they don't block the final join
      dplyr::select(-loc_clean, -extracted_county, -extracted_state, -extracted_state_abbr)
      
  } else {
    countyMatches <- data.frame()
  }

  # ==========================================
  # Final Recombine (No Filtering)
  # ==========================================
  # bind_rows stacks cleanly and pads missing columns with NA
  final_data <- dplyr::bind_rows(successful_spatial, countyMatches) |>
    dplyr::select(taxon, state, county, stateFIPS, countyFIPS, iso3, longitude, latitude, spatial_match_type, everything())
  
  return(final_data)
}
