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

pacman::p_load(dplyr, sf, stringr, tidyr)

assign_spatial_attributes <- function(obs_data, county_shp) {
  
  # Build a text lookup table and calculate county centroids for the fallback
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
  
  # Split data: has coordinates vs missing coordinates
  has_coords <- obs_data |> dplyr::filter(!is.na(longitude) & !is.na(latitude))
  no_coords  <- obs_data |> dplyr::filter(is.na(longitude) | is.na(latitude))
  
  if (nrow(has_coords) > 0) {
    # Convert to sf and intersect
    coords_sf <- sf::st_as_sf(has_coords, coords = c("longitude", "latitude"), 
                              crs = 4326, remove = FALSE)
    
    joined_sf <- sf::st_join(coords_sf, county_shp, join = sf::st_intersects) |>
      sf::st_drop_geometry() |>
      # Assign the intersected values
      dplyr::mutate(
        state = ifelse(is.na(state), STATE_NAME, state),
        county = ifelse(is.na(county), NAME, county),
        stateFIPS = STATEFP,
        countyFIPS = GEOID,
        iso3 = ifelse(!is.na(GEOID), "USA", iso3), # ASSIGN ISO3 FOR US MATCHES
        spatial_match_type = ifelse(!is.na(GEOID), "Coordinate Join", NA)
      ) |>
      # Clean up tigris columns
      dplyr::select(-any_of(c("STATEFP", "COUNTYFP", "COUNTYNS", "GEOIDFQ", 
                              "NAME", "NAMELSAD", "STUSPS", "STATE_NAME", "LSAD", "ALAND", "AWATER")))
    
    # Separate successful spatial joins from those that fell outside US borders
    successful_spatial <- joined_sf |> dplyr::filter(!is.na(countyFIPS))
    failed_spatial <- joined_sf |> dplyr::filter(is.na(countyFIPS))
    
    # Send failed spatial joins to the text mining pile
    no_coords <- dplyr::bind_rows(no_coords, failed_spatial)
  } else {
    successful_spatial <- obs_data[0, ]
  }
  
  # ==========================================
  # TIER 2: Text Mining Locality Information
  # ==========================================
  
  if (nrow(no_coords) > 0) {
    mined_text <- no_coords |>
      dplyr::mutate(
        # Standardize text for searching
        loc_clean = tolower(localityInformation),
        
        # Look for "[Word] County" or "[Word] Co."
        extracted_county = stringr::str_match(loc_clean, "([a-z\\s]+)\\s+(county|co\\.)")[,2],
        extracted_county = stringr::str_trim(extracted_county),
        
        # Try to pull state based on state name or 2-letter abbreviation
        extracted_state = stringr::str_extract(loc_clean, paste(county_lookup$STATE_lower, collapse = "|")),
        extracted_state_abbr = stringr::str_extract(localityInformation, paste(paste0("\\b", unique(county_lookup$STUSPS), "\\b"), collapse = "|"))
      ) 
    # join on the state first 
    uniqueStates <- county_lookup |> dplyr::select(STATE_lower, STATEFP, STATE_NAME) |> distinct()
    # connect to the mined_text
    stateJoin <- mined_text |>
      dplyr::left_join(uniqueStates, by = c("extracted_state" = "STATE_lower")) 
    # assign the state and stateFIPS based on the join. 
    stateJoin$state <- stateJoin$STATE_NAME
    stateJoin$stateFIPS <- stateJoin$STATEFP
    # remove some data we no longer need
    stateJoin <- stateJoin |> dplyr::select(-STATE_NAME, -STATEFP, -extracted_state, -extracted_state_abbr)

    # parse to the data into records with State FIPs and those without.
    withStateFIPS <- stateJoin |> dplyr::filter(!is.na(stateFIPS))
    withoutStateFIPS <- stateJoin |> dplyr::filter(is.na(stateFIPS))
    
    # For records with state FIPS, we can attempt a more targeted county match by joining to the county lookup using both the extracted county and state FIPS
    for(i in nrow(withStateFIPS)) {
      message(paste("Attempting to match county for record", i, "with extracted county:", withStateFIPS$extracted_county[i], "and state FIPS:", withStateFIPS$stateFIPS[i]))
      state <- withStateFIPS$stateFIPS[i]
      county <- withStateFIPS$extracted_county[i]
      if(is.na(county)){
        next()
      }else{
        # filtet the couty lookup to the state FIPS
        county_lookup_state <- county_lookup |> dplyr::filter(STATEFP == state)
        # unique counties in the state
        unique_counties <- county_lookup_state$NAME_lower
        # attempt to match the extracted county to the unique counties in the state
        county_match <- stringr::str_detect(unique_counties, county)
        if(sum(county_match) == 1){
          matched_county <- unique_counties[county_match]
          withStateFIPS$county[i] <- matched_county
          withStateFIPS$countyFIPS[i] <- county_lookup_state |> dplyr::filter(NAME_lower == matched_county) |> dplyr::pull(GEOID)
          withStateFIPS$iso3[i] <- "USA"
          withStateFIPS$spatial_match_type[i] <- "Text Mining with State FIPS"
        } else {
          # split string to unique features 
          string_parts <- str_split(county, "\\s+")[[1]]
          # check if any of the string parts match the county names in the state
          matches <- c()
          for(word in string_parts){
            county_match_parts <- stringr::str_detect(unique_counties, word)
            if(sum(county_match_parts) == 1){
              print(paste("Matched county using part of the string:", word))
            }
          }
        
      }

    }
    withStateFIPS <- withStateFIPS |>
      dplyr::left_join(county_lookup, 
        by = c("extracted_county" = "NAME_lower", "stateFIPS" = "STATEFP"), suffix = c("", "_state")) |>
  

 
  # ==========================================
  # Recombine and Filter Non-US Records
  # ==========================================
  
  final_data <- dplyr::bind_rows(successful_spatial, mined_text) |>
    # Explicitly remove known records from Canada and Mexico (or any other non-US iso3 codes you wish to exclude)
    dplyr::filter(!iso3 %in% c("CAN", "MEX")) |>
    dplyr::select(taxon, state, county, stateFIPS, countyFIPS, iso3, longitude, latitude, spatial_match_type, everything())
  
  return(final_data)
}