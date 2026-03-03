# R/geoprocessing.R
pacman::p_load(dplyr, sf, stringr, tidyr)

prep_species_data <- function(speciesName, namedFeatures, plantsData1, bonapData, synData,
                              natureSeverData, observationData, fnaData, countySHP, origData,
                              stateSHP, reviewedPoints, reviewedCounty, nsRefData,
                              export_dir = "output/countyMapTables") {
  
  # --- 1. Taxonomic & Reference Data ---
  plantsSpecies <- dplyr::left_join(plantsData1, namedFeatures, by = c("plant_symbol" = "Accepted Symbol")) |>
    dplyr::select("plant_symbol", "Scientific Name") |>
    dplyr::distinct()
  
  species <- dplyr::tibble(`Scientific Name` = speciesName) |>
    dplyr::left_join(plantsSpecies, by = "Scientific Name")

  #need to join the occurrence  ith the origil county p  to get the stend county FIPS. I correctly noted in the data

  
  s1 <- synData |> 
    dplyr::filter(taxon == speciesName) |>
    as.data.frame() |>
    dplyr::select(taxon, acceptedSynonym, 
                  Names_to_exclude_from_this_Species_Complex = `Names to exclude from this concept`, 
                  Species_Complex = `Species Complex`,
                  NatureServe_Concept_reference = `NatureServe Concept reference`,
                  Relationship_to_FNA = `Relationship to FNA`,
                  Taxonomic_notes = `Taxonomic notes`,
                  Classification_Status_NatureServe = `Classification Status NatureServe`) |>
    dplyr::slice(1)
  
  nsRefData2 <- nsRefData |> dplyr::filter(`Scientific Name` == speciesName) |>
    dplyr::mutate(link = `View on NatureServe Explorer`)
  
  # --- 2. Reviewed Data Subsetting ---
  reviewedPoints1 <- reviewedPoints[reviewedPoints$Taxon == speciesName, ]
  reviewedCounty1 <- reviewedCounty[reviewedCounty$Taxon == speciesName, ]
  
  # --- 3. County Reference Data (USDA/BONAP/NatureServe) ---
  if(!is.na(species$plant_symbol)) {
    plantsData <- plantsData1 |> 
      dplyr::filter(plant_symbol == species$plant_symbol) |>
      dplyr::mutate(plantsData = stringr::str_sub(geoid, start = 3, end = 7)) |>
      dplyr::select(plantsData)
  } else {
    plantsData <- dplyr::tibble(plantsData = character())
  }
  
  bonap <- bonapData |> dplyr::filter(`Scientific Name` == speciesName) |> dplyr::select(bonap = FIPS)
  natureServe <- natureSeverData |> dplyr::filter(taxon == speciesName) |> dplyr::select(natureServe = countyFIPS)
  
  # UPDATED: Using tigris columns (NAME, NAMELSAD, GEOID, STUSPS)
  countyGathered <- countySHP |>
    dplyr::select(NAME, NAMELSAD, GEOID, STUSPS) |>
    dplyr::mutate(
      plants = dplyr::case_when(GEOID %in% plantsData$plantsData ~ "USDA_Plants"),
      bonap = dplyr::case_when(GEOID %in% bonap$bonap ~ "bonap"),
      natureServe = dplyr::case_when(GEOID %in% natureServe$natureServe ~ "Nature_Serve"),
      allCountySources = paste0(plants, " ", bonap, " ", natureServe)
    ) |>
    dplyr::mutate(allCountySources = stringr::str_remove_all(allCountySources, pattern = "NA")) |>
    dplyr::filter(allCountySources != "  ")
  
  # Handle reviewed counties
  if(nrow(reviewedCounty1) > 0) {
    reviewedCounty1$GEOID <- as.character(unlist(reviewedCounty1$`GEOID for County`))
    rCounty <- countySHP |>
      dplyr::filter(GEOID %in% reviewedCounty1$GEOID) |>
      dplyr::left_join(reviewedCounty1, by = "GEOID") |>
      dplyr::mutate(popup = paste0("<b>Suggested Action:</b> ", `Suggested Action for this Occurrence: select one`, 
                                   "<br/><b>Comments:</b> ", Comments))
    countyGathered <- countyGathered |> dplyr::filter(!GEOID %in% reviewedCounty1$GEOID)
  } else {
    rCounty <- countyGathered[0,]
  }
  
  # --- 4. Occurrence Data & FNA Filtering ---
  occData <- observationData |>
    dplyr::filter(taxon == speciesName)|>
    dplyr::mutate(type = dplyr::case_when(sampleCategory == "HUMAN_OBSERVATION" ~ "O", TRUE ~ type)) |>
    dplyr::distinct()
  
  # Handle reviewed points
  if(nrow(reviewedPoints1) > 0) {
    rPoints <- occData |>
      dplyr::filter(recordID %in% reviewedPoints1$`Record ID for point`) |>
      dplyr::left_join(reviewedPoints1, by = c("recordID" = "Record ID for point")) |>
      dplyr::mutate(popup = paste0("<b>Suggested Action:</b> ", `Suggested Action for this Occurrence: select one`, 
                                   "<br/><b>Comments:</b> ", Comments))
    occData <- occData |> dplyr::filter(!recordID %in% reviewedPoints1$`Record ID for point`)
  } else {
    rPoints <- occData[0,]
  }
  
  # FNA Filter Logic
  fnaData <- fnaData[!fnaData$`States from FNA` == "NA,", ]
  if(speciesName %in% fnaData$`Taxon Name`) {
    fna1 <- fnaData |> dplyr::filter(`Taxon Name` == speciesName) |> dplyr::pull(`States from FNA`) |>
      stringr::str_split(pattern = ",") |> unlist() |> stringr::str_trim() |> stringr::str_subset(".+")
    state1 <- fna1[fna1 != "NA"]
    all_states <- stateSHP |> dplyr::filter(name %in% state1)
    FNAspecies <- TRUE
  } else {
    fna1 <- "no"
    state1 <- unique(occData$state)
    # UPDATED: Mapping to STUSPS to derive state names
    state_CountyData <- stateSHP |>
      dplyr::filter(NAME %in% state1) |> 
      sf::st_drop_geometry() |> 
      dplyr::pull(NAME)
    all_states <- stateSHP |> 
      dplyr::filter(NAME %in% state_CountyData)
    FNAspecies <- FALSE
  }
  
  occFNAFilter <- occData |>
    dplyr::filter(!state %in% all_states$NAME)
  occData <- occData |> dplyr::filter(state %in% all_states$NAME)
  
  # --- 5. Spatial Data Generation ---
  sp1 <- occData |> 
    dplyr::filter(!is.na(latitude), iso3 != "CAN", iso3 != "MEX") |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs(stateSHP))
  # ensure that the countyFIPS is in the sp1 data 


  countyCounts <- occData |> 
    dplyr::group_by(type, countyFIPS) |> 
    dplyr::summarise(count = dplyr::n(), .groups = 'drop')
  
  # UPDATED: Simplified county gathering based on GEOID and STUSPS
  countyState <- countySHP |> 
    dplyr::filter(STUSPS %in% all_states$postal) |>
    dplyr::select(NAME, GEOID, STUSPS)
  
  sp2 <- sp1 |> dplyr::filter(is.na(yearRecorded) | yearRecorded >= 1970)
  older <- sp1 |> dplyr::filter(yearRecorded < 1970)
  
  # Summarize by type
  for(i in c("O", "H", "G")) {
    c2 <- countyCounts |> dplyr::filter(type == i) |> dplyr::select(countyFIPS, !!sym(i) := count)
    countyState <- dplyr::left_join(countyState, c2, by = c("GEOID" = "countyFIPS"))
  }
  
  # Pre-1970 records
  if(nrow(older) > 0) {
    df2 <- countyState[unlist(sf::st_intersects(older, countyState)), "GEOID"] |>
      sf::st_drop_geometry() |> dplyr::group_by(GEOID) |> dplyr::summarise(pre1970 = dplyr::n())
    countyState <- dplyr::left_join(countyState, df2, by = "GEOID")
  } else {
    countyState$pre1970 <- NA
  }
  
  # Join gathered features and calc total
  c2 <- countyGathered |> sf::st_drop_geometry() |>
    dplyr::select(GEOID, `USDA Plants` = plants, BONAP = bonap, `Nature Serve` = natureServe) |>
    dplyr::mutate(across(c(`USDA Plants`, BONAP, `Nature Serve`), ~ifelse(!is.na(.), 1, NA)))
  
  countyState <- dplyr::left_join(countyState, c2, by = "GEOID") |>
    dplyr::rowwise() |> dplyr::mutate(anyRecord = sum(c_across(c(O, H, G, `USDA Plants`, BONAP, `Nature Serve`)), na.rm = TRUE)) |> dplyr::ungroup()
  
  # Export table
  if(!is.null(export_dir)) {
    if(!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE)
    readr::write_csv(sf::st_drop_geometry(countyState), file.path(export_dir, paste0(speciesName, ".csv")))
  }
  
  # --- 6. Final Map Layers Preparation ---
  brbg <- c("#a6611a", "#dfc27d", "#f5f5f5", '#c7eae5', '#80cdc1', '#35978f', '#01665e')
  
  # UPDATED: Referencing NAME and STUSPS in the popup
  countyClass <- countyState |> 
    dplyr::mutate(
      anyRecord = ifelse(anyRecord == 0, NA, anyRecord),
      allColor = dplyr::case_when(
        is.na(anyRecord) ~ "#d9dbde",
        anyRecord == 1 ~ brbg[1],
        anyRecord >= 2 & anyRecord < 3 ~ brbg[2],
        anyRecord >= 3 & anyRecord < 5 ~ brbg[4],
        anyRecord >= 5 & anyRecord < 8 ~ brbg[5],
        anyRecord >= 8 & anyRecord < 12 ~ brbg[6],
        anyRecord >= 12 ~ brbg[7]
      ),
      singleColor = ifelse(is.na(anyRecord), "#d9dbde", "#a8ddb5"),
      popup = paste0("<b>County Name: </b>", NAME, "<br/> <b>State: </b>", STUSPS, "<br/> <b> Total Records: </b>", anyRecord)
    )
  
  points_sf <- sp1 |>
    dplyr::mutate(
      color = dplyr::case_when(type == "O" ~ "#fc8d62", type == "H" ~ "#8da0cd", type == "G" ~ "#66c2a5"),
      url = dplyr::case_when(
        grepl("http", sourceUniqueID) ~ paste0("<a target='_blank' href='", sourceUniqueID, "'>view</a>"),
        grepl("http", collectionSource) ~ paste0("<a target='_blank' href='", collectionSource, "'>view</a>"),
        TRUE ~ NA_character_
      ),
      h_link = ifelse(type == "H" & !is.na(url), TRUE, ifelse(type == "H", FALSE, NA)),
      popup = paste0("<b>Data Source: </b>", databaseSource, "<br/> <b> Type: </b>", type)
    )
  
  # --- 7. Return Bundled Object ---
  return(list(
    meta = list(
      speciesName = speciesName,
      synonyms = s1,
      nsRef = nsRefData2,
      fnaIncluded = FNAspecies,
      fnaStates = fna1,
      allStateNames = all_states$name
    ),
    map_layers = list(
      countyClass = countyClass,
      bpData = countyClass |> dplyr::filter((`USDA Plants` == 1 | BONAP == 1) & is.na(O) & is.na(G) & is.na(H) & is.na(`Nature Serve`)),
      hgData = countyClass |> dplyr::filter(H > 0 | G > 0),
      oData = countyClass |> dplyr::filter(O > 0 & anyRecord == O),
      points = points_sf,
      rCounty = rCounty,
      rPoints = rPoints
    ),
    tables = list(
      occFiltered = occData,
      countySummary = sf::st_drop_geometry(countyClass) |> dplyr::filter(!is.na(anyRecord)),
      fnaFiltered = occFNAFilter
    )
  ))
}
