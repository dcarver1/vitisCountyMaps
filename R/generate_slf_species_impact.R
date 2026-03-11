# R/generate_slf_species_impact.R
pacman::p_load(dplyr, readr, stringr, fs, purrr, tools)


#' Generate SLF Species Impact Summary
#'
#' @param slf_fips_file Path to the CSV containing SLF observations per county (FIPS).
#' @param species_dir Directory containing the individual species summary CSVs.
#' @param output_file Path to save the final summary dataset.
#' @return A formatted data frame matching the SLF Species Impact structure.
generate_slf_impact_summary <- function(
  slf_fips_file = "output_county_observations_fips.csv",
  species_dir = "output/countyMapTables",
  output_file = "SLF_Species_Impact_Summary.csv"
) {
  # --- 1. Load SLF affected counties ---
  # Read as character to prevent dropping leading zeroes on FIPS codes
  slf_data <- readr::read_csv(
    slf_fips_file,
    col_types = readr::cols(.default = "c")
  )

  # Ensure the FIPS list is perfectly standardized to 5 digits
  slf_counties <- slf_data |>
    dplyr::pull(FIPS) |>
    stringr::str_pad(width = 5, side = "left", pad = "0") |>
    unique()

  # --- 2. Locate all Species CSV files ---
  species_files <- fs::dir_ls(species_dir, glob = "*.csv")

  if (length(species_files) == 0) {
    stop("No species CSV files found in the specified directory.")
  }

  # --- 3. Process each species file to calculate spatial overlap ---
  summary_list <- purrr::map_df(species_files, function(file) {
    # Extract taxon name from the filename (e.g., "Vitis aestivalis.csv" -> "Vitis aestivalis")
    taxon_name <- tools::file_path_sans_ext(basename(file))

    # Read species county data
    sp_data <- readr::read_csv(
      file,
      col_types = readr::cols(.default = "c"),
      show_col_types = FALSE
    )

    # Skip if GEOID column is missing for any reason
    if (!"GEOID" %in% names(sp_data)) {
      warning(paste("GEOID column missing in", file, "- skipping."))
      return(NULL)
    }

    # Extract species GEOIDs and ensure 5-digit padding
    sp_geoids <- sp_data |>
      dplyr::pull(GEOID) |>
      stringr::str_pad(width = 5, side = "left", pad = "0") |>
      unique()

    # Calculate intersecting metrics
    total_counties <- length(sp_geoids)
    slf_overlap <- sum(sp_geoids %in% slf_counties)

    # Return as a single-row tibble
    dplyr::tibble(
      Taxon = taxon_name,
      `Total Counties` = total_counties,
      `Counties with SLF` = slf_overlap
    )
  })

  # --- 4. Calculate final metrics and add range estimates ---
  final_table <- summary_list |>
    dplyr::mutate(
      # Calculate Percent Overlap
      `Percent Overlap (%)` = dplyr::if_else(
        `Total Counties` > 0,
        (`Counties with SLF` / `Total Counties`) * 100,
        0
      ),
      `Percent Overlap (%)` = round(`Percent Overlap (%)`, 2),

      # Apply range estimate logic based on taxon names
      `Estimated Range` = dplyr::case_when(
        stringr::str_detect(
          Taxon,
          "(?i)californica|arizonica|girdiana"
        ) ~ "Western",
        stringr::str_detect(
          Taxon,
          "(?i)novae-angliae|labrusca|aestivalis var. bicolor"
        ) ~ "Northern/Eastern",
        stringr::str_detect(
          Taxon,
          "(?i)rotundifolia|mustangensis|shuttleworthii|berlandieri|monticola|acerifolia|lincecumii|rufotomentosa|rupestris|champinii|doaniana|baileyana"
        ) ~ "Southern",
        stringr::str_detect(
          Taxon,
          "(?i)aestivalis|riparia|vulpina|cinerea"
        ) ~ "Widespread",
        TRUE ~ "Widespread"
      )
    ) |>
    # Arrange from most impacted to least impacted
    dplyr::arrange(dplyr::desc(`Percent Overlap (%)`))

  # --- 5. Export and return ---
  readr::write_csv(final_table, output_file)
  message("Success! Saved impact summary to: ", output_file)

  return(final_table)
}

# # --- Example Usage ---
# final_summary <- generate_slf_impact_summary(
#   slf_fips_file = "data/processed/slf/output_county_observations_fips.csv",
#   species_dir = "output/countyMapTables",
#   output_file = "output/SLF_Species_Impact_Summary.csv"
# )
# print(head(final_summary))
