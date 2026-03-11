# R/process_eddmaps_fips.R

#' Process EDDMapS Mapping CSV to Standardized FIPS and Observation Counts
#'
#' @param file_path Character string for the path to the mapping CSV.
#' @return A data frame containing standardized 5-digit character FIPS codes
#'         and total observations per county.
process_eddmaps_fips <- function(file_path) {
  # 1. Load the occurrence data
  df <- readr::read_csv(file_path, show_col_types = FALSE)

  # 2. Parse the 'Location' column into County, State, Country
  df_parsed <- df |>
    dplyr::mutate(Location = stringr::str_remove_all(Location, '"')) |>
    # Split the column by comma into three expected parts
    tidyr::separate(
      Location,
      into = c("County", "State", "Country"),
      sep = ",\\s*",
      fill = "right"
    ) |>
    # Keep only complete records successfully parsed to US counties
    dplyr::filter(Country == "United States") |>
    dplyr::mutate(
      # Standardize EDDMapS independent cities to match tigris formatting
      # e.g., "Alexandria (independent city)" -> "Alexandria city"
      County = stringr::str_replace_all(
        County,
        "(?i)\\s*\\(independent city\\)",
        " city"
      ),
      County = stringr::str_trim(County),
      State = stringr::str_trim(State)
    )

  # 3. Get FIPS crosswalk from the `tigris` package
  # The tigris package has a built-in 'fips_codes' dataset containing state and county identifiers
  fips_lookup <- tigris::fips_codes |>
    dplyr::mutate(
      # Tigris appends " County", " Parish", " Borough", etc. We strip those to match EDDMapS.
      # Note: We keep "city" intact to distinguish independent cities (e.g., Baltimore city vs Baltimore County)
      County_Match = stringr::str_remove(
        county,
        "(?i)\\s*(County|Parish|Borough|Census Area|Municipality)$"
      ),
      County_Match = stringr::str_trim(County_Match),

      # Create 5-digit character code natively (2 digit state + 3 digit county)
      FIPS = paste0(state_code, county_code)
    ) |>
    dplyr::select(state_name, County_Match, FIPS) |>
    dplyr::distinct()

  # 4. Join the data and summarize the total number of observations per FIPS
  county_summary <- df_parsed |>
    dplyr::left_join(
      fips_lookup,
      by = c("State" = "state_name", "County" = "County_Match")
    ) |>
    # Drop rows that don't match to a valid US county FIPS code
    dplyr::filter(!is.na(FIPS)) |>
    # Group by the standardized FIPS code and summarize
    dplyr::group_by(FIPS, State, County) |>
    dplyr::summarise(Total_Observations = dplyr::n(), .groups = "drop") |>
    # Final cleanup: ensure FIPS is stored as exactly a 5-digit character vector
    dplyr::mutate(
      FIPS = as.character(FIPS),
      FIPS = stringr::str_pad(FIPS, width = 5, side = "left", pad = "0")
    ) |>
    # Arrange from most observations to least
    dplyr::arrange(dplyr::desc(Total_Observations))

  return(county_summary)
}

# # --- Example Usage ---
# fips_results <- process_eddmaps_fips("data/raw/eddmaps_mappings.csv")
# readr::write_csv(
#   county_summary,
#   "data/processed/slf/output_county_observations_fips.csv"
# )
# print(head(county_summary))
