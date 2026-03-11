pacman::p_load(dplyr, readr, tigris, stringr, fs, purrr, tools)

source("scripts/config.R") # Load configuration settings

# process the slf data from edd maps
fips_results <- process_eddmaps_fips("data/raw/eddmaps_mappings.csv")
readr::write_csv(
  county_summary,
  "data/processed/slf/output_county_observations_fips.csv"
)

# generate the summary table of overlap between the species
final_summary <- generate_slf_impact_summary(
  slf_fips_file = "data/processed/slf/output_county_observations_fips.csv",
  species_dir = "output/countyMapTables",
  output_file = "output/SLF_Species_Impact_Summary.csv"
)
