pacman::p_load(dplyr, readr, stringr, tools, tibble)

# 1. Load configuration and helper functions (if needed)
# source("scripts/config.R")

# List files
files <- list.files(
  path = "~/trueNAS/work/vitisCountyMaps/output/countyMapTables",
  full.names = TRUE
)

# Helper function to round values to N significant digits
sigfig <- function(vec, n = 4) {
  formatC(signif(vec, digits = n), digits = n, format = "fg", flag = "#")
}

# Initialize an empty list to store the summary for each species
summary_list <- list()

# 2. Summarize by species --------------------------------------------------
for (i in seq_along(files)) {
  # Read in the CSV
  d1 <- read_csv(files[i], show_col_types = FALSE)

  # EXTRACTION STEP: Pull species name from the file path
  taxa_name <- tools::file_path_sans_ext(basename(files[i]))

  # Filter to only counties with at least one record
  d2 <- d1 |>
    dplyr::filter(anyRecord > 0)

  # Calculate total counties for this species
  c1 <- nrow(d2)

  # If the species has records, calculate the summaries
  if (c1 > 0) {
    hg <- d2 |> dplyr::filter(!is.na(H) | !is.na(G)) |> nrow()
    n1 <- d2 |> dplyr::filter(!is.na(O)) |> nrow()
    b1 <- d2 |> dplyr::filter(!is.na(BONAP)) |> nrow()
    p1 <- d2 |> dplyr::filter(!is.na(`USDA Plants`)) |> nrow()

    # Construct the summary row (NO SIGFIG YET)
    df <- tibble(
      taxon = taxa_name,
      totalCounties = c1,
      `Counties with H or G` = hg,
      `Percent Coverage H or G` = (hg / c1) * 100,
      `Counties with BONAP` = b1,
      `Percent Coverage BONAP` = (b1 / c1) * 100,
      `Counties with USDA Plants` = p1,
      `Percent Coverage USDA Plants` = (p1 / c1) * 100,
      `Counties with INaturalist` = n1,
      `Percent Coverage INaturalist` = (n1 / c1) * 100
    )
  } else {
    # Failsafe: if the CSV exists but has 0 records
    df <- tibble(
      taxon = taxa_name,
      totalCounties = 0,
      `Counties with H or G` = 0,
      `Percent Coverage H or G` = 0,
      `Counties with BONAP` = 0,
      `Percent Coverage BONAP` = 0,
      `Counties with USDA Plants` = 0,
      `Percent Coverage USDA Plants` = 0,
      `Counties with INaturalist` = 0,
      `Percent Coverage INaturalist` = 0
    )
  }

  # Store the dataframe in our list
  summary_list[[i]] <- df
}

# 3. Combine all lists into a single dataframe, THEN format the numbers
output <- dplyr::bind_rows(summary_list) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ sigfig(.x)))

# View and Export
View(output)
write_csv(
  output,
  file = "~/trueNAS/work/vitisCountyMaps/output/countyCountsSummaryTable.csv"
)
