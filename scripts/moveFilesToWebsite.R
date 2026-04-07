###
# script for transferring report files to the share site
# carverd@colostate.edu
# 20230713
###

pacman::p_load(dplyr, readr)

source("scripts/config.R") # Load configuration settings


##################
# vitis specific moves ----------------------------------------------------
folder <- "~/trueNAS/work/vitis/m"
folderCounty <- "~/trueNAS/work/vitis/c"
# species
spList <- read_csv(config$paths$orig_vitis, show_col_types = FALSE) |>
  dplyr::select(taxon) |>
  tidyr::drop_na(taxon) |> # Removes rows where taxon is NA
  dplyr::distinct() |>
  dplyr::pull()

# point to the county map outputs
files <- list.files(
  "output/maps",
  pattern = "_Evaluation.html",
  full.names = TRUE
)
print(files)
# Find the files
for (i in seq_along(files)) {
  if (length(files) > 0) {
    file.copy(files[i], folderCounty, overwrite = TRUE)
    print(paste0(i, " moved"))
  }
}
