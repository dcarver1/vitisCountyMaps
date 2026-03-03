# config.R
# Define all file paths and external URLs for the county evaluation workflow

config <- list(
  paths = list(
    # Raw Data
    taxonomy         = "data/raw/taxonomy20231212.csv",
    named_features   = "data/raw/nameList.csv",
    plants_data      = "data/raw/USDAplants_completeVitis.csv",
    bonap_data       = "data/raw/bonap.csv",
    nature_serve     = "data/raw/natureServe.csv",
    fna_data         = "data/raw/FNA_stateClassification.csv",
    ns_ref_data      = "data/raw/vitisReferenceFile.csv",
    syn_vitis        = "data/raw/taxonomy20231212.csv",
    orig_vitis       = "data/raw/DataForCountyMaps_20230320.csv",
    # Processed / Spatial Data
    observation_data = "data/processed/model_data20251216.csv",
    county_shp       = "data/processed/counties/counties.gpkg",
    state_shp        = "data/processed/states/states.gpkg",
    
    # Outputs & Templates
    pb_output        = "data/processed/vitis_plants_bonap.csv",
    rmd_template     = "scripts/countyEvaluation.Rmd",
    output_dir       = "output/maps"
  ),
  urls = list(
    # External Data Sources
    reviewed_data    = "https://docs.google.com/spreadsheets/d/1_BfJawocOnA-1m9_gl5qZvufXHBCCOacMZX69cQz2LY/edit#gid=139317771"
  )
)
