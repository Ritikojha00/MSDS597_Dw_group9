# 0. Load required libraries
library(tidyverse)
library(janitor)
library(skimr)
library(maps)
library(ggplot2)
library(sf)
library(leaflet)
library(fs)
library(rmarkdown)

# 1. Data Ingestion & Cleaning
load_and_clean_data <- function(input_path) {
  airports_raw <- read_csv(input_path, show_col_types = FALSE)
  airports_clean <- airports_raw %>%
    clean_names() %>%
    mutate(across(where(is.character), str_trim)) %>%
    rename_with(~ str_replace_all(., "\\s+", "_")) %>%
    replace_na(list(type = "unknown", elevation_ft = 0))
  list(raw = airports_raw, clean = airports_clean)
}

# 2. Feature Engineering
enrich_airports <- function(airports_clean) {
  airports_clean %>%
    mutate(
      hemisphere = if_else(latitude_deg >= 0, "Northern", "Southern"),
      elevation_category = case_when(
        elevation_ft < 1000   ~ "Low",
        elevation_ft < 10000  ~ "Medium",
        TRUE                  ~ "High"
      ),
      airport_size = case_when(
        type == "large_airport"  ~ "Large",
        type == "medium_airport" ~ "Medium",
        TRUE                     ~ "Small"
      ),
      region_group = case_when(
        iso_region %in% c("AF‑SS","AF‑CW","AF‑EA","AF‑NA","AF‑SA") ~ "Sub‑Saharan Africa",
        str_starts(iso_region, "EU") ~ "Europe",
        str_starts(iso_region, "NA") ~ "North America",
        TRUE ~ "Other"
      )
    )
}

# 3. Data Backup Automation
backup_data <- function(raw, clean, enriched, backup_dir = "backup") {
  dir_create(backup_dir)
  write_csv(raw,      path(backup_dir, "airports_raw_backup.csv"))
  write_csv(clean,    path(backup_dir, "airports_clean_backup.csv"))
  write_csv(enriched, path(backup_dir, "airports_enriched_backup.csv"))
  
  tibble(
    timestamp   = Sys.time(),
    step        = c("Raw Load","Clean","Enrich"),
    rows        = c(nrow(raw), nrow(clean), nrow(enriched)),
    cols        = c(ncol(raw), ncol(clean), ncol(enriched))
  ) %>%
    write_csv(path(backup_dir, "transformation_log.csv"))
}

# 4. Country & Region Analysis
analyze_navaid_coverage <- function(enriched) {
  by_country <- enriched %>%
    count(iso_country, type, name = "airport_count") %>%
    arrange(desc(airport_count))
  by_region  <- enriched %>%
    count(region_group, type, name = "airport_count") %>%
    arrange(desc(airport_count))
  list(by_country = by_country, by_region = by_region)
}

# 5. Data Merging & Integration
merge_analysis <- function(enriched, analysis) {
  merged_country <- enriched %>%
    left_join(analysis$by_country, by = c("iso_country","type"))
  merged_region  <- enriched %>%
    left_join(analysis$by_region,  by = c("region_group","type"))
  list(merged_country = merged_country, merged_region = merged_region)
}

# 6. Navigation Aid Proximity Analysis
# 6.1 Haversine distance
calc_distance <- function(lon1, lat1, lon2, lat2) {
  to_rad <- pi/180
  lon1 <- lon1 * to_rad; lat1 <- lat1 * to_rad
  lon2 <- lon2 * to_rad; lat2 <- lat2 * to_rad
  dlon <- lon2 - lon1; dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1)*cos(lat2)*sin(dlon/2)^2
  2 * asin(sqrt(a)) * 6371  # km
}

# 6.2 Find all navaids within a radius (e.g. 50 km)
find_navaids_proximity <- function(airports, navaids, radius_km = 50) {
  airports %>%
    filter(!is.na(latitude_deg), !is.na(longitude_deg)) %>%
    rowwise() %>%
    mutate(
      nearby = list(
        navaids %>% filter(!is.na(latitude_deg), !is.na(longitude_deg)) %>%
          rowwise() %>%
          mutate(dist = calc_distance(
            longitude_deg, latitude_deg,
            cur_data()$longitude_deg, cur_data()$latitude_deg
          )) %>%
          filter(dist <= radius_km) %>%
          select(ident, name, type, frequency_khz, dist)
      )
    ) %>%
    unnest(nearby)
}

# 7. Magnetic Variation Analysis
analyze_magnetic_variation <- function(navaids) {
  mag_var_data <- navaids %>%
    filter(!is.na(magnetic_variation_deg)) %>%
    mutate(
      variation_category = case_when(
        magnetic_variation_deg < -15 ~ "Strong West",
        magnetic_variation_deg < -5  ~ "Moderate West",
        magnetic_variation_deg < 5   ~ "Minimal",
        magnetic_variation_deg < 15  ~ "Moderate East",
        TRUE                         ~ "Strong East"
      )
    )
  mag_var_data
}

# 8. Visualization & Reporting
generate_report <- function(merged_country, merged_region,
                            prox_data, mag_var_data,
                            rmd_input = "dw_final_project_merged_group9.Rmd",
                            output_html = "dw_final_project_merged_group9.html") {
  # (reproduce static plots as before…)
  # …
  render(rmd_input, output_file = output_html, quiet = TRUE)
}

# ---------------------------
# Example end‑to‑end workflow
# ---------------------------
paths    <- load_and_clean_data("data/airports.csv")
enriched <- enrich_airports(paths$clean)
backup   <- backup_data(paths$raw, paths$clean, enriched)

analysis <- analyze_navaid_coverage(enriched)
merged   <- merge_analysis(enriched, analysis)

# navaids_clean <- read_csv("data/navaids.csv") %>% clean_names()

# Proximity analysis
# prox_data <- find_navaids_proximity(enriched, navaids_clean)

# Magnetic variation
# mag_data  <- analyze_magnetic_variation(navaids_clean)

# Finally, render the interactive report
# generate_report(
#   merged_country = merged$merged_country,
#   merged_region  = merged$merged_region,
#   prox_data      = prox_data,
#   mag_var_data   = mag_data
# )
