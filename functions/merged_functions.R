# Load required libraries
library(tidyverse)
library(janitor)
library(skimr)
library(knitr)
library(maps)
library(ggplot2)
library(fs)
library(rmarkdown)

# 1. Data Ingestion & Cleaning
load_and_clean_data <- function(input_path) {
  # Read raw CSV
  airports_raw <- read_csv(input_path, show_col_types = FALSE)
  
  # Clean names, trim whitespace, handle missing values
  airports_clean <- airports_raw %>%
    clean_names() %>%
    mutate(across(where(is.character), str_trim)) %>%
    rename_with(~ str_replace_all(., "\\s+", "_")) %>%
    # (customize missing‐value handling as needed)
    replace_na(list(type = "unknown", elevation_ft = 0))
  
  list(raw = airports_raw, clean = airports_clean)
}

# 2. Feature Engineering
enrich_airports <- function(airports_clean) {
  airports_enriched <- airports_clean %>%
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
        iso_region %in% c("AF‑SS", "AF‑CW", "AF‑EA", "AF‑NA", "AF‑SA") ~ "Sub‑Saharan Africa",
        str_starts(iso_region, "EU") ~ "Europe",
        str_starts(iso_region, "NA") ~ "North America",
        TRUE ~ "Other"
      )
    )
  airports_enriched
}

# 3. Data Backup Automation
backup_data <- function(raw, clean, enriched, backup_dir = "backup") {
  dir_create(backup_dir)
  
  # Save snapshots
  write_csv(raw,        path(backup_dir, "airports_raw_backup.csv"))
  write_csv(clean,      path(backup_dir, "airports_clean_backup.csv"))
  write_csv(enriched,   path(backup_dir, "airports_enriched_backup.csv"))
  
  # Log transformations
  transformation_log <- tibble(
    timestamp   = Sys.time(),
    step        = c("Raw Data Load", "Data Cleaning", "Data Enrichment"),
    rows        = c(nrow(raw), nrow(clean), nrow(enriched)),
    cols        = c(ncol(raw), ncol(clean), ncol(enriched)),
    description = c(
      "Initial data load from airports.csv",
      "Cleaned column names, handled missing values, trimmed text",
      "Added hemisphere, elevation_category, airport_size, region_group"
    )
  )
  write_csv(transformation_log, path(backup_dir, "transformation_log.csv"))
  
  transformation_log
}

# 4. Country & Region Analysis
analyze_navaid_coverage <- function(enriched) {
  country_summary <- enriched %>%
    count(iso_country, type, name = "airport_count") %>%
    arrange(desc(airport_count))
  
  region_summary <- enriched %>%
    count(region_group, type, name = "airport_count") %>%
    arrange(desc(airport_count))
  
  list(
    by_country = country_summary,
    by_region  = region_summary
  )
}

# 5. Data Merging & Integration
merge_analysis <- function(enriched, analysis) {
  merged_country <- enriched %>%
    left_join(analysis$by_country, by = c("iso_country", "type"))
  
  merged_region <- enriched %>%
    left_join(analysis$by_region, by = c("region_group", "type"))
  
  list(
    merged_country = merged_country,
    merged_region  = merged_region
  )
}

# 6. Visualization & Reporting
generate_report <- function(merged_country, merged_region,
                            rmd_input = "dw_final_project_merged_group9.Rmd",
                            output_html = "dw_final_project_merged_group9.html") {
  # Example static plots
  p1 <- ggplot(merged_country, aes(longitude_deg, latitude_deg, color = airport_size)) +
    borders("world", colour = "gray80", fill = "gray95") +
    geom_point(alpha = 0.6) +
    theme_minimal() +
    labs(title = "Global Airport Size Distribution")
  
  p2 <- ggplot(merged_region, aes(region_group, airport_count, fill = airport_size)) +
    geom_col(position = "dodge") +
    theme_minimal() +
    coord_flip() +
    labs(title = "Airport Counts by Region and Size")
  
  # Save plots
  ggsave("plot_global_distribution.png", p1, width = 8, height = 4)
  ggsave("plot_region_counts.png",     p2, width = 8, height = 4)
  
  # Render interactive HTML report from R Markdown
  render(
    input       = rmd_input,
    output_file = output_html,
    quiet       = TRUE
  )
}

# ---------------------------
# Example workflow
# ---------------------------
# 1. Load & clean
data_paths <- load_and_clean_data("data/airports.csv")
enriched   <- enrich_airports(data_paths$clean)

# 2. Backup
log_tbl <- backup_data(data_paths$raw, data_paths$clean, enriched)

# 3. Analyze
analysis <- analyze_navaid_coverage(enriched)

# 4. Merge
merged   <- merge_analysis(enriched, analysis)

# 5. Report
generate_report(
  merged_country = merged$merged_country,
  merged_region  = merged$merged_region
)

