# ==============================================================================
# Netflix Top 10 Scraper & Importer
# ==============================================================================
# Requirements:
# install.packages(c("rvest", "tidyverse", "janitor", "lubridate", "readxl"))

library(rvest)
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
devtools::load_all()

# ==============================================================================
# METHOD 1: SCRAPING (If you don't have the file)
# ==============================================================================
# Note: Functions are now defined in the package (R/scrape-data.R)
# Use get_netflix_weekly_data() and scrape_multiple_weeks() from the package

# ==============================================================================
# METHOD 2: IMPORT EXCEL (Recommended)
# ==============================================================================
# Note: Function is now defined in the package (R/import-data.R)
# Use import_netflix_excel() from the package

# ==============================================================================
# EXECUTION EXAMPLES
# ==============================================================================

# --- RECOMMENDED: SMART IMPORT (Auto-fills missing weeks) ---
# This automatically checks if the Excel file is up to date
# and scrapes any missing weeks from the website
official_url <- "https://www.netflix.com/tudum/top10/data/all-weeks-global.xlsx"
netflix_data <- import_netflix_smart(official_url)

# View summary
glimpse(netflix_data)
message(paste("Date range:", min(netflix_data$week), "to", max(netflix_data$week)))

# --- OPTION A: BASIC EXCEL IMPORT (No auto-update) ---
# If you just want the Excel data without checking for updates:
# official_data <- import_netflix_excel(official_url)

# --- OPTION B: MANUAL SCRAPING ---
# If you need specific weeks or want to bypass Excel entirely:
# scraped_data <- scrape_multiple_weeks("2025-10-01", "2025-10-31")
# single_week <- get_netflix_weekly_data("2025-11-30")

# ==============================================================================
# EXAMPLE ANALYSES
# ==============================================================================

# Top shows by total hours viewed
top_shows <- netflix_data %>%
  group_by(show_title, category) %>%
  summarize(
    total_hours = sum(hours_viewed, na.rm = TRUE),
    weeks_on_list = n_distinct(week),
    avg_rank = mean(weekly_rank, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_hours)) %>%
  head(20)

print(top_shows)
