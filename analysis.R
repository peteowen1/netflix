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

#' Scrape Netflix Top 10 Data for a Specific Week
#'
#' @param week_date String or Date. The Sunday ending the week (Format: "YYYY-MM-DD").
#' @return A dataframe containing the Top 10 for all 4 categories.
get_netflix_weekly_data <- function(week_date) {

  # Ensure date is a character string for URL
  week_str <- as.character(week_date)
  base_url <- "https://www.netflix.com/tudum/top10"

  # The 4 categories used by Netflix
  categories <- c(
    "films" = "Films (English)",
    "films-non-english" = "Films (Non-English)",
    "tv" = "TV (English)",
    "tv-non-english" = "TV (Non-English)"
  )

  all_data <- list()
  message(paste("Processing week:", week_str))

  for (slug in names(categories)) {
    target_url <- paste0(base_url, "/", slug, "?week=", week_str)

    tryCatch({
      # Read HTML
      page <- read_html(target_url)
      table_node <- page %>% html_element("table")

      if (!is.na(table_node)) {
        # Parse table
        df <- table_node %>%
          html_table() %>%
          clean_names() %>%
          mutate(
            category = categories[slug],
            week = as.Date(week_str),
            # Clean numeric columns if they exist
            hours_viewed = if("hours_viewed" %in% names(.)) as.numeric(gsub(",", "", hours_viewed)) else NA,
            views_in_millions = if("views_in_millions" %in% names(.)) as.numeric(gsub(",", "", views_in_millions)) else NA,
            weeks_in_top_10 = if("weeks_in_top_10" %in% names(.)) as.integer(weeks_in_top_10) else NA
          )
        all_data[[slug]] <- df
      }
    }, error = function(e) {
      warning(paste("  Skip:", slug, "-", e$message))
    })

    # Polite delay
    Sys.sleep(0.5)
  }

  bind_rows(all_data)
}

#' Scrape Multiple Weeks of Data
#'
#' @param start_date String. Start date (YYYY-MM-DD).
#' @param end_date String. End date (YYYY-MM-DD).
#' @return A combined dataframe of all weeks.
scrape_multiple_weeks <- function(start_date, end_date) {

  dates <- seq(as.Date(start_date), as.Date(end_date), by = "week")
  sunday_dates <- floor_date(dates, "week", week_start = 7) %>% unique()

  message(paste("Found", length(sunday_dates), "weeks to scrape."))

  full_dataset <- map_df(sunday_dates, function(date) {
    data <- get_netflix_weekly_data(date)
    return(data)
  })

  return(full_dataset)
}

# ==============================================================================
# METHOD 2: IMPORT EXCEL (Recommended)
# ==============================================================================

#' Import Official Netflix Excel Dataset
#'
#' @param file_path String. Local path to 'all-weeks-global.xlsx' OR a direct URL.
#' @return A cleaned dataframe ready for analysis.
import_netflix_excel <- function(file_path) {

  # 1. Handle URL downloads if necessary
  if (grepl("^http", file_path)) {
    message("Attempting to download file from URL...")
    temp_file <- tempfile(fileext = ".xlsx")

    # Mode "wb" is crucial for Excel files on Windows
    tryCatch({
      download.file(file_path, temp_file, mode = "wb")
      file_path <- temp_file
      message("Download successful.")
    }, error = function(e) {
      stop("Download failed. Please download the file manually and provide the local path.")
    })
  }

  # 2. Read the Excel file
  # Netflix Excel files typically have a specific sheet structure.
  # We assume the data is in the first sheet.
  message("Reading Excel file...")
  raw_data <- read_excel(file_path, sheet = 1)

  # 3. Clean the data
  cleaned_data <- raw_data %>%
    clean_names() %>%
    mutate(
      # Netflix excel dates are sometimes strings, sometimes Date objects.
      # This handles both cases safely.
      week = as.Date(week),

      # Ensure numeric columns are actually numeric
      weekly_rank = as.integer(weekly_rank),
      weeks_in_top_10 = as.integer(weeks_in_top_10),
      hours_viewed_first_28_days = if("hours_viewed_first_28_days" %in% names(.)) as.numeric(hours_viewed_first_28_days) else NA
    )

  return(cleaned_data)
}

# ==============================================================================
# EXECUTION EXAMPLES
# ==============================================================================

# --- OPTION A: DIRECT DOWNLOAD (Fastest & Best) ---
# This uses the official direct link extracted from the viewer URL you found.
official_url <- "https://www.netflix.com/tudum/top10/data/all-weeks-global.xlsx"
official_data <- import_netflix_excel(official_url)

# Filter for the specific week you requested (2025-10-26)
week_data <- official_data %>% filter(week == "2025-10-26")
print(head(week_data))

# --- OPTION B: SCRAPING (Backup Method) ---
# If the Excel link changes or fails, use the scraper:
# scraped_data <- scrape_multiple_weeks("2025-10-01", "2025-10-31")
# print(head(scraped_data))
