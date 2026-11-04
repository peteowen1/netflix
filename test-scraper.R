# Quick test to see what the scraped data looks like
library(rvest)
library(tidyverse)
library(janitor)
devtools::load_all()

# Test scraping the latest week
test_data <- get_netflix_weekly_data("2025-12-07")

# Show the structure
glimpse(test_data)

# Show first few rows
print(head(test_data, 20))

# Check what columns we got
cat("\nColumn names:", paste(names(test_data), collapse = ", "), "\n")
