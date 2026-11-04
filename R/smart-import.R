#' Get Latest Available Week from Netflix Website
#'
#' @return A Date object representing the latest Sunday week available on Netflix Top 10
#' @importFrom rvest read_html html_elements html_attr
#' @importFrom lubridate floor_date wday weeks
#' @export
get_latest_netflix_week <- function() {

  # Try to fetch the films page to see what the latest week is
  base_url <- "https://www.netflix.com/tudum/top10/films"

  tryCatch({
    page <- read_html(base_url)

    # Look for week selector or date in the page
    # Netflix uses a dropdown or displays the current week
    # We'll try to extract it from the URL or page metadata

    # Method 1: Check for date in dropdown options
    date_elements <- page %>%
      html_elements("option[value*='week=']") %>%
      html_attr("value")

    if (length(date_elements) > 0) {
      # Extract dates from the value attributes
      dates <- gsub(".*week=", "", date_elements) %>%
        as.Date()
      latest_week <- max(dates, na.rm = TRUE)
      message(paste("Latest week on Netflix website:", latest_week))
      return(latest_week)
    }

    # Method 2: Default to last Sunday if we can't find it
    # Netflix updates weekly on Tuesdays, so last Sunday is usually current
    today <- Sys.Date()
    last_sunday <- floor_date(today, "week", week_start = 7)

    # If today is Sunday, Monday, or early Tuesday, use previous Sunday
    if (wday(today) <= 3) {
      last_sunday <- last_sunday - weeks(1)
    }

    message(paste("Assuming latest week is:", last_sunday))
    return(last_sunday)

  }, error = function(e) {
    warning(paste("Could not determine latest week from website:", e$message))
    # Fallback to last Sunday
    today <- Sys.Date()
    last_sunday <- floor_date(today, "week", week_start = 7)
    if (wday(today) <= 3) {
      last_sunday <- last_sunday - weeks(1)
    }
    return(last_sunday)
  })
}


#' Import Netflix Data with Automatic Gap Filling
#'
#' @param file_path String. Local path to 'all-weeks-global.xlsx' OR a direct URL.
#' @param check_latest Logical. If TRUE, checks for missing weeks and scrapes them. Default: TRUE
#' @return A cleaned dataframe with all available data including scraped updates
#' @importFrom dplyr bind_rows mutate
#' @importFrom lubridate weeks
#' @export
import_netflix_smart <- function(file_path, check_latest = TRUE) {

  # Step 1: Import Excel data
  message(paste(rep("=", 70), collapse = ""))
  message("STEP 1: Importing Excel data...")
  message(paste(rep("=", 70), collapse = ""))

  excel_data <- import_netflix_excel(file_path)

  # Get latest week in Excel data
  latest_excel_week <- max(excel_data$week, na.rm = TRUE)
  message(paste("\nLatest week in Excel file:", latest_excel_week))

  # Step 2: Check if we need to scrape additional weeks
  if (!check_latest) {
    message("\nSkipping latest week check (check_latest = FALSE)")
    return(excel_data)
  }

  message("\n", paste(rep("=", 70), collapse = ""))
  message("STEP 2: Checking for missing weeks...")
  message(paste(rep("=", 70), collapse = ""))

  latest_website_week <- get_latest_netflix_week()
  message(paste("Latest week on website:", latest_website_week))

  # Step 3: Scrape missing weeks if needed
  if (latest_website_week > latest_excel_week) {
    weeks_behind <- as.numeric(difftime(latest_website_week, latest_excel_week, units = "weeks"))
    message(paste("\nExcel data is", floor(weeks_behind), "week(s) behind!"))
    message("\n", paste(rep("=", 70), collapse = ""))
    message("STEP 3: Scraping missing weeks...")
    message(paste(rep("=", 70), collapse = ""))

    # Scrape from the week after latest Excel to latest website week
    start_scrape <- latest_excel_week + weeks(1)

    scraped_data <- scrape_multiple_weeks(
      start_date = start_scrape,
      end_date = latest_website_week
    )

    if (nrow(scraped_data) > 0) {
      message(paste("\nSuccessfully scraped", nrow(scraped_data), "new rows"))

      # Standardize column types before combining
      # Scraped data may have different types (especially runtime)
      if ("runtime" %in% names(scraped_data)) {
        scraped_data <- scraped_data %>%
          mutate(runtime = as.numeric(runtime))
      }

      if ("weekly_views" %in% names(scraped_data)) {
        scraped_data <- scraped_data %>%
          mutate(weekly_views = as.numeric(weekly_views))
      }

      # Combine datasets
      combined_data <- bind_rows(excel_data, scraped_data)

      message(paste("Combined dataset now has", nrow(combined_data), "total rows"))
      message(paste("Date range:", min(combined_data$week), "to", max(combined_data$week)))

      return(combined_data)
    } else {
      warning("Scraping returned no data. Using Excel data only.")
      return(excel_data)
    }

  } else {
    message("\nExcel data is up to date! No scraping needed.")
    return(excel_data)
  }
}
