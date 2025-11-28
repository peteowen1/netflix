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
