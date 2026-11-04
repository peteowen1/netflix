#' Scrape Netflix Top 10 Data for a Specific Week
#'
#' @param week_date String or Date. The Sunday ending the week (Format: "YYYY-MM-DD").
#' @return A dataframe containing the Top 10 for all 4 categories.
#' @importFrom rvest read_html html_element html_table
#' @importFrom dplyr mutate bind_rows select any_of
#' @importFrom janitor clean_names
#' @export
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
        # 1. Parse table and clean initial names
        df <- table_node %>%
          html_table() %>%
          clean_names()

        # 2. Rename columns to match target structure
        # 'x' is usually 'weeks_in_top_10' and 'views' is 'weekly_views'
        df <- df %>%
          rename(
            weeks_in_top_10 = any_of("x"),
            weekly_views = any_of("views")
          )

        # 3. Handle Ranking (Split rank and title)
        if ("ranking" %in% names(df)) {
          df <- df %>%
            mutate(
              weekly_rank = as.integer(str_extract(ranking, "^\\d+")),
              show_title = str_remove(ranking, "^\\d+")
            ) %>%
            select(-ranking)
        }

        # 4. Clean Numerics (Remove commas)
        # We use mutate combined with across for efficiency
        df <- df %>%
          mutate(
            across(c(weekly_views, hours_viewed), ~ as.numeric(gsub(",", "", .x))),
            weeks_in_top_10 = as.integer(weeks_in_top_10)
          )

        # 5. Fix Runtime (Convert "1:32" to 1.53 decimal hours)
        if ("runtime" %in% names(df)) {
          df$runtime <- sapply(strsplit(as.character(df$runtime), ":"), function(t) {
            if(length(t) == 2) as.numeric(t[1]) + as.numeric(t[2])/60 else NA
          })
        }

        # 6. Add Metadata and ensure missing columns exist
        df <- df %>%
          mutate(
            category = categories[slug],
            week = as.Date(week_str),
            # Ensure these exist so bind_rows doesn't fail later
            season_title = if("season_title" %in% names(.)) season_title else NA_character_,
            views_in_millions = NA_real_ # Keeping as NA to match your target snippet
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
#' @importFrom lubridate floor_date
#' @importFrom purrr map_df
#' @export
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
