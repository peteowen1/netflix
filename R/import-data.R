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
  message("Reading Excel file...")
  raw_data <- read_excel(file_path, sheet = 1)

  # 3. Clean the data
  cleaned_data <- raw_data %>%
    clean_names() %>%
    # Standardize column names (Excel uses "cumulative_", Scraper uses "weeks_")
    rename_with(~ "weeks_in_top_10", .cols = any_of("cumulative_weeks_in_top_10")) %>%
    rename_with(~ "hours_viewed", .cols = any_of("weekly_hours_viewed")) %>%
    mutate(
      week = as.Date(week),
      weekly_rank = as.integer(weekly_rank),
      # Use checking to avoid errors if columns are missing
      weeks_in_top_10 = if("weeks_in_top_10" %in% names(.)) as.integer(weeks_in_top_10) else NA,
      hours_viewed = if("hours_viewed" %in% names(.)) as.numeric(hours_viewed) else NA
    )

  return(cleaned_data)
}
