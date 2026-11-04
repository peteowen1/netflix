#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @importFrom utils download.file
NULL

# Column names used unquoted in dplyr pipelines
utils::globalVariables(c(
  ".", "hours_viewed", "ranking", "runtime", "season_title", "week",
  "weekly_rank", "weekly_views", "weeks_in_top_10"
))
