#' Categorical Summary
#' 
#' Summarize a categorical variable (just a specific level) and give the 
#' number of observations and the corresponding percentage.
#'
#' @param values a vector (usually from a data frame)
#' @param level the level to summarize
#'
#' @return a character string listing the number of occurrences of `level` in
#'   `values` along with the percentage in parentheses
#' @export
categorical_summary <- function(values, level) {
  paste0(
    scales::number(sum(values == level), big.mark = ","), 
    " (", round(sum(values == level) / length(values)*100, 2), "\\%)"
  )
}