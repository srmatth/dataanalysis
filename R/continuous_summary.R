#' Continuous Value Summary
#'
#' @param values the values to be summarized, a numeric vector
#' @param subset a logical vector denoting the subset of `values`
#'   that you wish to summarize. Defaults to `NULL`.
#' @param type either `"mean"` in which case the mean and standard
#'   deviation are computed, or `"median"` where the median and IQR
#'   are computed.
#' @param accuracy the rounding accuracy of the output string, as in
#'   `scales::number()`.
#' @param na.rm whether or not to remove `NA`s, defaults to `TRUE`
#'
#' @return a character string summarizing `values`
#' @export
continuous_summary <- function(
  values, 
  subset = NULL, 
  type = "mean", 
  accuracy = 0.01, 
  na.rm = TRUE
) {
  if (!is.null(subset)) values <- values[subset]
  if (type == "mean") {
    res <- stringr::str_c(
      scales::number(
        mean(values, na.rm = na.rm), 
        accuracy = accuracy, 
        big.mark = ","
      ),
      " (",
      scales::number(
        sd(values, na.rm = na.rm), 
        accuracy = accuracy, 
        big.mark = ","
      ),
      ")"
    )
  } else if (type == "median") {
    res <- stringr::str_c(
      scales::number(
        median(values, na.rm = na.rm), 
        accuracy = accuracy, 
        big.mark = ","
      ),
      " (",
      scales::number(
        IQR(values, na.rm = na.rm), 
        accuracy = accuracy, 
        big.mark = ","
      ),
      ")"
    )
  }
  res
}