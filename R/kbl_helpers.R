#' Kable PDF Format
#' 
#' Presets for table formats I commonly use in my reports.
#'
#' @param df data frame to be output in the `kbl`
#' @param colnames the displayed column names in the output
#' @param caption the caption of the table in the output
#' @param escape logical, whether or not to escape the table.
#'   Defaults to `TRUE` (i.e. math notation will not be formatted).
#'
#' @return an object with class `kbl`
#' @export
kbl_pdf <- function(df, colnames, caption, escape = TRUE) {
  df %>%
    kableExtra::kbl(
      col.names = colnames,
      caption = caption,
      align = stringr::str_c(
        "l", 
        stringr::str_c(
          rep("c", ncol(df) - 1),
          collapse = ""
        )
      ),
      booktabs = TRUE,
      format = "latex",
      escape = escape,
      linesep = "",
      position = "ht!"
    )
}