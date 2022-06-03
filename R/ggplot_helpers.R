#' Spencer's Preferred Theme 
#' 
#' Subject to change at any given point in time without warning.
#'
#' @return a ggplot `theme()` object
#' @export
theme_spencer <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = "Times New Roman"
      ),
      plot.title = ggplot2::element_text(
        hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5
      )
    )
}