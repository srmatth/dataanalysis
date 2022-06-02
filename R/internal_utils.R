#' Alternative ifelse Function
#'
#' @param test logical test
#' @param yes return if `test` is true
#' @param no return if `test` is false
#'
#' @return either `yes` or `no` depending on the value of `test`
ifelse1 <- function (test, yes, no){
  if (test) yes
  else no
}