#' Mean Absolute Change
#' @description Mean Absolute Change
#'
#'
#' @param x a vector
#'
#'
#' @export
#'@seealso \code{\link{mean}}
#' @examples \dontrun{
#' # don't run this sript
#' info <- mean_abs_change("data")
#' }
#'


mean_abs_change <- function(x){
  if(missing(x)){"X is notsupplied."}
  stopifnot(inherits(x, "numeric"))
  ch <- mean(abs(diff(x)))
  return(ch)
}
