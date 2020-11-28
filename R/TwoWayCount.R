#' Get Two Way Count
#' @description Provides two way interaction count on two features, useful for feature engineering
#'
#'
#' @param data a dataframe.
#' @param variable1 a character defining the first feature.
#' @param variable2 a character defining the second feature.
#'
#' @importFrom sqldf sqldf
#' @export
#'@seealso \code{\link{cor} \link{freq.encode}}
#' @examples \dontrun{
#' # don't run this sript
#' 2waycnt <- my2waycnt(data,"feat1","feat2")
#' }
#'


my2waycnt <- function(data,variable1,variable2){

  data <- data.frame(f1= data[,variable1],f2=data[,variable2])
  colnames(data) <- c("f1","f2")
  sum1 <- sqldf("select f1,f2, count(*) as cnt from data group by f1,f2")
  tmp <- sqldf("select b.cnt from data a left join sum1 b on a.f1=b.f1 and a.f2=b.f2")
  tmp$cnt[is.na(tmp$cnt)] = 0

  return(tmp$cnt)

}
