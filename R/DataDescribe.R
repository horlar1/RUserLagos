#' Get Data Information
#' @description Provides information like unique counts,data type,percentage of missing vlaues spearman correlation
#'
#'
#' @param data a dataframe or matrix.
#' @param target target variable.
#'
#' @export DataInfo
#' @seealso \code{\link{cor}}
#' @examples \dontrun{
#' # don't run this sript
#' info <- DataInfo("path to data")
#' }
#'

DataInfo = function(data, target){

  output <- data.frame(
    type <- unlist(lapply(data,class)),
    n.unique <- unlist(lapply(data,function(data)length(unique(data)))),
    f.missing <- unlist(lapply(data,function(data)mean(is.na(data)))),
    spear.cor <- unlist(lapply(data,function(data){idx = !is.na(data);
    if(is.factor(data)) data <- as.numeric(data);
    if(is.character(data)) data <- as.numeric(as.factor(data));
    if(is.integer(data)) data <- as.numeric(data);
    if(is.logical(data)) data <- as.numeric(data);
    cor(data[idx],y = target[idx], method = "spearman")
    })))
  colnames(output) = c("Data type","N.unique","Per.missing","spear.cor")

  return(output)
}


