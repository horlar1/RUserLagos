% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataDescribe.R
\name{DataInfo}
\alias{DataInfo}
\title{Get Data Information}
\usage{
DataInfo(data, target)
}
\arguments{
\item{data}{a dataframe or matrix.}

\item{target}{target variable.}
}
\description{
Provides information like unique counts,data type,percentage of missing vlaues spearman correlation
}
\examples{
\dontrun{
# don't run this sript
info <- DataInfo("path to data")
}

}
\seealso{
\code{\link{cor}}
}
