% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{data.frame.empty}
\alias{data.frame.empty}
\title{Empty Data Frame}
\usage{
data.frame.empty(nrow = 0, ncol)
}
\arguments{
\item{nrow}{number of rows. default = 0}

\item{ncol}{number of columns}
}
\value{
data frame populated with nrows and ncols of NAs
}
\description{
Creates an empty data frame with nrows and ncols.
}
\examples{
data.frame.empty(4, 5)

}
