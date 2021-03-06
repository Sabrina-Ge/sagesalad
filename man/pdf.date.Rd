% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{pdf.date}
\alias{pdf.date}
\title{\code{\link{pdf}} wrapper with datestamp}
\usage{
pdf.date(file = "Rplots.pdf", dateFormat = "\%Y\%m\%d_\%H\%M", width,
  height, family, title, fonts, version, paper, encoding, bg, fg,
  pointsize, pagecentre, colormodel, useDingbats, useKerning, fillOddEven,
  compress)
}
\description{
A wrapper of the \code{\link{pdf}} function that automatically datestamps the filename.
Currently no support for multiple files (i.e.: no support for onefile = FALSE)
}
