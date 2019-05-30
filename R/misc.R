#' Empty Data Frame
#'
#' Creates an empty data frame with nrows and ncols.
#' 
#' @param nrow number of rows. default = 0
#' @param ncol number of columns
#' 
#' @return data frame populated with nrows and ncols of NAs
#' 
#' @examples 
#' data.frame.empty(4, 5)
#' 
#' @export
data.frame.empty <- function(nrow = 0, ncol) {
  return(data.frame(matrix(nrow = nrow, ncol = ncol)))
}


#' Try \code{\link{source}} with multiple files
#'
#' \code{\link{source}}s the first file that exists in a vector of files. 
#' 
#' @param multiPath vector of character strings representing inputs to \code{\link{source}}
#' 
#' @return index of the path in multiPath that was sourced
#' 
#' @examples 
#' source.multi(c("/path/on/network", "/path/on/local"))
#' 
#' @export
source.multi <- function(multiPath) {
  for (i in 1:length(multiPath)) {
    curPath <- multiPath[[i]]
    if (file.exists(curPath)) {
      source(curPath)
      cat(sprintf('executed: source("%s")\n', curPath))
      return(i)
    }
  }
  stop("None of the files exist")
}


#' Try \code{\link{setwd}} with multiple files
#'
#' \code{\link{setwd}}s the first dir that exists in a vector of dir 
#' 
#' @param multiPath vector of characters representing inputs to \code{\link{setwd}}
#' 
#' @return index of dir in multiPath that was set as the working directory
#' 
#' @examples 
#' setwd.multi(c("/path/on/network", "/path/on/local"))
#' 
#' @export
setwd.multi <- function(multiPath) {
  for (i in 1:length(multiPath)) {
    curPath <- multiPath[[i]]
    if (dir.exists(curPath)) {
      setwd(curPath)
      cat(sprintf('executed: setwd("%s")\n', curPath))
      return(i)
    }
  }
  stop("None of the directories exist")
}


#' Datestamp character string
#' 
#' Add a datestamp to a character string, with considerations for filenames
#' 
#' @param filenameOrSomething character string to be datestamped
#' @param location c("beforeFileExt", "start", "end"). where the datastamp should be inserted. 
#' default is before the file extension ("beforeFileExt")
#' @param sep character that separates the datastamp and the original characters. 
#' default = "."
#' @param dateFormat character formatting string. see \code{\link{format.POSIXct}} for more info.
#' 
#' @return datastamped character string
#' 
#' @examples
#' datestamp("salad.pdf")    # "salad.20181211_1005.pdf"
#' datestamp("sample", location="end", sep=" ", dateFormat = "at %H%M")    #"sample at 1005"
#' 
#' @export
datestamp <- function(filenameOrSomething, location = c("beforeFileExt", "start", "end"), 
                      sep = ".", dateFormat = "%Y%m%d_%H%M") {
  
  if (location[1] == "beforeFileExt" && length(grep("[.]", filenameOrSomething)) == 0) {
    location[1] <- "end"
  }
  
  stamp_date <- format(Sys.time(), dateFormat)
  if (location[1] == "beforeFileExt") {
    insert <- sprintf("%s%s.\\1", sep, stamp_date)
    datestamped <- sub("[.]([^.]+$)", insert, filenameOrSomething )
  } else if (location[1] == "start" || location[1] == "beginning") {
    datestamped <- sprintf("%s%s%s", stamp_date, sep, filenameOrSomething)
  } else if (location[1] == "end") {
    datestamped <- sprintf("%s%s%s", filenameOrSomething, sep, stamp_date)
  } else {
    stop("Not a valid date-stamp-inserting location")
  }
  
  return(datestamped)
}


#' \code{\link{pdf}} wrapper with datestamp
#'
#' A wrapper of the \code{\link{pdf}} function that automatically datestamps the filename.
#' Currently no support for multiple files (i.e.: no support for onefile = FALSE)
#'
#' @export
pdf.date <- function(file = "Rplots.pdf", dateFormat = "%Y%m%d_%H%M",
                     width, height, family, title, fonts, version,
                     paper, encoding, bg, fg, pointsize, pagecentre, colormodel,
                     useDingbats, useKerning, fillOddEven, compress) {
  
  dated_file <- datestamp(file)
  pdf(file = dated_file,
      width, height, onefile = TRUE, family, title, fonts, version,
      paper, encoding, bg, fg, pointsize, pagecentre, colormodel,
      useDingbats, useKerning, fillOddEven, compress)
}


#' \code{\link{png}} wrapper with datestamp
#'
#' A wrapper of the \code{\link{png}} function that automatically datestamps the filename,
#' and uses a default of 10 inches by 10 inches at 300 dpi for image size
#'
#' @export
png.date <- function (filename = "Rplot%03d.png", dateFormat = "%Y%m%d_%H%M", width = 10, height = 10, 
                      units = "in", pointsize = 12, bg = "white", res = 300, 
                      type = c("cairo", "cairo-png", "Xlib", "quartz"), antialias = "none") {
  dated_file <- datestamp(filename)
  png(filename = dated_file, 
      width = width, height = height, units = units, pointsize = pointsize, 
      bg = bg, res = res, type = type, antialias = antialias)
}
  