library(testthat)

source("./misc.R")

context("datestamp")

# TODO: figure out a better way to test this without rewriting a function
now <- as.POSIXct("2012-12-12 12:12:12 EST")
backup.Sys.time <- Sys.time
Sys.time <- function() {return(now)}

test_that("datestamp works with default parameters", {
  expect_that(datestamp("cat.pdf"), equals("cat.20121212_1212.pdf"))
  expect_that(datestamp("potato.cat.pdf"), equals("potato.cat.20121212_1212.pdf"))
  expect_that(datestamp("cat"), equals("cat.20121212_1212"))
})

test_that("datestamp with different location parameters", {
  expect_that(datestamp("cat.pdf", location = "beforeFileExt"), equals("cat.20121212_1212.pdf"))
  expect_that(datestamp("cat.pdf", location = "start"), equals("20121212_1212.cat.pdf"))
  expect_that(datestamp("cat.pdf", location = "end"), equals("cat.pdf.20121212_1212"))
})

test_that("datestamp with different sep parameters", {
  expect_that(datestamp("cat.pdf", sep = "_"), equals("cat_20121212_1212.pdf"))
  expect_that(datestamp("cat.pdf", location = "start", sep = " "), equals("20121212_1212 cat.pdf"))
  expect_that(datestamp("cat.pdf", location = "end", sep = "catfish"), equals("cat.pdfcatfish20121212_1212"))
})

test_that("datestamp with different date format parameters", {
  expect_that(datestamp("cat.pdf", sep = "_", dateFormat = "%Y%m%d_%H%M"), equals("cat_20121212_1212.pdf"))
  expect_that(datestamp("cat.pdf", location = "start", sep = " ", dateFormat = "...%H%M "), equals("...1212  cat.pdf"))
})

test_that("datestamp with invalid input", {
  expect_error(datestamp("cat.pdf", location = "somewhere"))
  expect_error(datestamp())
})

#Sys.time <- backup.Sys.time
rm(Sys.time)

##### Functions used in development ####

time_datestamp <- function() {
  filename <- "poatao.test.pdf"
  dateFormat = "%Y%m%d_%H%M"
  stamp_date <- format(Sys.time(), dateFormat)
  times <- data.frame.empty(4, 5)
  times[1,] <- system.time({for (i in 1:2000) {
    tokens <- unlist(strsplit(filename, "[.]"))
    dated_file <- sprintf("%s.%s.%s", paste0(tokens[1:length(tokens) - 1], collapse="."), 
                          stamp_date, 
                          tokens[length(tokens)])
  }})
  times[2,] <- system.time({for (i in 1:2000) {
    tokens <- unlist(strsplit(filename, "[.]"))
    dated_file <- paste(paste0(tokens[1:length(tokens) - 1], collapse="."), 
                        stamp_date, 
                        tokens[length(tokens)], sep=".")
  }})
  times[3,] <- system.time({for (i in 1:2000) {
    insert <- paste0(".", stamp_date, ".\\1")
    dated_file <- sub("[.]([^.]+$)", insert, filename )
  }})
  times[4,] <- system.time({for (i in 1:2000) {
    insert <- sprintf(".%s.\\1", stamp_date)
    dated_file <- sub("[.]([^.]+$)", insert, filename )
  }})
  return(times)
}
