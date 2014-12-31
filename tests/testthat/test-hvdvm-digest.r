context("hvdvm-digest")

# This tests are avoided in CRAN tests because (a) requires a folder
# with image samples which I don't know how to manage for the CRAN tests
# (b) the tests might take some time to processing
# testthat::skip_on_cran()

#-------------
test.data.folder <- imgnoiser:::.get.test.data.folder()
photo.conds.file <- paste0(test.data.folder,'iso100White.csv')

# Retun the range of the number of rows with the same condition
cond.range.after.digest <- function(hvdvm.obj, photo.conds) {
  #Turn off progress bar
  imgnoiser.option('show.progress', FALSE)

  # process n samples
  suppressMessages(
    hvdvm.obj$digest(photo.conds, test.data.folder)
  )
  # Return the renge of condition values
  range(table(hvdvm.obj$var.df$cond))
}

get.photo.conds <- function(nrows) {
  photo.conds.df <- read.csv(photo.conds.file, stringsAsFactors=FALSE)
  photo.conds.df[1:nrows,]
}
#-------------

test_that("All channel labels are given, process all rows", {
  # Create hvdvm object
  nikon.channel.names <- c("greenB","blue","red","greenR")
  hvdvm.obj <- hvdvm$new(green.channels= c(1,4), channel.labels=nikon.channel.names)

  # There must be between 3*4 to 1*4 rows per condition
  # The digest argument is the file name
  rng <- cond.range.after.digest(hvdvm.obj, photo.conds.file)
  expect_true(rng[1] == 4)
  expect_true(rng[2] == 12)

  expect_true(nrow(hvdvm.obj$var.df) == 320)
  expect_true(nrow(hvdvm.obj$cov.df) == 480)
})

test_that("All channel labels inferred by RGGB pattern", {
  # Set has RGGB pattern as default
  imgnoiser.option('has.RGGB.pattern', TRUE)
  # Creation does not require any parameter
  hvdvm.obj<- hvdvm$new()

  photo.conds.df <- get.photo.conds(84)
  # There must be between 15*4 to 10*4 rows per condition
  # The digest argument is the file content
  rng <- cond.range.after.digest(hvdvm.obj, photo.conds.df)
  expect_true(rng[1] == 4)
  expect_true(rng[2] == 12)

  expect_true(nrow(hvdvm.obj$var.df) == 320)
  expect_true(nrow(hvdvm.obj$cov.df) == 480)
})

test_that("Channel labels inferred by RGGB pattern, but Avg green not desired", {
  # green.channels equal to NA signals not desired green average
  hvdvm.obj <- hvdvm$new(has.RGGB.pattern=TRUE, green.channels=NA)

  photo.conds.df <- get.photo.conds(84)
  # There must be between 15*4 to 10*4 rows per condition
  # The digest argument is the file content
  rng <- cond.range.after.digest(hvdvm.obj, photo.conds.df)
  expect_true(rng[1] == 4)
  expect_true(rng[2] == 12)

  expect_true(nrow(hvdvm.obj$var.df) == 320)
  expect_true(nrow(hvdvm.obj$cov.df) == 480)
})

test_that("The hvdvm$digest can receive as argument a data frame with the photo conditions", {

  nikon.channel.names <- c("greenB","blue","red","greenR")
  hvdvm.obj <- hvdvm$new(channel.labels=nikon.channel.names)

  photo.conds.df <- get.photo.conds(84)
  # There must be between 15*4 to 10*4 rows per condition
  # The digest argument is the file content
  rng <- cond.range.after.digest(hvdvm.obj, photo.conds.df)
  expect_true(rng[1] == 4)
  expect_true(rng[2] == 12)

  expect_true(nrow(hvdvm.obj$var.df) == 320)
  expect_true(nrow(hvdvm.obj$cov.df) == 480)
})
