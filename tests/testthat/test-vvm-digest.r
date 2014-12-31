context("vvm-digest")

# This tests are avoided in CRAN tests because (a) requires a folder
# with image samples which I don't know how to manage for the CRAN tests
# (b) the tests might take some time to processing
# testthat::skip_on_cran()

#-------------
test.data.folder <- imgnoiser:::.get.test.data.folder()

var.len.after.digest <- function(n, vvm.obj) {
  #Turn off progress bar
  imgnoiser.option('show.progress', FALSE)

  # process n samples
  suppressMessages(
    vvm.obj$digest('crop_', n, test.data.folder)
  )
  nrow(vvm.obj$var.df)
}
#-------------

test_that("All channel labels are given", {
  # Create vvm object
  nikon.channel.names <- c("greenB","blue","red","greenR")
  vvm.obj <- vvm$new(green.channels= c(1,4), channel.labels=nikon.channel.names)

  # process n samples
  n <- 84
  # There must be 5 channel values per sample:
  # four channels plus green average.
  expect_true(var.len.after.digest(n, vvm.obj) == n*5)
  expect_true(nrow(vvm.obj$cov.df) == n*9)
})

test_that("All channel labels inferred by RGGB pattern", {
  # Set has RGGB pattern as default
  imgnoiser.option('has.RGGB.pattern', TRUE)
  # Creation does not require any parameter
  vvm.obj <- vvm$new()

  # process n samples
  n <- 12
  # There must be 5 channel values per sample:
  # four channels plus green average.
  expect_true(var.len.after.digest(n, vvm.obj) == n*5)
  expect_true(nrow(vvm.obj$cov.df) == n*9)
})

# Reset the package options from previous tests
imgnoiser.options.force.reset()

test_that("Channel labels inferred by RGGB pattern, but Avg green not desired", {
  # green.channels equal to NA signals not desired green average
  vvm.obj <- vvm$new(has.RGGB.pattern=TRUE, green.channels=NA)

  # process n samples
  n <- 12
  # There must be 4 channel values per sample
  # No green average.
  expect_true(var.len.after.digest(n, vvm.obj) == n*4)
  expect_true(nrow(vvm.obj$cov.df) == n*6)
})

test_that("Channel labels given but not Green average label", {

  nikon.channel.names <- c("greenB","blue","red","greenR")
  vvm.obj <- vvm$new(channel.labels=nikon.channel.names)

  # process n samples
  n <- 12
  # There must be 4 channel values per sample
  # No green average.
  expect_true(var.len.after.digest(n, vvm.obj) == n*4)
  expect_true(nrow(vvm.obj$cov.df) == n*6)
})

test_that("The vvm$digest function can receive as argument an explicit list of sample file names", {

  vvm.obj <- vvm$new(has.RGGB.pattern = TRUE)

  # process n samples
  n <- 16
  sample.file.names <- paste0('crop_', 1:n, '.fit')
  # There must be 5 channel values per sample
  # including green average.
  suppressMessages(
    vvm.obj$digest(sample.file.names, file.path = test.data.folder)
  )
  expect_true(nrow(vvm.obj$var.df) == n*5)
  expect_true(nrow(vvm.obj$cov.df) == n*9)
})
