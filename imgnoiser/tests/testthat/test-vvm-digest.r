context("vvm-digest")

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
  n <- 12
  # There must be 5 channel values per sample:
  # four channels plus green average.
  expect_true(var.len.after.digest(n, vvm.obj) == n*5)
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
})

test_that("Channel labels given but not Green average label", {

  nikon.channel.names <- c("greenB","blue","red","greenR")
  vvm.obj <- vvm$new(channel.labels=nikon.channel.names)

  # process n samples
  n <- 12
  # There must be 4 channel values per sample
  # No green average.
  expect_true(var.len.after.digest(n, vvm.obj) == n*4)
})
