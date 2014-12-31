context("hvdvm-fit.model")

# This tests are avoided in CRAN tests because (a) requires a folder
# with image samples which I don't know how to manage for the CRAN tests
# (b) the tests might take some time to processing
# testthat::skip_on_cran()

suppressPackageStartupMessages(require(imgnoiser))

#-------------
global.hvdvm.obj <- NULL

test.data.folder <- imgnoiser:::.get.test.data.folder()
photo.conds.file <- paste0(test.data.folder,'iso100White.csv')

# Digest the images silently
digest.silently <- function(hvdvm.obj, photo.conds) {
  #Turn off progress bar
  imgnoiser.option('show.progress', FALSE)

  # process n samples
  suppressMessages(
    hvdvm.obj$digest(photo.conds, test.data.folder)
  )
}

get.photo.conds <- function(nrows) {
  photo.conds.df <- read.csv(photo.conds.file, stringsAsFactors=FALSE)
  photo.conds.df[1:nrows,]
}

get.summ.output.one.line <- function(obj, ...) {
  model.summary <- capture.output(obj$print.model.summary(...))
  paste(model.summary, sep="", collapse="")
}
#-------------

test_that("A linear model summary is printed per each channel", {
  # The samples has a RGGB pattern
  global.hvdvm.obj <<- hvdvm$new(has.RGGB.pattern = TRUE)

  photo.conds.df <- get.photo.conds(30)
  # There must be between 15*4 to 10*4 rows per condition
  # The digest argument is the file name
  digest.silently(global.hvdvm.obj, photo.conds.file)

  # Fit the standard model.
  suppressMessages(
    global.hvdvm.obj$fit.model()
  )
  model.summary <- get.summ.output.one.line(global.hvdvm.obj)

  # Expected one set of regression coefficients per channel
  expect_output(paste(model.summary), '(.*Coefficients){4}')
})

test_that("There are at least 30 model predictions per channel", {
  # There are at least 30*4 predictions
  expect_true(nrow(global.hvdvm.obj$get.model.predictions()) >= 120 )
})

test_that("You can get a model summary for the chosen channels", {
  model.summary <- get.summ.output.one.line(global.hvdvm.obj, select=c(2,4))

  # Expected one set of regression coefficients per channel
  expect_output(paste(model.summary), '(.*Coefficients){2}')
})


test_that("You can get all (4) the model objects", {
  models <- global.hvdvm.obj$get.model()

  # Expected one model object per channel
  expect_true(length(models) == 4)
})


test_that("You can get only the selected model object", {
  models <- global.hvdvm.obj$get.model(select = 'Red')

  # Expected one regression: for the Red channel
  expect_true(length(models) == 1)
})


test_that("The created model exists inside the object", {
  # The standard model exists
  suppressMessages(
    expect_true(global.hvdvm.obj$exists.model())
  )
})


test_that("The model list contains one entry", {
  # Expected one fitted model
  expect_true(length(global.hvdvm.obj$model.list) == 1)
})


test_that("If I remove the only existing model, the model list becomes empty", {
  suppressMessages(
    global.hvdvm.obj$remove.model('standard')
  )
  # Expected no fitted model
  expect_true(length(global.hvdvm.obj$model.list) == 0)
})

