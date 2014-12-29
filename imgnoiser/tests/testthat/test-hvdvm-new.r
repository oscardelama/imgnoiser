context("hvdvm-new")

# Reset the package options from previous tests
imgnoiser.options.force.reset()

test_that("A hvdvm object can be constructed without parameters", {
  # The channels are named "channel 1" "channel 2"...
  # With no green channels identification
  expect_is(hvdvm$new(), 'hvdvm')
})

test_that("A hvdvm object can be constructed specifying channel labels", {
  expect_is(hvdvm$new(channel.labels = c("greenB","blue","red","greenR")), 'hvdvm')
})

test_that("A hvdvm object can be constructed with a vector of named elements", {
  expect_is(hvdvm$new(c(a="greenB",b="blue",c="red",d="greenR")), 'hvdvm')
})

test_that("A hvdvm construction requires exactly 4 channel labels", {
  expect_error(hvdvm$new(c('ch1')))
  expect_error(hvdvm$new(c('ch1', 'ch2')))
  expect_error(hvdvm$new(c('ch1', 'ch2', 'ch3')))
  expect_error(hvdvm$new(c('ch1', 'ch2', 'ch3', 'ch4', 'ch5')))
})

test_that("A hvdvm object can be constructed specifying also the green channel indices", {
  expect_is(hvdvm$new(green.channels=c(4,1)), 'hvdvm')
})

test_that("The green channel indices must be between 1 and 4", {
  expect_error(hvdvm$new(green.channels=c(2,5)))
  expect_error(hvdvm$new(green.channels=c(0,2)))
  expect_error(hvdvm$new(green.channels=c(0,5)))
})

test_that("There must be exactly two green channels", {
  expect_error(hvdvm$new(green.channels=c(2,3,4)))
  expect_error(hvdvm$new(green.channels=c(1)))
})

test_that("The average green channel can have a customized label", {
  expect_is(hvdvm$new(avg.green.label='Avg Green'), 'hvdvm')
})

test_that("The average green channel can have only one customized label", {
  expect_error(hvdvm$new(avg.green.label=c('Avg Green','Other Label')))
})

test_that("The channel labels are inferred from a RGGB Bayer pattern", {
  hvdvm <- hvdvm$new(has.RGGB.pattern=TRUE)
  expect_is(hvdvm , 'hvdvm')
  # Inferred 4 channels plus green average
  expect_true(length(hvdvm$channel.labels) == 5)
  # Infrerred green channels
  expect_true(length(hvdvm$green.channels) == 2)
})

