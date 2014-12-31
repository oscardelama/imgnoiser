context("vvm-new")

# Reset the package options from previous tests
imgnoiser.options.force.reset()

test_that("A vvm object can be constructed without parameters", {
  # The channels are named "channel 1" "channel 2"...
  # With no green channels identification
  expect_is(vvm$new(), 'vvm')
})

test_that("A vvm object can be constructed specifying channel labels", {
  expect_is(vvm$new(channel.labels = c("greenB","blue","red","greenR")), 'vvm')
})

test_that("A vvm object can be constructed with a vector of named elements", {
  expect_is(vvm$new(c(a="greenB",b="blue",c="red",d="greenR")), 'vvm')
})

test_that("A vvm construction requires exactly 4 channel labels", {
  expect_error(vvm$new(c('ch1')))
  expect_error(vvm$new(c('ch1', 'ch2')))
  expect_error(vvm$new(c('ch1', 'ch2', 'ch3')))
  expect_error(vvm$new(c('ch1', 'ch2', 'ch3', 'ch4', 'ch5')))
})

test_that("A vvm object can be constructed specifying also the green channel indices", {
  expect_is(vvm$new(green.channels=c(4,1)), 'vvm')
})

test_that("The green channel indices must be between 1 and 4", {
  expect_error(vvm$new(green.channels=c(2,5)))
  expect_error(vvm$new(green.channels=c(0,2)))
  expect_error(vvm$new(green.channels=c(0,5)))
})

test_that("There must be exactly two green channels", {
  expect_error(vvm$new(green.channels=c(2,3,4)))
  expect_error(vvm$new(green.channels=c(1)))
})

test_that("The average green channel can have a customized label", {
  expect_is(vvm$new(avg.green.label='Avg Green'), 'vvm')
})

test_that("The average green can have only one customized label", {
  expect_error(vvm$new(avg.green.label=c('Avg Green','Other Label')))
})

test_that("The channel labels are inferred from a RGGB Bayer pattern", {
  vvm <- vvm$new(has.RGGB.pattern=TRUE)
  expect_is(vvm , 'vvm')
  # Inferred 4 channels plus green average
  expect_true(length(vvm$channel.labels) == 5)
  # Infrerred green channels
  expect_true(length(vvm$green.channels) == 2)
})
