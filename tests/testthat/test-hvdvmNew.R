context("hvdvm New: Construction of hvdvm objects")

test_that("hvdvm objects can be constructed without parameters", {
  expect_is(hvdvm$new(), 'hvdvm')
})

test_that("hvdvm objects can be constructed specifying unnamed channel labels", {
  expect_is(hvdvm$new(c("greenB","blue","red","greenR")), 'hvdvm')
})

test_that("hvdvm objects can be constructed specifying named channel labels", {
  expect_is(hvdvm$new(c(a="greenB",b="blue",c="red",d="greenR")), 'hvdvm')
})

test_that("hvdvm construction requires exactly 4 channel labels", {
  expect_error(hvdvm$new(character()))
  expect_error(hvdvm$new(c('ch1')))
  expect_error(hvdvm$new(c('ch1', 'ch2')))
  expect_error(hvdvm$new(c('ch1', 'ch2', 'ch3')))
  expect_error(hvdvm$new(c('ch1', 'ch2', 'ch3', 'ch4', 'ch5')))
})

test_that("hvdvm objects can be constructed specifying also the green channel indices", {
  expect_is(hvdvm$new(green.channels=c(4,1)), 'hvdvm')
})

test_that("green channel indices must be between 1 and 4", {
  expect_error(hvdvm$new(green.channels=c(2,5)))
  expect_error(hvdvm$new(green.channels=c(0,2)))
  expect_error(hvdvm$new(green.channels=c(0,5)))
})

test_that("there must be exactly two green channels", {
  expect_error(hvdvm$new(green.channels=c(2,3,4)))
  expect_error(hvdvm$new(green.channels=c(1)))
  expect_error(hvdvm$new(green.channels=c()))
})

test_that("the average green can have a customized label", {
  expect_is(hvdvm$new(avg.green.channel.label='AvgGreen'), 'hvdvm')
})

test_that("the average green can have only one customized label", {
  expect_error(hvdvm$new(avg.green.channel.label=c('AvgGreen','other')))
})
