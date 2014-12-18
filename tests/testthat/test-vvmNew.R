context("vvm New: Construction of vvm objects")

test_that("vvm objects can be constructed without parameters", {
  expect_is(vvm$new(), 'vvm')
})

test_that("vvm objects can be constructed specifying unnamed channel labels", {
  expect_is(vvm$new(c("greenB","blue","red","greenR")), 'vvm')
})

test_that("vvm objects can be constructed specifying named channel labels", {
  expect_is(vvm$new(c(a="greenB",b="blue",c="red",d="greenR")), 'vvm')
})

test_that("vvm construction requires exactly 4 channel labels", {
  expect_error(vvm$new(character()))
  expect_error(vvm$new(c('ch1')))
  expect_error(vvm$new(c('ch1', 'ch2')))
  expect_error(vvm$new(c('ch1', 'ch2', 'ch3')))
  expect_error(vvm$new(c('ch1', 'ch2', 'ch3', 'ch4', 'ch5')))
})

test_that("vvm objects can be constructed specifying also the green channel indices", {
  expect_is(vvm$new(green.channels=c(4,1)), 'vvm')
})

test_that("green channel indices must be between 1 and 4", {
  expect_error(vvm$new(green.channels=c(2,5)))
  expect_error(vvm$new(green.channels=c(0,2)))
  expect_error(vvm$new(green.channels=c(0,5)))
})

test_that("there must be exactly two green channels", {
  expect_error(vvm$new(green.channels=c(2,3,4)))
  expect_error(vvm$new(green.channels=c(1)))
  expect_error(vvm$new(green.channels=c()))
})

test_that("the average green can have a customized label", {
  expect_is(vvm$new(avg.green.channel.label='AvgGreen'), 'vvm')
})

test_that("the average green can have only one customized label", {
  expect_error(vvm$new(avg.green.channel.label=c('AvgGreen','other')))
})
