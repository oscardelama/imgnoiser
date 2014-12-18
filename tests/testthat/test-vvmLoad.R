context("vvm load")

global.vvm <- NULL

test_that("vvm digest method", {

  nikon.channel.names <- c("greenB","blue","red","greenR")
  vvm <- vvm$new(green.channels= c(1,4), channel.labels=nikon.channel.names)

  #Turn off progress bar
  set_option('show.progress', FALSE)

  vvm$digest('crop_', 245, '../../data-raw/')
  row.count <- nrow(vvm$get.var())
  expect_true(row.count > 10)

  global.vvm <<- vvm

})

test_that("vvm fit model", {
  expect_is(global.vvm$fit.model(), 'vvm')
})

test_that("vvm print model summary", {
  expect_output(global.vvm$print.model.summary(),'(.*Channel.*Call.*Coeff.*){4,5}')
})

test_that("vvm plot", {
  expect_match(class(global.vvm$plot()), 'gg.*')
})
