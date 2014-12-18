context("hvdvm load")

global.hvdvm <- NULL

test_that("hvdvm digest method", {

  nikon.channel.names <- c("greenB","blue","red","greenR")
  hvdvm <- hvdvm$new(green.channels= c(1,4), channel.labels=nikon.channel.names)

  #Turn off progress bar
  set_option('show.progress', FALSE)

  hvdvm$digest('../../data-raw/iso100White.csv', '../../data-raw/')
  row.count <- nrow(hvdvm$get.var())
  expect_true(row.count > 10)

  global.hvdvm <<- hvdvm

})

test_that("hvdvm fit model", {
  expect_is(global.hvdvm$fit.model(), 'hvdvm')
})

test_that("hvdvm print model summary", {
  expect_output(global.hvdvm$print.model.summary(),'(.*Channel.*Call.*Coeff.*){4,5}')
})

test_that("hvdvm plot", {
  expect_match(class(global.hvdvm$plot()), 'gg.*')
})
