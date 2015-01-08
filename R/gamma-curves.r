
prepare.sRGB.gamma.curve <- function() {
  n <- 39L
  gamma.curve <- data.frame('x' = (0:n)/n, 'y' = 0)
  power <- 1/2.4

  for (ix in 1L:nrow(gamma.curve)) {
    x <- gamma.curve[ix,1]
    if (x < 0.0031308)
      gamma.curve[ix,2] <- 12.92*x
    else
      gamma.curve[ix,2] <- 1.055*x^power - 0.055
  }
  # result
  gamma.curve;
}

prepare.BT.709.gamma.curve <- function() {

  n <- 39L
  gamma.curve <- data.frame('x' = (0:n)/n, 'y' = 0)
  power <- 0.45

  for (ix in 1L:nrow(gamma.curve)) {
    x <- gamma.curve[ix,1]
    if (x < 0.018)
      gamma.curve[ix,2] <- 4.5*x
    else
      gamma.curve[ix,2] <- 1.099*x^power - 0.099
  }
  # result
  gamma.curve;

}

prepare.std.2.2.gamma.curve <- function() {

  n <- 39L
  gamma.curve <- data.frame('x' = (0:n)/n, 'y' = 0)
  power <- 1/2.2

  for (ix in 1L:nrow(gamma.curve)) {
    x <- gamma.curve[ix,1]
    gamma.curve[ix,2] <- x^power
  }
  # result
  gamma.curve;

}

prepare.std.1.8.gamma.curve <- function() {

  n <- 39L
  gamma.curve <- data.frame('x' = (0:n)/n, 'y' = 0)
  power <- 1/1.8

  for (ix in 1L:nrow(gamma.curve)) {
    x <- gamma.curve[ix,1]
    gamma.curve[ix,2] <- x^power
  }
  # result
  gamma.curve;

}

prepare.and.save.gamma.curves <- function() {

  sRGB.gamma.curve <- prepare.sRGB.gamma.curve()
  save(sRGB.gamma.curve, file = './data/sRGB-gamma-curve.rdata')
  sRGB.gamma.curve <- NULL

  BT.709.gamma.curve <- prepare.BT.709.gamma.curve()
  save(BT.709.gamma.curve, file = './data/BT-709-gamma-curve.rdata')
  BT.709.gamma.curve <- NULL

  std.2.2.gamma.curve <- prepare.std.2.2.gamma.curve()
  save(std.2.2.gamma.curve, file = './data/std-2.2-gamma-curve.rdata')
  std.2.2.gamma.curve <- NULL

  std.1.8.gamma.curve <- prepare.std.1.8.gamma.curve()
  save(std.1.8.gamma.curve, file = './data/std-1.8-gamma-curve.rdata')
  std.1.8.gamma.curve <- NULL
}
