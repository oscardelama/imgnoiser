# Force linear interpolation from 0 to max
linear.interpolation <- function(slope, input.max.linear, num.of.linear.points) {

  segment.count <- num.of.linear.points -1
  x <- (0:segment.count)/segment.count * input.max.linear
  data.frame('src' = x, 'dst' = x * slope)

}

#----------------------------
#'
#' @importFrom data.table rbindlist
#----------------------------
build.generic.gamma.curve <- function(gamma, gamma.slope, gamma.intercept, linear.slope, input.max.linear, num.of.linear.points, num.of.points) {

  num.of.points <- num.of.points - 1L
  start.point <- ceiling(input.max.linear*num.of.points)
  gamma.curve <- data.frame('src' = (start.point:num.of.points)/num.of.points, 'dst' = 0)

  power <- 1/gamma
  for (ix in 1L:nrow(gamma.curve)) {
    x <- gamma.curve[ix,1L]
    gamma.curve[ix,2L] <- gamma.slope*x^power + gamma.intercept
  }

  if (input.max.linear > 0) {
    # Build the initial linear segment
    num.of.linear.points <- max(num.of.linear.points, start.point - 1L)
    linear.segment <- linear.interpolation(linear.slope, input.max.linear, num.of.linear.points)

    # Result: merge linear and gamma segments
    data.table::rbindlist(list(linear.segment, gamma.curve));
  } else
    gamma.curve
}

prepare.sRGB.gamma.curve <- function() {

  build.generic.gamma.curve(
    gamma = 2.4,
    gamma.slope = 1.055,
    gamma.intercept = -0.055,
    linear.slope = 12.92,
    input.max.linear = 0.0031308,
    num.of.linear.points = 5L,
    num.of.points = 41L
  )
}

prepare.ProPhoto.gamma.curve <- function() {

  build.generic.gamma.curve(
    gamma = 1.8,
    gamma.slope = 1,
    gamma.intercept = 0,
    linear.slope = 16,
    input.max.linear = 0.001953,
    num.of.linear.points = 5L,
    num.of.points = 41L
  )
}

prepare.BT.709.gamma.curve <- function() {

  build.generic.gamma.curve(
    gamma = 1/0.45,
    gamma.slope = 1.099,
    gamma.intercept = -0.099,
    linear.slope = 4.5,
    input.max.linear = 0.018,
    num.of.linear.points = 5,
    num.of.points = 41
  )
}

prepare.std.2.2.gamma.curve <- function() {

  build.generic.gamma.curve(
    gamma = 2.2,
    gamma.slope = 1,
    gamma.intercept = 0,
    linear.slope = 1,
    input.max.linear = 0,
    num.of.linear.points = 0,
    num.of.points = 41
  )
}

prepare.std.1.8.gamma.curve <- function() {

  build.generic.gamma.curve(
    gamma = 1.8,
    gamma.slope = 1,
    gamma.intercept = 0,
    linear.slope = 1,
    input.max.linear = 0,
    num.of.linear.points = 0,
    num.of.points = 41
  )
}

prepare.and.save.gamma.curves <- function() {

  sRGB.gamma.curve <- prepare.sRGB.gamma.curve()
  save(sRGB.gamma.curve, file = './data/sRGB-gamma-curve.rdata')
  sRGB.gamma.curve <- NULL

  ProPhoto.gamma.curve <- prepare.ProPhoto.gamma.curve()
  save(ProPhoto.gamma.curve, file = './data/ProPhoto-gamma-curve.rdata')
  ProPhoto.gamma.curve <- NULL

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


#-- Prepare a smooth spline multiplying both tone curves
# They must have the same scale
# returns a spline object
#
#' @importFrom data.table setnames
prepare.merged.tone.curve <- function(tc1, tc2, scale) {

#   tone.curve <- tone.curve * rgb.scale
  spline.of <- function(tc) {
    if (is.null(tc)) return(NULL)
    data.table::setnames(tc, c('x', 'y'))
    smooth.spline(x = tc$x, y = tc$y, all.knots = TRUE)
  }

  sp2 <- spline.of(tc2*scale)

  if (is.null(tc1)) {
    sp2
  } else if (is.null(tc2)) {
    spline.of(tc1*scale)
  } else {

    tc <- data.frame(
          'x' = tc1$x*scale
          ,'y' = predict(sp2, tc1$y)[['y']]
      )

    spline.of(tc);
  }
}
