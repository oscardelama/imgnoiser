#-----------------------------------------------------------------------------
#     imgnoiser R package
#     Copyright (C) 2014  Oscar de Lama del Rio
#
#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 2 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License along
#     with this program; if not, write to the Free Software Foundation, Inc.,
#     51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#-----------------------------------------------------------------------------

#-------------------------------------------
# Planckian locus in xy 1931 space
#
# The formula used here was published in the Journal of the Korean Physical
# Society, Vol. 41, No. 6, December 2002, pp. 865-871
#
# "Design of Advanced Color -
# Temperature Control System for HDTV Applications"
#
# By Bongsoon Kang, ∗ Ohak Moon and Changhee Hong
# and Honam Lee, Bonghwan Cho and Youngsun Kim
#
#' @export
xy.1931.from.temp.planckian <- function(temp.k) {

  if (temp.k < 1667 | temp.k > 25000)
    stop('The temperature must be in the [1667, 25000] Kelvin range.')

  if (temp.k < 4000)
    x <- ((-0.2661239E+09/temp.k - 0.2343589E+06)/temp.k + 0.8776956E+03)/temp.k + 0.179910
  else
    x <- ((-3.0258469E+09/temp.k + 2.1070379E+06)/temp.k + 0.2226347E+03)/temp.k + 0.24039

  if (temp.k >= 4000)
    y <- ((3.0817580*x - 5.8733867)*x + 3.75112997)*x - 0.37001483
  else if (temp.k >= 2222)
    y <- ((-0.9549476*x - 1.37418593)*x + 2.09137015)*x - 0.16748867
  else
    y <- ((-1.1063814*x - 1.3481102)*x + 2.18555832)*x - 0.20219683

  # Result
  c(x,y);
}

#-------------------------------------------
# Planckian locus in uv 1960 UCS space
#
# Published in Krystek, Michael P. (January 1985).
# "An algorithm to calculate correlated colour temperature".
# Color Research & Application
#
# by  Krystek, Michael P
#
#' @export
uv.1960.from.temp.planckian <- function(temp.k) {

  if (temp.k < 1000 | temp.k > 15000)
    stop('The temperature must be in the [1000, 15000] Kelvin range.')

  u <- (0.860117757 + (1.54118254E-04 + 1.28641212E-07 * temp.k) * temp.k) /
       (1 + (8.42420235E-04 + 7.08145163E-07 * temp.k) * temp.k)

  v <- (0.317398726 + (4.22806245E-05 + 4.20481691E-08 * temp.k) * temp.k) /
       (1 + (-2.89741816E-05 + 1.61456053E-07 * temp.k) * temp.k)
  #result
  c(u,v)
}

#-------------------------------------------
# Convert from xy 1931 to uv 1960
# from C. Poynton:
# http://www.poynton.com/PDFs/coloureq.pdf
# pg. 9
#
#' @export
uv.1960.from.xy.1931 <- function(xy) {
  denom <- 6*xy[2L] - xy[1L] + 1.5
  u <- (2*xy[1L]) / denom
  v <- (3*xy[2L]) / denom
  #result
  c(u,v);
}

#-------------------------------------------
# Convert from XYZ 1931 to uv 1960
# Reversing equations in uv.1960.from.xy.1931
# function
#
#' @export
uv.1960.from.XYZ.1931 <- function(XYZ) {
  denom <- XYZ[1L] + 15*XYZ[2L] + 3*XYZ[3L]
  u <- (4*XYZ[1L]) / denom
  v <- (6*XYZ[2L]) / denom
  # Result
  c(u,v);
}

#-------------------------------------------
# Convert from XYZ to xy
# From wikipedia: CIE_1931_color_space
#' @export
#-------------------------------------------
xy.from.XYZ <- function(XYZ) {
  XYZ[1L:2L]/sum(XYZ);
}

#-------------------------------------------
# Convert from xyY to XYZ
# From wikipedia: CIE_1931_color_space
#' @export
#-------------------------------------------
XYZ.from.xyY <- function(xyY) {
  Y <- xyY[3L]
  X <- (Y/xyY[2L])*xyY[1L]
  Z <- (Y/xyY[2L])*(1 - x - y)
  #Result
  c(X, Y, Z);
}
#-------------------------------------------
# Convert from xy 1931 to uv 1960
# Principles of Color Technology
# by  Fred W. Billmeyer pg. 57
#' @export
#-------------------------------------------
xy.1931.from.uv.1960 <- function(uv) {
  denom <- 2*uv[1L] - 8*uv[2L] + 4
  x = (3*uv[1L]) / denom
  y = (2*uv[2L]) / denom
  c(x,y)
}

#-------------------------------------------
# Convert from xy 1931 to CCT
# From the article "Calculating correlated color temperatures across
# the entire gamut of daylight and skylight chromaticities"
# By Javier Hernández-Andrés, Raymond L. Lee, Jr., and Javier Romero
# As available at 2015-Jan from:
# http://www.usna.edu/Users/oceano/raylee/papers/RLee_AO_CCTpaper.pdf
#' @export
#-------------------------------------------
cct.approx.from.xy.1931 <- function(xy) {
  xe <- 0.3366
  ye <- 0.1735
  A0 <- -949.86315
  A1 <- 6253.80338;   t1 <- 0.92159
  A2 <- 28.70599;     t2 <- 0.20039
  A3 <- 0.00004;      t3 <- 0.07125

  n <- (xy[1L] - xe) / (xy[2L] - ye)
  (A0 + A1*exp(-n/t1) + A2*exp(-n/t2) + A3*exp(-n/t3))
}

#' @export
uv.distance.to.planck.locus <- function(temp.k, xy0) {
  if(temp.k < 1667 | temp.k > 15000) {
    message('Out of range')
    return(last.value*8)
  }
  #browser()
  uv1 <- uv.1960.from.xy.1931(xy.1931.from.temp.planckian(temp.k))
  uv2 <- uv.1960.from.temp.planckian(temp.k)
  # mean uv corresponding to the cct
  uv <- c(mean(c(uv1[1L],uv2[1L])), mean(c(uv1[2L],uv2[2L])))
  # result
  delta.uv <- uv.1960.from.xy.1931(xy0) - uv
  sqrt(sum(delta.uv^2L))
}

#-------------------------------------------
# xy.1931.to.cct
#' @export
#-------------------------------------------
planckian.temp.from.xy.1931 <- function(xy, verbosity=0) {

  last.value <- 0

  dist.to.planck.locus <- function(inv.temp.k, xy0) {
    temp.k <- 1 / inv.temp.k
    if(temp.k < 1667 | temp.k > 15000) {
      if (verbosity == 2L) msg('Out of range')
      return(last.value*4)
    }
    #browser()
    last.value <<- uv.distance.to.planck.locus(temp.k, xy0)
    last.value;
  }

  # first approximation (a very good one)
  temp.k <- cct.approx.from.xy.1931(xy)
  dist <- dist.to.planck.locus(1/temp.k, xy)

  if (dist < 1e-04)
    return(temp.k)
  else {
    if (verbosity > 0) msg('Initial estimate:', temp.k)
    # Find the closest point to the planckian locus
    min <- nlm(dist.to.planck.locus, p=c(1/temp.k), xy0=xy, fscale=1e-04, steptol=2e-4, print.level=verbosity)
    # Result
    1/min$estimate
  }
}

#' @export
interp.matrix.by.inv.temp = function(cct.k, mat.a, cct.a, mat.b, cct.b) {

  if ( cct.k < 0.7*cct.a | cct.k > 1.6*cct.b)
    stop("The 'cct.k' argument value must be between the illuminants CCTs.")

  # If the matrices are equal return eny of them
  if (all(mat.a == mat.b)) return(mat.a)

  slope <- (1/cct.k - 1/cct.a)/(1/cct.b - 1/cct.a)
  # Result
  mat.a + (mat.b - mat.a)*slope;
}

#' @export
uv.distance.from.xy.1931 <- function(xy.1, xy.2) {
  uv.1 <- uv.1960.from.xy.1931(xy.1)
  uv.2 <- uv.1960.from.xy.1931(xy.2)
  sqrt(sum((uv.1 - uv.2)^2L));
}

#' @export
XYZ.of.illuminant <- function (illuminant) {

  switch(
    illuminant,

    'A' = c(1.09850,  1.00000,	0.35585),
    'D50' = c(0.96422,  1.00000,	0.82521),
    'D65' = c(0.95047,  1.00000,	1.08883),
    'D55' = c(0.95682,  1.00000,	0.92149),
    'E'   = c(1.00000,  1.00000,	1.00000),

    stop("Unknown Illuminant.")
  )
}

#---------------------------------------
#' Get sRGB from linear tonal values
#'
#' Convert linear tonal values to sRGB.
#'
#' @param linear tonal values in \code{[0, 1]}
#' @return sRGB tonal values in \code{[0, 1]}
#'
#' @export
#---------------------------------------
sRGB.from.linear <- function(linear){
  srgb.lin <- function(linear){
    12.92*linear
  }

  srgb.gamma <- function(linear){
    1.055*linear^(1/2.4) - 0.055
  }

  sRGB.result <- vector("numeric", length = length(linear))
  tones.in.linear.part <- which(linear <= 0.0031308)

  if (length(tones.in.linear.part) == 0) {
    sRGB.result <- srgb.gamma(linear)
  } else {
    sRGB.result[tones.in.linear.part] <- srgb.lin(linear[tones.in.linear.part])
    sRGB.result[-tones.in.linear.part] <- srgb.gamma(linear[-tones.in.linear.part])
  }
  # Clip to zero below numerical precision
  sRGB.result[abs(sRGB.result) < 5e-6] <- 0
  sRGB.result;
}

#---------------------------------------
#' Get linear tonal values from sRGB ones
#'
#' Convert sRGB tonal values to linear ones.
#'
#' @param sRGB tonal values in \code{[0, 1]}
#' @return linear tonal values in \code{[0, 1]}
#'
#' @export
#---------------------------------------
linear.from.sRGB <- function(sRGB) {

  linear.from.sRGB.linear <- function(x) {
    x/12.92;
  }

  linear.from.sRGB.gamma <- function(x) {
    ((x + 0.055)/1.055)^2.4;
  }

  linear.result <- vector("numeric", length = length(sRGB))
  tones.in.linear.part <- which(sRGB <= 0.04045)

  if (length(tones.in.linear.part) == 0) {
    linear.result <- linear.from.sRGB.gamma(sRGB)
  } else  {
    linear.result[tones.in.linear.part] <- linear.from.sRGB.linear(sRGB[tones.in.linear.part])
    linear.result[-tones.in.linear.part] <- linear.from.sRGB.gamma(sRGB[-tones.in.linear.part])
  }

  # Clip to zero below numerical precision
  linear.result[abs(linear.result) < 5e-6] <- 0
  linear.result;
}

#---------------------------------------
#' Get L.Lab tonal values from linear ones
#'
#' Convert linear tonal values to Lighteness (L in Lab).
#'
#' @param linear tonal values in \code{[0, 1]}
#' @return L.Lab tonal values in \code{[0, 100]}
#'
#' @export
#---------------------------------------
L.Lab.from.linear <- function(linear) {
  L.Lab.linear <- function(x) {
    903.2963*x;
  }
  L.Lab.gamma <- function(x) {
    116*x^0.33333333 - 16;
  }

  L.Lab.result <- vector("numeric", length = length(linear))
  tones.in.linear.part <- which(linear <= 0.008856452)

  if (length(tones.in.linear.part) == 0) {
    L.Lab.result <- L.Lab.gamma(linear)
  } else  {
    L.Lab.result[tones.in.linear.part] <- L.Lab.linear(linear[tones.in.linear.part])
    L.Lab.result[-tones.in.linear.part] <- L.Lab.gamma(linear[-tones.in.linear.part])
  }

  # Clip to zero below numerical precision
  L.Lab.result[abs(L.Lab.result) < 5e-6] <- 0
  L.Lab.result;
}

#---------------------------------------
#' Get linear tonal values from L.Lab ones
#'
#' Convert Lighteness (L in Lab) tonal values to linear ones.
#'
#' @param L.Lab tonal values in \code{[0, 100]}
#' @return linear tonal values in \code{[0, 1]}
#'
#' @export
#---------------------------------------
linear.from.L.Lab <- function(L.Lab) {
  l.rev.cubic <- function(x) x^3;
  l.rev.lin <- function(x) 0.1284185*x - 0.0177129;

  # Reverting L to linear
  l <- (L.Lab + 16)/116
  tones.in.linear.part <- which(l <= 0.2068966)
  lin.result <- vector("numeric", length = length(L.Lab))
  if (length(tones.in.linear.part) == 0) {
    lin.result <- l.rev.cubic(l)
  } else {
    lin.result[tones.in.linear.part] <- l.rev.lin(l[tones.in.linear.part])
    lin.result[-tones.in.linear.part] <- l.rev.cubic(l[-tones.in.linear.part])
  }
  # Result
  lin.result;
}

#---------------------------------------
#' Get Adobe RGB tonal values from linear ones
#'
#' Convert linear tonal values to Adobe RGB ones.
#'
#' @param linear tonal values in \code{[0, 1]}
#' @return Adobe RGB tonal values in \code{[0, 1]}
#'
#' @export
#---------------------------------------
adobeRGB.from.linear <- function(linear) {
  linear^(1/2.19921875);
}

#---------------------------------------
#' Get linear tonal values from Adobe RGB ones
#'
#' Convert Adobe RGB tonal values to linear ones.
#'
#' @param adobeRGB Adobe RGB tonal values in \code{[0, 1]}
#' @return linear tonal values in \code{[0, 1]}
#'
#' @export
#---------------------------------------
linear.from.adobeRGB <- function(adobeRGB) {
  adobeRGB^2.19921875;
}

#---------------------------------------
#' Get Adobe RGB tonal values from sRGB ones
#'
#' Convert sRGB tonal values to Adobe RGB ones.
#'
#' @param sRGB tonal values in \code{[0, 1]}
#' @return Adobe RGB tonal values in \code{[0, 1]}
#'
#' @export
#---------------------------------------
adobeRGB.from.sRGB <- function(sRGB) {
  linear.tones <- linear.from.sRGB(sRGB)
  adobeRGB.from.linear(linear.tones) ;
}

#---------------------------------------
#' Get sRGB tonal values from Adobe RGB ones
#'
#' Convert Adobe RGB tonal values to sRGB ones.
#'
#' @param adobeRGB Adobe RGB tonal values in \code{[0, 1]}
#' @return sRGB tonal values in \code{[0, 1]}
#'
#' @export
#---------------------------------------
sRGB.from.adobeRGB <- function(adobeRGB) {
  linear.tones <- linear.from.adobeRGB(adobeRGB)
  sRGB.from.linear(linear.tones) ;
}

#---------------------------------------
#' Get sRGB tonal values from L.Lab ones
#'
#' Convert Lightness (L from Lab) tonal values to sRGB ones.
#'
#' @param L L.Lab tonal values in \code{[0, 100]}
#' @return sRGB tonal values in \code{[0, 1]}
#'
#' @export
#---------------------------------------
sRGB.from.L.Lab <- function(L) {
  # Converting linear to sRGB
  sRGB.from.linear(linear.from.L.Lab(L))
}

#---------------------------------------
#' Get L.Lab tonal values from sRGB ones
#'
#' Convert sRGB tonal values to Lightness (L from Lab) ones.
#'
#' @param sRGB tonal values in \code{[0, 1]}
#' @return L.Lab tonal values in \code{[0, 100]}
#'
#' @export
#---------------------------------------
L.Lab.from.sRGB <- function (sRGB) {
  L.Lab.from.linear(linear.from.sRGB(sRGB));
}

#---------------------------------------
#' Get Adobe RGB tonal values from L.Lab ones
#'
#' Convert Lightness (L from Lab) tonal values to Adobe RGB ones.
#'
#' @param L L.Lab tonal values in \code{[0, 100]}
#' @return Adobe RGB tonal values in \code{[0, 1]}
#'
#' @export
#---------------------------------------
adobeRGB.from.L.Lab <- function(L) {
  # Converting linear to sRGB
  adobeRGB.from.linear(linear.from.L.Lab(L))
}

#---------------------------------------
#' Get L.Lab tonal values from Adobe RGB ones
#'
#' Convert Adobe RGB tonal values to Lightness (L from Lab) ones.
#'
#' @param adobeRGB Adobe RGB tonal values in \code{[0, 1]}
#' @return L.Lab tonal values in \code{[0, 100]}
#'
#' @export
#---------------------------------------
L.Lab.from.adobeRGB <- function (adobeRGB) {
  L.Lab.from.linear(linear.from.sRGB(adobeRGB));
}

#---------------------------------------
#' Get proPhoto tonal values from linear ones
#'
#' Convert linear tonal values to proPhoto ones.
#'
#' @param linear linear tonal values in \code{[0, 1]}
#' @return proPhoto tonal values in \code{[0, 1]}
#'
#' @export
#---------------------------------------
proPhoto.from.linear <- function(linear) {
  proPhoto.linear <- function(x) 16*x;
  proPhoto.gamma <- function(x) x^(1/1.8);

  proPhoto.result <- vector("numeric", length = length(linear))
  tones.in.linear.part <- which(linear <= 0.001953125)

  if (length(tones.in.linear.part) == 0) {
    proPhoto.result <- proPhoto.gamma(linear)
  } else  {
    proPhoto.result[tones.in.linear.part] <- proPhoto.linear(linear[tones.in.linear.part])
    proPhoto.result[-tones.in.linear.part] <- proPhoto.gamma(linear[-tones.in.linear.part])
  }

  # Clip to zero values below numerical precision
  proPhoto.result[abs(proPhoto.result) < 5e-6] <- 0
  proPhoto.result;
}

#---------------------------------------
#' Get linear tonal values to proPhoto ones
#'
#' Convert linear tonal values to proPhoto ones.
#'
#' @param proPhoto tonal values in \code{[0, 1]}
#' @return linear tonal values in \code{[0, 1]}
#'
#' @export
#---------------------------------------
linear.from.proPhoto<- function(proPhoto) {
  proPhoto.linear <- function(x) x/16;
  proPhoto.gamma <- function(x) x^(1.8);

  linear.result <- vector("numeric", length = length(proPhoto))
  tones.in.linear.part <- which(proPhoto <= 0.03125)

  if (length(tones.in.linear.part) == 0) {
    linear.result <- proPhoto.gamma(proPhoto)
  } else  {
    linear.result[tones.in.linear.part] <- proPhoto.linear(proPhoto[tones.in.linear.part])
    linear.result[-tones.in.linear.part] <- proPhoto.gamma(proPhoto[-tones.in.linear.part])
  }

  # Clip to zero values below numerical precision
  linear.result[abs(linear.result) < 5e-6] <- 0
  linear.result;
}
