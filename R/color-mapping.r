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

matrix_3x3 <- function(x) matrix(x, nrow=3L, ncol=3L, byrow=TRUE);

#' @include imnr-utils.r
#' @include R6-base.r
#' @export
#-----------------------------------------------------------------------------
colmap <- R6::R6Class('colmap', inherit = R6.base,

  private = list(

    # Diag Analog balance matrix (AB:AnalogBalance)
    .analog.bal.matx  = diag(1,3,3),

    # Illuminant 1 (CalibrationIlluminant1)
    .illum.cct.k.1    = NULL,
    # Camera calibration 1 (CC:CameraCalibration1)
    .cam.calib.matx.1 = NULL,
    # [XYZ.to.raw] color matrix (CM:ColorMatrix1)
    .color.matx.1     = NULL,
    # Reduction Matrix (RM:ReductionMatrix1)
    .reduct.matx.1    = NULL,
    # [raw.to.XYZ.D50] Forward matrix (FM:ForwardMatrix1)
    .forward.matx.1   = NULL,

    # Illuminant 2 (CalibrationIlluminant2)
    .illum.cct.k.2    = NULL,
    # Camera calibration 1 (CC:CameraCalibration2)
    .cam.calib.matx.2 = NULL,
    # [XYZ.to.raw] color matrix (CM:ColorMatrix1)
    .color.matx.2     = NULL,
    # Reduction Matrix (RM:ReductionMatrix2)
    .reduct.matx.2    = NULL,
    # [raw.to.XYZ.D50] Forward matrix (FM:ForwardMatrix2)
    .forward.matx.2   = NULL,

    # raw levels
    .black.raw.level  = NULL,
    .white.raw.level  = NULL,

    # Lab conversion constants
#     .lab.eps          = 216/24389,
#     .lab.k            = 24389/27,
    .lab.eps          = NULL,
    .lab.slope        = NULL,
    .lab.intercept    = 4/29,
    .lab.gamma        = NULL,

    # Tone curve in [0,1]
    .tone.curve        = NULL,

    #-- Temporary variables for RGB conversions (performance)
    # Without raw white balance
    .raw.to.dest.mtx        = NULL,
    .target.space.id        = NULL,
    .space.tone.curve.id    = NULL,
    .scaled.raw.to.dest.mtx = NULL,
    # Including raw white balance
    .forward.mtx           = NULL,
    .scaled.forward.mtx    = NULL,
    .AB.CC.inverse         = NULL,
    # Black and white levels sorted according to RGGB.indices
    .rggb.black.level      = NULL,
    # White level with black level subtracted
    .rggb.white.level      = NULL,
    # A spline over the up-scaled tone curve
    .spline.tone.curve     = NULL,
    .rggb.indices          = NULL,
    .dest.scale            = NULL,
    #--

    # Specified by Bruce Lindbloom (2015-jan)
    # http://www.brucelindbloom.com/Eqn_ChromAdapt.html
    .XYZ.D65.from.XYZ.D50 = matrix_3x3(c( 0.9555766, -0.0230393,  0.0631636,
                                         -0.0282895,  1.0099416,  0.0210077,
                                          0.0122982, -0.0204830,  1.3299098)),

    # As specified in IEC 61966-2-1:1999
    # http://www.w3.org/Graphics/Color/srgb:
    # "with slight modifications to produce 1,1,1 for the D50 white defined in the specification"
    .sRGB_D65.from.XYZ.D50 = matrix_3x3(c( 3.1339, -1.6170,	-0.4906,
                                          -0.9785,  1.9160,	 0.0333,
                                           0.0720, -0.2290,	 1.4057)),

    # As specified Bruce Lindbloom (2015-jan)
    # http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
    .adobe_RGB_D65.from.XYZ.D50 = matrix_3x3(c( 1.9624274, -0.6105343, -0.3413404,
                                               -0.9787684,  1.9161415,  0.0334540,
                                                0.0286869, -0.1406752,  1.3487655)),

    # As specified Bruce Lindbloom (2015-jan)
    # http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
    .prophoto_RGB_D50.from.XYZ.D50 = matrix_3x3(c( 1.3459433, -0.2556075, -0.0511118,
                                                  -0.5445989,  1.5081673,  0.0205351,
                                                   0.0000000,  0.0000000,  1.2118128)),

    # As specified Bruce Lindbloom (2015-jan)
    # http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
    .cie_RGB_E.from.XYZ.D50 = matrix_3x3(c( 2.3638081, -0.8676030, -0.4988161,
                                           -0.5005940,  1.3962369,  0.1047562,
                                            0.0141712, -0.0306400,  1.2323842)),

    .get.cam.matrices = function(ilum.nbr) {
      list(
        'illum.cct.k'     = private[[paste0('.illum.cct.k.', ilum.nbr)]],
        'cam.calib.matx'  = private[[paste0('.cam.calib.matx.', ilum.nbr)]],
        'color.matx'      = private[[paste0('.color.matx.', ilum.nbr)]],
        'reduct.matx'     = private[[paste0('.reduct.matx.', ilum.nbr)]],
        'forward.matx'    = private[[paste0('.forward.matx.', ilum.nbr)]]
      )
    },

    .set.cam.matrices = function(
      ilum.nbr,
      illum.cct.k,
      color.matrix,
      forward.matrix,
      cam.calib.matrix,
      reduct.matrix
    ){

      to.matrix <- function(x) {
        if (!is.matrix(x))
          matrix(x, nrow=3, ncol=3, byrow=TRUE)
        else
          x
      }

      if.null.diag1.3x3 <- function(x) {
        if (is.null(x))
          diag(1,3L,3L)
        else
          to.matrix(x)
      }

      # Validate illuminant CCT
      vector.alike(illum.cct.k, 1L, type='n')
      if (illum.cct.k < 1667 | illum.cct.k > 15000)
        stop("The illuminant CCT must be in the [1667, 15000]ºK range.")

      private[[paste0('.illum.cct.k.', ilum.nbr)]]   = illum.cct.k
      private[[paste0('.color.matx.', ilum.nbr)]]     = to.matrix(color.matrix)
      private[[paste0('.forward.matx.', ilum.nbr)]]   = to.matrix(forward.matrix)
      private[[paste0('.cam.calib.matx.', ilum.nbr)]] = if.null.diag1.3x3(cam.calib.matrix)
      private[[paste0('.reduct.matx.', ilum.nbr)]]    = if.null.diag1.3x3(reduct.matrix)
    },

    .raw.to.linear = function(raw.color) {
      linear.color <- (raw.color - private$.black.raw.level)/
        (private$.white.raw.level - private$.black.raw.level)

      linear.color[linear.color < 0] <- 0
      linear.color[linear.color > 1] <- 1
      # Result
      linear.color;
    },

    .set.cam.matrices.1 = function(
      illum.cct.k,
      color.matrix,
      forward.matrix,
      cam.calib.matrix,
      reduct.matrix
    ){
      if (!is.null(private$.illum.cct.k.2) && illum.cct.k > private$.illum.cct.k.2)
        stop("CCT of illuminant 1 must be lower than CCT of illuminant 2.")

      private$.set.cam.matrices(
        '1',
        illum.cct.k,
        color.matrix,
        forward.matrix,
        cam.calib.matrix,
        reduct.matrix
      )
    },

    .set.cam.matrices.2 = function(
      illum.cct.k,
      color.matrix,
      forward.matrix,
      cam.calib.matrix,
      reduct.matrix
    ){
      if (!is.null(private$.illum.cct.k.1) && illum.cct.k < private$.illum.cct.k.1)
        stop("CCT of illuminant 1 must be lower than CCT of illuminant 2.")

      private$.set.cam.matrices(
        '2',
        illum.cct.k,
        color.matrix,
        forward.matrix,
        cam.calib.matrix,
        reduct.matrix
      )
    },

    .set.raw.levels = function(
      black.raw.level = stop("The 'black.raw.level' argument is missing."),
      white.raw.level = stop("The 'white.raw.level' argument is missing.")
    ){

      # Validate levels
      black.raw.level <- vector.alike(black.raw.level, c(1L,3L, 4L), type='n', all.unique = FALSE)
      white.raw.level <- vector.alike(white.raw.level, c(1L,3L, 4L), type='n', all.unique = FALSE)
      # At least 11 bits of scale are expected in the raw range
      # This is not a code requirement but a rule of thumb to detect input  errors
      if (any(white.raw.level - black.raw.level < 2048))
        stop('The white level is expected to be greater than the black one at least by 2^11.')

      force.to.three.values <- function(x)
        if (length(x) == 1L) rep(x, 3L)
        else x

      private$.black.raw.level = force.to.three.values(black.raw.level)
      private$.white.raw.level = force.to.three.values(white.raw.level)
    },

    .set.analog.balance = function(analog.balance) {
      vector.alike(analog.balance, 3, type='n', all.unique=FALSE)
      private$.analog.bal.matx <- diag(analog.balance, 3L, 3L)
    },

    .interp.raw.from.XYZ = function(cct.k) {

      CC <- interp.matrix.by.inv.temp(
        cct.k,
        private$.cam.calib.matx.1,
        private$.illum.cct.k.1,
        private$.cam.calib.matx.2,
        private$.illum.cct.k.2
      )

      CM <- interp.matrix.by.inv.temp(
        cct.k,
        private$.color.matx.1,
        private$.illum.cct.k.1,
        private$.color.matx.2,
        private$.illum.cct.k.2
      )

      # Result
      list(
        'raw.from.XYZ.mtx' = private$.analog.bal.matx %*% CC %*% CM,
        'cam.calib.mtx'    = CC
      );
    }

  ),

  active = list(

    cam.matrices.1 = function(value) {
      if (!missing(value)) stop('This is a read-only variable.')
      private$.get.cam.matrices('1')
     },

    cam.matrices.2 = function(value) {
      if (!missing(value)) stop('This is a read-only variable.')
      private$.get.cam.matrices('2')
    },

    raw.levels = function(value) {
      if (!missing(value)) stop('This is a read-only variable.')
      list(
        'black.level' = private$.black.raw.level,
        'white.level' = private$.white.raw.level
      )
    },

    analog.balance = function(value) {
      if (!missing(value)) stop('This is a read-only variable.')
      private$.analog.bal.matx;
    },

    target.space.id = function(value) {
      if (!missing(value)) stop('This is a read-only variable.')
      private$.target.space.id;
    },

    dest.from.raw.mtx = function(value) {
      if (!missing(value)) stop('This is a read-only variable.')
      private$.raw.to.dest.mtx
    },

    AB.CC.inverse = function(value) {
      if (!missing(value)) stop('This is a read-only variable.')
      private$.AB.CC.inverse;
    },

    total.tc.sp = function(value) {
      if (!missing(value)) stop('This is a read-only variable.')
      private$.spline.tone.curve;
    },

    XYZ.D50.from.raw.mtx = function(value) {
      if (!missing(value)) stop('This is a read-only variable.')
      private$.forward.mtx;
    }

  ),

  public = list(

    initialize = function (
        camera.coldata = stop("The 'camera.coldata' argument is missing.")
    ){

      # The illuminant 1 is the one with lower illuminant CCT
      # browser()
      if (camera.coldata[['cam.matrices.1']][['illum.cct.k']] <
          camera.coldata[['cam.matrices.2']][['illum.cct.k']]) {
        cam.matrices.1 <- camera.coldata[['cam.matrices.1']]
        cam.matrices.2 <- camera.coldata[['cam.matrices.2']]
      } else {
        cam.matrices.2 <- camera.coldata[['cam.matrices.1']]
        cam.matrices.1 <- camera.coldata[['cam.matrices.2']]
      }

      private$.set.cam.matrices.1(
        cam.matrices.1[['illum.cct.k']],
        cam.matrices.1[['color.matrix']],
        cam.matrices.1[['forward.matrix']],
        cam.matrices.1[['cam.calib.matrix']],
        cam.matrices.1[['reduct.matrix']]
      )

      cam.matrices <- camera.coldata[['cam.matrices.2']]
      private$.set.cam.matrices.2(
        cam.matrices.2[['illum.cct.k']],
        cam.matrices.2[['color.matrix']],
        cam.matrices.2[['forward.matrix']],
        cam.matrices.2[['cam.calib.matrix']],
        cam.matrices.2[['reduct.matrix']]
      )

      private$.set.raw.levels(camera.coldata[['black.raw.level']], camera.coldata[['white.raw.level']])
      private$.set.analog.balance(camera.coldata[['analog.balance']])

      tone.curve <- camera.coldata[['tone.curve']]
      if (is.null(tone.curve))
        private$.tone.curve <- NULL
      else {
        # The tone curve must be explicit. In the future
        tone.curve <- as.data.frame(tone.curve)
        if (is.a.valid.tone.curve(tone.curve)) {
          # Keep the first two columns as (x,y)
          tone.curve <- tone.curve[,1L:2L]
          data.table::setnames(tone.curve, c('x','y'))
          private$.tone.curve <- tone.curve
        }
      }
    },

    find.raw.white.cct = function(
      white.linear = stop("The 'white.linear' argument is missing."),
      verbosity = 0
    ) {

      # Validate argument
      if (any(white.linear < 0 | white.linear > 1))
        stop("The 'rgb.raw' coordinates must be in the [0, 1] range.")
      # Validate calibration matrix a
      if (is.null(private$.color.matx.1))
        stop("The calibration matrix '1' has not been set yet.")
      # Validate calibration matrix b
      if (is.null(private$.color.matx.2))
        stop("The calibration matrix '2' has not been set yet.")

      # If there is only information for one illuminant ??
      if (private$.illum.cct.k.1 == private$.illum.cct.k.2) {
        # @TODO. Handle this case with only one color matrix
        stop("Sorry, but this case is not handled yet.")
      }

      # White balance CCT temp, initial seed
      white.bal.cct <- 5000
      white.xy <- c(0.3451, 0.3516)

      for (n in 1L:5L) {

        # Interpolated matrix fro raw to XYZ
        raw.from.XYZ <- private$.interp.raw.from.XYZ(white.bal.cct)
        raw.from.XYZ.mtx <- raw.from.XYZ[['raw.from.XYZ.mtx']]

        # Get white in XYZ space
        white.XYZ <- mtx.inverse(raw.from.XYZ.mtx) %*% white.linear

        # If the white point has not significantly changed: stop
        white.xy.old <- white.xy
        white.xy <- xy.from.XYZ(white.XYZ)
        uv.dist <- uv.distance.from.xy.1931(white.xy, white.xy.old)
        if (uv.dist < 1e-04) break;

        # If the white point CCT has not significantly changed: stop
        white.bal.cct.old <- white.bal.cct
        white.bal.cct <- planckian.temp.from.xy.1931(white.xy, verbosity)
        if (abs(white.bal.cct - white.bal.cct.old) < 15) break;
      }

      # Result
      list(
        'white.bal.cct'    = white.bal.cct,
        'raw.from.XYZ.mtx' = raw.from.XYZ.mtx,
        'cam.calib.mtx'    = raw.from.XYZ[['cam.calib.mtx']],
        'white.bal.xy'     = white.xy
      )
    },

    get.conv.matrix.from.raw = function(
      from.neutral.raw = stop("The 'white.linear' argument is missing."),
      to.space = imgnoiser.option('target.rgb.space')
    ) {
      # Computing white linear (in [0,1])
      white.linear <- private$.raw.to.linear(from.neutral.raw)
      white.linear <- white.linear/max(white.linear)

      raw.white.info <- self$find.raw.white.cct(white.linear)

      # If we don't have forward matrices:
      if (is.null(private$.forward.matx.1) | all(private$.forward.matx.1 == diag(1,3L,3L))) {
        XYZ.from.raw <- mtx.inverse(raw.white.bal[['raw.from.XYZ.mtx']])
        # @TODO. Handle this case with no forward matrices
        stop("Sorry, but this case is not handled yet.")
      }

      # If there is only information for one illuminant
      if (private$.illum.cct.k.1 == private$.illum.cct.k.2) {
        # @TODO. Handle this case with only one color matrix
        stop("Sorry, but this case is not handled yet.")
      }

      cam.calib.mtx <- raw.white.info[['cam.calib.mtx']]
      AB.CC.inverse <- mtx.inverse(private$.analog.bal.matx %*% cam.calib.mtx)
      ref.neutral <-  c(AB.CC.inverse %*% white.linear)
      raw.wbal <- mtx.inverse(diag(ref.neutral, 3L, 3L))

      # Report some findings
      msg("White Balance CCT:", raw.white.info[['white.bal.cct']])
      msg("White Balance xy (1931):", raw.white.info[['white.bal.xy']])

      # Interpolate the forward matrices
      forward.mtx <- interp.matrix.by.inv.temp(
        raw.white.info[['white.bal.cct']],
        private$.forward.matx.1,
        private$.illum.cct.k.1,
        private$.forward.matx.2,
        private$.illum.cct.k.2
      )

      # Get the 'base' XYZ.to.raw matrix from which we can get other
      # color transformation matrices
      space.convert.mtx <-
        switch(
          to.space,
          'sRGB' = {
            private$.space.tone.curve.id <- 'sRGB'
            private$.sRGB_D65.from.XYZ.D50;
          },

          'Adobe RGB' = {
            private$.space.tone.curve.id <- 'Gamma.2.2'
            private$.adobe_RGB_D65.from.XYZ.D50;
          },

          'ProPhoto RGB' = {
            private$.space.tone.curve.id <- 'ProPhoto'
            private$.prophoto_RGB_D50.from.XYZ.D50;
          },

          'CIE RGB' = {
            private$.space.tone.curve.id <- 'Gamma.2.2'
            private$.cie_RGB_E.from.XYZ.D50;
          },

          'Lab' = {
            private$.space.tone.curve.id <- 'Lab'
            diag(1,3,3);
          },

          stop(paste0("Cannot convert to the space '", to.space, "'."))
        )

      # Results
      private$.target.space.id <- to.space
      private$.AB.CC.inverse <- if (any(AB.CC.inverse != diag(1,3,3))) AB.CC.inverse else NULL

      # Normalize forward matrix
      forward.mtx <- space.convert.mtx %*% forward.mtx
      if (to.space != 'Lab') {
        row.sum <- apply(forward.mtx, 1, sum)
        forward.mtx[1,] <- forward.mtx[1,] / row.sum[1]
        forward.mtx[2,] <- forward.mtx[2,] / row.sum[2]
        forward.mtx[3,] <- forward.mtx[3,] / row.sum[3]
      }

      # Dest.from.raw matrix without raw white balance
      private$.forward.mtx <- forward.mtx
      # Dest.from.raw matrix with raw white balance
      private$.raw.to.dest.mtx <- forward.mtx %*% raw.wbal %*% AB.CC.inverse

      invisible(private$.raw.to.dest.mtx);
    },

    #-------------------------
    # prepare.to.dest.conversions
    #-------------------------
    prepare.to.dest.conversions = function(
      dest.scale = 255,
      RGGB.indices = c(1,2,3,4),
      use.camera.tc = TRUE,
      conv.tone.curve = imgnoiser.option('conv.to.rgb.tc'),
      white.ref.XYZ,
      gamma = 1/3
    ) {
      # browser()
      # Validate the matrices are ready
      if (is.null(private$.forward.mtx) | is.null(private$.raw.to.dest.mtx))
        stop("The color conversion matrix is NULL.")

      # Initialize lab conversion constants
      private$.lab.eps <- NULL
      private$.lab.slope <- NULL
      private$.lab.gamma <- NULL

      private$.dest.scale <- dest.scale
      # compute the scale transformation factor
      if (self$target.space.id != 'Lab') {
        scale <- dest.scale / (private$.white.raw.level - private$.black.raw.level)
      } else {
        scale <- (1/white.ref.XYZ) / (private$.white.raw.level - private$.black.raw.level)
        # This scale is till not in use
        if ((gamma >= 1) || (gamma < 1/4)) stop ("Invalid gamma.")
        private$.lab.gamma <- gamma
        # Compute the joint point
        private$.lab.slope <- (private$.lab.intercept/((1/gamma)^(gamma/(gamma-1))-(1/gamma)^(1/(gamma-1))))^((gamma-1)/gamma)
        private$.lab.eps <- (private$.lab.slope/gamma)^(1/(gamma-1))
      }

      # Apply the scale to the conversion matrix (for performance reasons)
      upscale <- function(x) x*scale;
      private$.scaled.forward.mtx <- apply(private$.forward.mtx, 2, upscale)
      private$.scaled.raw.to.dest.mtx <- apply(private$.raw.to.dest.mtx, 2, upscale)

      # Black & white level according to RGGB.indices
      private$.rggb.black.level <- private$.black.raw.level[c(1,2,2,3)]
      private$.rggb.white.level <- private$.white.raw.level[c(1,2,2,3)]

      private$.rggb.black.level <- private$.rggb.black.level[RGGB.indices]
      # White level "after black frame subtraction"
      private$.rggb.white.level <- private$.rggb.white.level[RGGB.indices] - private$.rggb.black.level
      private$.rggb.indices <- RGGB.indices

      #-- Prepare a smooth spline through the scaled camera and conversion tone curves
      # browser()
      cam.tc <-
        if (use.camera.tc == TRUE) private$.tone.curve
        else NULL

      conv.tc <-
        if (is.a.valid.tone.curve(conv.tone.curve))
          as.data.frame(conv.tone.curve[,c(1:2)])
        else {

          conv.tone.curve <-
            if (conv.tone.curve == 'target.space.tc')
              private$.space.tone.curve.id
            else
              vector.alike(conv.tone.curve, 1L)

          switch(
            conv.tone.curve,
            'linear'    = NULL,
            'sRGB'      = prepare.sRGB.gamma.curve(),
            'ProPhoto'  = prepare.ProPhoto.gamma.curve(),
            'BT.709'    = prepare.BT.709.gamma.curve(),
            'Gamma.2.2' = prepare.std.2.2.gamma.curve(),
            'Gamma.1.8' = prepare.std.1.8.gamma.curve(),
            'Lab'       = NULL,
            stop("Invalid 'conv.tone.curve' argument.")
          )
        }
      # browser()
      private$.spline.tone.curve <- prepare.merged.tone.curve(cam.tc, conv.tc, dest.scale)
    },

    convert.raw.to.dest = function(
      cfa = stop("The 'cfa' argument is missing."),
      is.neutral = FALSE
    ) {

      #-- Linearization and scaling
      # browser()
      # Subtract black level
      raw.red  <- cfa[[private$.rggb.indices[1]]] - private$.rggb.black.level[1]
      raw.green.r <- cfa[[private$.rggb.indices[2]]] - private$.rggb.black.level[2]
      raw.green.b <- cfa[[private$.rggb.indices[3]]] - private$.rggb.black.level[3]
      raw.blue <- cfa[[private$.rggb.indices[4]]] - private$.rggb.black.level[4]

      # Clip the lower limit
      raw.red[raw.red < 0] <- 0
      raw.green.r[raw.green.r < 0] <- 0
      raw.green.b[raw.green.b < 0] <- 0
      raw.blue[raw.blue < 0] <- 0

      # clip the upper limit
      raw.red[raw.red > private$.rggb.white.level[1]] <- private$.rggb.white.level[1]
      raw.green.r[raw.green.r > private$.rggb.white.level[2]] <- private$.rggb.white.level[2]
      raw.green.b[raw.green.b > private$.rggb.white.level[3]] <- private$.rggb.white.level[3]
      raw.blue[raw.blue > private$.rggb.white.level[4]] <- private$.rggb.white.level[4]

      if (is.neutral) {
        #--- Raw white balance
        red.mean <- channelMean(raw.red)
        green.mean <- (channelMean(raw.green.r) + channelMean(raw.green.b)) / 2
        # if (green.mean > 10000) browser()
        blue.mean <- channelMean(raw.blue)

        neutral.raw <- c(red.mean, green.mean, blue.mean)
        raw.white.bal <- diag(max(neutral.raw)/neutral.raw)
        if (!is.null(private$.AB.CC.inverse)) raw.white.bal <- raw.white.bal %*% private$.AB.CC.inverse
        #---
        raw.to.rgb <- private$.scaled.forward.mtx %*% raw.white.bal
      } else {
        raw.to.rgb <- private$.scaled.raw.to.dest.mtx
      }

      # Hold the green channels in an array
      raw.greens <- array(0, dim = c(dim(cfa[[1]]),2))
      raw.greens[,,1L] <- raw.green.r
      raw.greens[,,2L] <- raw.green.b
      # Pick randomly three green channels
      green.indices <- sample(c(1L, 2L), 3L, replace=TRUE)

      #--- Space Conversion
      rgb.red   <- raw.red*raw.to.rgb[1,1] + raw.greens[,,green.indices[1]]*raw.to.rgb[1,2] + raw.blue*raw.to.rgb[1,3]
      rgb.green <- raw.red*raw.to.rgb[2,1] + raw.greens[,,green.indices[2]]*raw.to.rgb[2,2] + raw.blue*raw.to.rgb[2,3]
      rgb.blue  <- raw.red*raw.to.rgb[3,1] + raw.greens[,,green.indices[3]]*raw.to.rgb[3,2] + raw.blue*raw.to.rgb[3,3]

      # Truncate values out of range: underflows
      rgb.red[rgb.red < 0]     <- 0
      rgb.green[rgb.green < 0] <- 0
      rgb.blue[rgb.blue < 0]   <- 0

      if (private$.target.space.id != 'Lab') {
        # Truncate values out of range: overflows
        rgb.red[rgb.red > private$.dest.scale] <- private$.dest.scale
        rgb.green[rgb.green > private$.dest.scale] <- private$.dest.scale
        rgb.blue[rgb.blue > private$.dest.scale] <- private$.dest.scale

        #-- Apply tone curve
        # This is a "naive" tone curve application. i.e there is no hue chroma compensations,
        # this means some color shifting may occurr.
        if (!is.null(private$.spline.tone.curve)) {
          sp <- private$.spline.tone.curve
          apply.tc <- function(img) {
            dims <- dim(img)
            img <- sp(img)
            dim(img) <- dims
            img;
          }

          rgb.red   <- apply.tc(rgb.red)
          rgb.green <- apply.tc(rgb.green)
          rgb.blue <- apply.tc(rgb.blue)
        }

        result <-
          list(
            'r' = rgb.red,
            'g' = rgb.green,
            'b' = rgb.blue
            )

        } else {

        # Truncate values out of range: overflows
        rgb.red[rgb.red > 1]     <- 1
        rgb.green[rgb.green > 1] <- 1
        rgb.blue[rgb.blue > 1]   <- 1

        # Lab tonal scale constants
        eps       <- private$.lab.eps
        slope     <- private$.lab.slope
        intercept <- private$.lab.intercept
        gamma     <- private$.lab.gamma

        f <- function(x) {
          x[x > eps] <- x[x > eps]^gamma
          x[x <= eps] <- x[x <= eps]*slope + intercept
          # Result
          x;
        }

        f.green <- f(rgb.green)
        L <- 116*f.green - 16
        a <- 500*(f(rgb.red) - f.green)
        b <- 200*(f.green - f(rgb.blue))

        result <-
          list(
            'r' = L,
            'g' = a,
            'b' = b
          )
      }
      # Return result
      result;
    }

  )
)
