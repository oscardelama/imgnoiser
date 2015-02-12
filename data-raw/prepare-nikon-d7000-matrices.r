#-----------------------------
# Create Nikon D7000 DNG matrices
#----------------------------

create.nikon.d7000.ISO100.metadata <- function() {

  to.matrix <- function(x) {
    if (!is.matrix(x))
      matrix(x, nrow=3, ncol=3, byrow=TRUE)
    else
      x
  }

  nikon.d7000.cam.matrix.1 <-
    list(
      'illum.cct.k'        =  2856
      ,'color.matrix'      = to.matrix(c(0.8642, -0.3058, -0.0243, -0.3999, 1.1033, 0.3422, -0.0453, 0.1099, 0.7814))
      ,'forward.matrix'    = to.matrix(c(0.7132, 0.1579, 0.0931, 0.2379, 0.8888, -0.1267, 0.0316, -0.3024, 1.0959))
      ,'cam.calib.matrix'  = diag(1,3,3)
      ,'reduct.matrix'     = diag(1,3,3)
    )

  nikon.d7000.cam.matrix.2 <-
    list(
      'illum.cct.k'        = 6504
      ,'color.matrix'      = to.matrix(c(0.8198, -0.2239, -0.0724, -0.4871, 1.2389, 0.2798, -0.1043, 0.205, 0.7181))
      ,'forward.matrix'    = to.matrix(c(0.6876, 0.3081, -0.0314, 0.2676, 1.0343, -0.3019, 0.0151, -0.1801, 0.9901))
      ,'cam.calib.matrix'  = diag(1,3,3)
      ,'reduct.matrix'     = diag(1,3,3)
    )

  analog.balance <- c(1,1,1)
  black.raw.level <- 0
  # For iso 100 this camera clips the green highlights at this level, despite the
  # 15892 ADU value the .DNG files show
  white.raw.level <- 15779

  #print(getwd())
  tone.curve <- read.csv('./data-raw/dng-tone-conversion.csv')
  data.table::setnames(tone.curve, c('x', 'y'))

  nikon.d7000.ISO100.colmap <- list(
        'cam.matrices.1'   = nikon.d7000.cam.matrix.1
        ,'cam.matrices.2'  = nikon.d7000.cam.matrix.2
        ,'analog.balance'  = analog.balance
        ,'black.raw.level' = black.raw.level
        ,'white.raw.level' = white.raw.level
        ,'tone.curve'      = tone.curve
    )

  save(nikon.d7000.ISO100.colmap, file = './data/nikon-d7000-ISO100-colmap.rdata')
}
