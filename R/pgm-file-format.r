#-----------------------------------------------
#
# The code in this file was extracted from
# the pixmap package, which uses the GPL-2 license:
#
# pixmap:     Bitmap Images (“Pixel Maps”)
# Version:    0.4-11
# Depends: 	  methods
# Published: 	2011-07-19
# Author: 	  Roger Bivand, Friedrich Leisch and Martin Maechler
# Maintainer: Friedrich Leisch <Friedrich.Leisch at R-project.org>
# License:    GPL-2
# Packaged:   2011-07-19 15:19:10 UTC; leisch
# Repository: CRAN
# Date/Publication: 2011-07-19 15:17:50
#
# The original code does not allow to correctly read 16-bit '.pgm'
# files, but only 8-bit ones. Seemingly, this code was built when the
# '.pgm' file format definition did not allow bit depths greater than 8 bits.
#
# The code was adapted to allow the reading of those 16 bit
# files. I wrote an email to F. Leisch but I never got an answer.
#
# The offical 'pgm' format is here:
#
# http://netpbm.sourceforge.net/doc/pgm.html
#
#-----------------------------------------------
read.pgm <- function(file.name, ...) {

  fsz <- file.info(file.name)$size
  con <- file(file.name, open = "rb")
  pnmhead <- read.pnm.head(con)

  # Extra
  if (pnmhead$type != 'pgm')
    stop("Only the 'pgm' format is expected.")

  retval <- read.pnm.data(con, pnmhead, ...)
  if (fsz != seek(con))
    warning("Possible reading error: file size ", fsz, " bytes, but ",
            seek(con), " bytes read")
  close(con)
  retval

}

read.pnm.head <- function(con)
{
  seek(con, 0)
  pm.getc <- function(con) {
    ch <- readChar(con, nchars = 1)
    if (ch == "#") {
      ch <- readChar(con, nchars = 1)
      while (ch != "\n" && ch != "\r") {
        ch <- readChar(con, nchars = 1)
      }
    }
    ch
  }

  pm.getuint <- function(con) {
    ch <- pm.getc(con)
    while (ch == " " || ch == "\t" || ch == "\n" || ch ==
             "\r") {
      ch <- pm.getc(con)
    }
    if (ch < "0" || ch > "9")
      stop("junk in file where an unsigned integer should be")
    i <- 0
    while (ch >= "0" && ch <= "9") {
      digitVal <- as.integer(ch)
      i <- i * 10 + digitVal
      ch <- pm.getc(con)
    }
    i
  }

  pm.readmagicnumber <- function(con) {
    ch <- pm.getc(con)
    if (ch != "P")
      stop("Not a PNM format file")
    ch <- as.integer(pm.getc(con))
    if (ch < 1 || ch > 6)
      stop("Unknown PNM format")
    ascii <- FALSE
    if (ch < 4)
      ascii <- TRUE
    if (ch == 1 || ch == 4)
      type <- "pbm"
    else if (ch == 2 || ch == 5)
      type <- "pgm"
    else if (ch == 3 || ch == 6)
      type <- "ppm"
    res <- list(type = type, ascii = ascii)
    res
  }

  magic <- pm.readmagicnumber(con)
  nc <- pm.getuint(con)
  nr <- pm.getuint(con)

  if (magic$type != "pbm")
    maxval <- pm.getuint(con)
  else
    maxval <- 1

  datastart <- seek(con)
  seek(con, 0)

  if (nc < 0 || nr < 0 || maxval < 1 || maxval > 65535)
    warning(paste("Possible error reading heading: nc:",
                  nc, "nr:", nr, "maxval:", maxval))
  res <- list(nc = nc, nr = nr, maxval = maxval, type = magic$type,
              datastart = datastart, ascii = magic$ascii)
  invisible(res)
}

read.pnm.data <- function(con, pnmhead, ...)
{
  ds <- pnmhead$datastart
  seek(con, ds)
  type <- pnmhead$type
  nl <- ifelse(type == "ppm", 3, 1)
  nc <- pnmhead$nc
  nr <- pnmhead$nr
  ncells <- nl * nc * nr
  int.size <- ifelse(pnmhead$maxval < 256, 1, 2)

  if (pnmhead$ascii) {
    xx <- scan(con, integer(0), n = ncells)
  }
  else {
    xx <- readBin(con, "integer", n = ncells,
                  size = int.size, endian = 'big', signed = FALSE)
  }

  res <- array(xx, dim = c(nl, nc, nr))
  xx <- NULL

  data <- t(res[1, , ])
  res <- NULL

  list(header = pnmhead, image = data)
}

