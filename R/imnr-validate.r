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


#-------------------------------------
# Return valid channel labels
#-------------------------------------
valid.channel.labels <- function(channel.labels, avg.green.label) {

  channel.labels <- vector.alike(channel.labels, 4L)
  avg.green.label <- vector.alike(avg.green.label, 1L)

  c(channel.labels, avg.green.label)
}

#-------------------------------------
# Check green channels are valid
#-------------------------------------
valid.green.channels <- function(green.channels) {

  green.channels <- vector.alike(green.channels, 2L, type='i')

  check.all.whole.numbers(green.channels)
  limit.fails <- sum(green.channels %nin% 1L:4L)

  if (limit.fails)
    stop("The 'green.channels' argument must contain two integer values in [1,4].")

  if (green.channels[1L] == green.channels[2L])
    stop("The 'green.channels' values must not be equal to each other.")

  return(green.channels)
}

#-------------------------------------
# Check if metadata.file.name is valid
#-------------------------------------
valid.metadata.fname <- function(metadata.file.name) {

  metadata.file.name <- vector.alike(metadata.file.name, 1)

  if (!file.exists(metadata.file.name))
    stop("Cannot find file ", sQuote(metadata.file.name),": there is no such file.")

  return (metadata.file.name)
}

#-------------------------------------
# test.single.value
#-------------------------------------
is.single.value <- function(x, name=deparse(substitute(x))) {

  if (is.list(x)) x <- unname(unlist(x))
  return (!is.atomic(x) | length(x) != 1L)
}

#-------------------------------------
# test.single.value
#-------------------------------------
check.single.value <- function(x, name=deparse(substitute(x)), errmsg) {

  if (!is.single.value(x, name)) {
    if (!missing(errmsg))
      stop(errmsg)
    else
      stop(sQuote(name)," must be a single value.")
  }
}

#-------------------------------------
# valid.single.value
#-------------------------------------
valid.single.value <- function(x, coerce='c') {

  if (is.list(x)) x <- unname(unlist(x))
  if (is.atomic(x)) {
    if (length(x) >= 1L) {
      x <- x[1L]
      if (!missing(coerce))
        switch(
          coerce
          ,'c' = as.character(x)
          ,'l' = as.logical(x)
          ,'n' = as.numeric(x)
          ,'i' = as.integer(x)
        )
    } else NA
  }
  else NA
}

#-------------------------------------
# %sv%
#-------------------------------------
"%sv%" <- function(x, y) {
  if (!is.single.value(x)) x else y
}

#-------------------------------------
# package.option
#-------------------------------------
package.option <- function(name) {

  value <- imgnoiser.option(name)

  switch(
    name

    ,show.progress = {
      valid.single.value(value, 'l') %||% TRUE
    }

    ,default.fit.model = {
      valid.single.value(value, 'c') %||% 'weighted'
    }

    ,show.signif.stars = {
      valid.single.value(value, 'l') %||% FALSE
    }

    ,color.pallette = {
      valid.color.pallette(value)
    }
    ,stop("Invalid option ",sQuote(name),".")
  )
}

#-------------------------------------
# get.img.file.names
#-------------------------------------
get.img.file.names <- function(img.file.name, img.file.count, img.file.name.ext, file.path) {

  img.file.name <- vector.alike(img.file.name, 1L, Inf)

  if (length(img.file.name) > 1L) {
    img.file.name
  } else
    expand_img_file_names(img.file.name, img.file.count, img.file.name.ext, file.path)
}

#-------------------------------------
# valid.file.path
#-------------------------------------
valid.file.path <- function(file.path) {

  file.path <- vector.alike(file.path, 1L)

  if (!grepl('.*/$', file.path))
    file.path <- paste0(file.path, '/')

  file.path
}

#-------------------------------------
# valid.file.path
#-------------------------------------
valid.color.pallette <- function(color.pallette) {

  color.pallette <- vector.alike(color.pallette, 5, Inf)
  match <- grepl('^#([[:xdigit:]]{6}|[[:xdigit:]]{8})$', color.pallette)
  if (!all(match))
    color.pallette <- c("#E41A1C","#307AB8","#2C972E","#984EA3", "#E17E25")

  color.pallette
}

