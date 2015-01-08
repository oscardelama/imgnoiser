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


#-----------------------------
# Check if x is a vector alike, with length between min.length and max.length
#-----------------------------
vector.alike <- function( x
                         ,min.length
                         ,max.length = min.length
                         ,name=deparse(substitute(x))
                         ,type='c'
                         ,allow.na = FALSE
                         ,allow.null = FALSE
                         ,all.unique = TRUE
                         ,whole.numbers = ifelse(type == 'i', TRUE, FALSE)
                         ,valid.range = NULL
                         ) {

  if (!is.atomic(x) & !is.list(x))
    stop(paste("The", sQuote(name), "argument is not an atomic vector."))

  if (is.list(x)) x <- as.vector(unlist(x))

  if (all(min.length == max.length)) {
    if (length(x) %nin% min.length) {

      # Customize the error message
      if (length(min.length) == 1L)
        stop(paste("The", sQuote(name), "argument must contain ", min.length, " elements."))
      else
        stop(paste("The", sQuote(name), "argument must contain either of {", min.length, "} elements."))
    }
  } else {
    if (length(x) < min.length)
      stop(paste("The", sQuote(name), "argument must contain at least", min.length, "elements."))

    if (is.finite(max.length) & length(x) > max.length)
      stop(paste("The", sQuote(name), "argument must contain at most", max.length, "elements."))
  }

  if (!allow.na & any(is.na(x)))
    stop(paste("The", sQuote(name), "argument must not contain NA elements."))

  if (!allow.null & any(is.null(x)))
    stop(paste("The", sQuote(name), "argument must not contain NULL elements."))

  if (type != '?') {
    nx <-
      switch(
        type
        ,'c' = as.character(x)
        ,'l' = as.logical(x)
        ,'n' = as.numeric(x)
        ,'i' = as.integer(x)
        ,{stop("Unexpected type.")}
      )

    # Check if the type coercion brought NA values
    if (!allow.na & any(is.na(nx)))
      if (length(x) == 1L)
        stop(paste("The", sQuote(name), "argument has not the expected type."))
      else
        stop(paste("One or more elements of the", sQuote(name), "argument has not the expected type."))

    # Check all of them are whole numbers
    if (whole.numbers == TRUE) {
      if (!all.whole.numbers(x)) {
        if (length(x) == 1L)
          stop(paste("The", sQuote(name), "argument is not an integer number as expected."))
        else
          stop(paste("The", sQuote(name), "argument must contain only integer values."))
      } else
        x <- round(x)
    }

    # Check the values are in the valid range
    if (!is.null(valid.range))
      if (any(x < valid.range[1L])) {
        if (length(x) == 1L)
          stop(paste("The", sQuote(name), "argument is bellow the minimum possible value:", sQuote(valid.range[1L]),"."))
        else
          stop(paste("The", sQuote(name), "argument contains one or more elements bellow the minimum possible value:", sQuote(valid.range[1L]),"."))
      } else if (any(x > valid.range[2L])) {
        if (length(x) == 1L)
          stop(paste("The", sQuote(name), "argument is above the maximum possible value:", sQuote(valid.range[2L]),"."))
        else
          stop(paste("The", sQuote(name), "argument contains one or more elements above the maximum possible value:", sQuote(valid.range[2L]),"."))
      }

  }

  # Check Uniqueness
  if (all.unique & (length(unique(x)) != length(x)))
    stop(paste("All the elements of the", sQuote(name), "argument must be different to each other."))

  return(x)
}

#-----------------------------
# Check if x is a numeric vector
#-----------------------------
check.is.numeric <- function(x, name=deparse(substitute(x))) {

  v <- vector.alike(x, 0L, Inf, type='n', name=name)
  if (!is.numeric(v))
    stop(paste("The", name, "argument is not numeric."))

}

#-----------------------------
# Test if x is even
#-----------------------------
is.even <- function(x) {
  ((x %% 2L) == 0L)
}

#-----------------------------
# Test if x is even
#-----------------------------
is.odd <- function(x) {
  ((x %% 2L) != 0L)
}

#-----------------------------
# Test if x is an integer
#-----------------------------
is.whole.number <- function(x, tol = .Machine$double.eps ^ 0.5) {
  abs(x - round(x)) < tol
}

#-----------------------------
# Test if all elements of x are integers (whole numbers)
#-----------------------------
all.whole.numbers <- function(x) {
  all(is.whole.number(x))
}

#-----------------------------
# Check if all elements of x are integers (whole numbers)
#-----------------------------
check.all.whole.numbers <- function(x, name=deparse(substitute(x))) {

  if (!is.numeric(x))
    stop(paste("The", sQuote(name), "argument is not an numeric."))

  if (!all.whole.numbers(x))
    stop(paste("The", sQuote(name), "argument is not an integer."))

}

#-----------------------------
# coalesce NULLs
#-----------------------------
"%||%" <- function(x, y) if(is.null(x) | is.na(x)) y else x

#-----------------------------
# coalesce NAs
#-----------------------------
"%NA%" <- function(x, y) if (!is.na(x)) x else y

#-----------------------------
# function is.given(x)
#-----------------------------
is.given <- function(x) {
  if (is.null(x)) FALSE
  else {
      if (is.na(x[1L])) FALSE
      else TRUE
  }
}

#-----------------------------
# function is.NA(x)
#-----------------------------
is.NA <- function(x){
  if (is.null(x))
    FALSE
  else
    if (is.na(x))
      TRUE
    else
      FALSE
}

#-----------------------------
# Not %in%
#-----------------------------
`%nin%` <- Negate(`%in%`)

is.empty.df <- function(df) {
  return (nrow(df) == 0)
}

#-----------------------------
# trim(x)
# returns string w/o leading or trailing whitespace
#-----------------------------
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#-----------------------------
# more.than.one(x)
# returns TRUE if x contains more tha one distinct values
#-----------------------------
more.than.one <- function(x) {
  result <- FALSE

  if (length(x) >= 2) {
    first <- x[1L]
    for (ix in 2L:length(x))
      if (x[ix] != first) {
        result <- TRUE
        break;
      }
  }
  result;
}

mtx.inverse <- function(mtx) {
  tryCatch(
    base::solve(mtx),
    error = function(c) {
      if (requireNamespace("MASS", quietly = TRUE))
        MASS:ginv(mtx)
      else
        stop("The MASS package is required. Please install it.")
    }
  )
}

between <- function(x, lower.limit, upper.limit) {
  if (any(x < lower.limit))
    FALSE
  else if (any(x > upper.limit))
    FALSE
  else
    TRUE
}
