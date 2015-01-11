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

R6.base <- R6::R6Class('R6.base',

  public = list(

   print = function(...) {
     cat(
       "<", class(self)[1], ">\n",
       indent(object.summaries(self), 2),
       "\n",
       sep = ""
     )
     invisible(self)
   }

  )
)

object.summaries <- function(x) {

  if (length(x) == 0) return(NULL)
  if (is.list(x)) obj.name <- names(x)
  else if (is.environment(x))
    obj.name <- ls(x, all.names = TRUE)
    obj.name <- obj.name[!grepl('^\\.', obj.name)]

  obj.kind <- vapply(obj.name, function(name) {
    if (is.environment(x) && bindingIsActive(name, x)) {
      "variable"
    } else {
      obj <- x[[name]]
      if (is.function(obj)) "function"
      else if (is.environment(obj)) "environment"
      else if (is.atomic(obj)) trim(paste(as.character(obj), collapse = " "))
      else paste(class(obj), collapse = ", ")
    }
  }, FUN.VALUE = character(1))

  # Remove the print function from the list
  # browser()
  to.delete <- which(obj.kind == 'function' & obj.name == 'print')
  obj.kind <- obj.kind[-to.delete]
  obj.name <- obj.name[-to.delete]

  obj.name[obj.kind == 'function'] <- paste0(obj.name[obj.kind == 'function'], '()')
  order <- order(obj.kind, obj.name)
  obj.kind <- obj.kind[order]
  obj.name <- obj.name[order]
  obj.kinds <- unique(obj.kind)
  ix <- 0L
  tapply(obj.name, obj.kind, function(x) {
          ix <<- ix + 1L
          paste0(
            obj.kinds[ix], ":\n",
            indent(paste0(x, sep='', collapse='\n'), 2),
            '\n'
            )
        }
      )

}

# Given a string, indent every line by some number of spaces.
# The exception is to not add spaces after a trailing \n.
indent <- function(str, indent = 0) {
  gsub("(^|\\n)(?!$)",
       paste0("\\1", paste(rep(" ", indent), collapse = "")),
       str,
       perl = TRUE
  )
}
