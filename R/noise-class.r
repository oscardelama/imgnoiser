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

#-----------------------------------------------------------------------------
#' @include R6-base.r
#' @import ggplot2
#' @importFrom R6 R6Class
#' @importFrom data.table rbindlist
#' @importFrom data.table setnames
#-----------------------------------------------------------------------------
noise.var <- R6::R6Class('noise.var', inherit = R6.base,

  private = list(

     '.channel.labels'  = NA # character()
    ,'.green.channels'  = NULL # numeric()
    ,'.var.df'          = data.frame()
    ,'.cov.df'          = data.frame()
    ,'.merged.var.cov.df' = NULL
    ,'.std.src.data'    = list()
    ,'.model'           = list()
    ,'.RGGB.indices'    = NULL # integer()

    ##------------------------------
    ## check.model.name
    ##------------------------------
    ,check.model.name = function(name) {
      if (name %nin% names(private$.model))
        stop("A model named ", sQuote(name), " has not been fitted yet.")
    }

    ##------------------------------
    ## pack
    ##------------------------------
    ,.pack = function(bag) {
      bag[['noise.var.class']] <- list(
         'channel.labels' = private$.channel.labels
        ,'green.channels' = private$.green.channels
        ,'var.df'         = private$.var.df
        ,'cov.df'         = private$.cov.df
        ,'std.src.data'   = private$.std.src.data
        ,'model'          = private$.model
        )
      bag;
    }

    ##------------------------------
    ## unpack
    ##------------------------------
    ,.unpack = function(bag) {

      my.bag <- bag[['noise.var.class']]
      private$.channel.labels  <- my.bag[['channel.labels']]
      private$.green.channels  <- my.bag[['green.channels']]
      private$.var.df          <- my.bag[['var.df']]
      private$.cov.df          <- my.bag[['cov.df']]
      private$.std.src.data    <- my.bag[['std.src.data']]
      private$.model           <- my.bag[['model']]

      bag[['noise.var.class']] <- NULL
    }

    ##------------------------------
    ## select.model
    ##------------------------------
    ,select.model = function(
        models,
        select
      ) {

      # Validate the models list
      if (!is.null(select)) {
        select <- vector.alike(select, 1L, Inf, type='?')
        if (!is.numeric(select)) select <- as.character(select)
      }

      # Subset the models satisfying the select argument
      if (!is.null(select)) {
        # Keep only the valid select
        if (is.numeric(select))
          select <- select[select %in% seq_along(models)]
        else
          select <- select[select %in% names(models)]

        if (length(select) == 0)
          stop('No model satisfies the select condition.')
        else
          # This returns a list, even if it has only one element
          models <- models[select]
      }

      models;
    }

  ), #private

  active = list(

    ##------------------------------
    ## var.df (documented in sub-class)
    ##------------------------------
    var.df = function() {
#       if (!missing(value)) stop('The "var.df" variable is read-only.')

      if (nrow(private$.var.df) == 0)
        warning('There is no "var" information. You should probably run the digest() function before.')

      return(private$.var.df)
    }

    ##------------------------------
    ## cov.df  (documented in sub-class)
    ##------------------------------
    ,cov.df = function() {

      if (nrow(private$.cov.df) == 0)
        warning('There is no "cov" information. You should probably run the digest() function before.')

      return(private$.cov.df)
    }

    ,merged.var.cov.df = function() {

      if (is.null(private$.merged.var.cov.df))
        msg('This data is built at the first request and may take a few seconds, please be patient.')
      else
        return(private$.merged.var.cov.df)

      var.df <- as.data.frame(private$.var.df)
      cov.df <- as.data.frame(private$.cov.df)
      cov.df$mean.a <- 0
      cov.df$mean.b <- 0
      cov.df$var.a <- 0
      cov.df$var.b <- 0

      chan.comb <- dplyr::distinct(cov.df, chan.a, chan.b)[,c('chan.a', 'chan.b')]
      chan.comb$level <- 1:nrow(chan.comb)
      chan.comb$label <- paste0(chan.comb$chan.a, ", ", chan.comb$chan.b)
      cov.df$channel.combo <- factor(1, levels=chan.comb$level, labels=chan.comb$label)

       # Show progress bar
      show.progress <- package.option('show.progress')
      if (show.progress == TRUE) {
        message(paste("Processing", nrow(cov.df), "rows:"))
        prog.bar <- txtProgressBar(min = 1L, max = nrow(cov.df), style = 3L)
      }

      for (ix in 1:nrow(cov.df)) {
        # Show progress bar
        if (show.progress) setTxtProgressBar(prog.bar, ix)

        picture <- cov.df[ix, 'pict']
        cov.chan.a <- cov.df[ix, 'chan.a']
        cov.chan.b <- cov.df[ix, 'chan.b']
        cov.df[ix, 'mean.a'] <- subset(var.df, pict == picture & channel == cov.chan.a, mean)
        cov.df[ix, 'mean.b'] <- subset(var.df, pict == picture & channel == cov.chan.b, mean)
        cov.df[ix, 'var.a']  <- subset(var.df, pict == picture & channel == cov.chan.a, var)
        cov.df[ix, 'var.b']  <- subset(var.df, pict == picture & channel == cov.chan.b, var)
        cov.df[ix, 'channel.combo']  <- subset(chan.comb, chan.a == cov.chan.a & chan.b == cov.chan.b, label)
      }
      private$.merged.var.cov.df <- cov.df
      var.df <- NULL
      cov.df <- NULL
      # Result
      private$.merged.var.cov.df;
    }

    ,wide.var.df = function() {
      # browser()
      var.df <- private$.var.df
      melted.df <- reshape2::melt(var.df, id=c('pict', 'channel'))
      wide.df <- reshape2::dcast(melted.df, pict ~ channel + variable)

      wide.df.names <- tolower(names(wide.df))
      wide.df.names <- sub("_", ".", wide.df.names)
      wide.df.names <- make.names(wide.df.names, unique=TRUE)
      data.table::setnames(wide.df, wide.df.names)
      # result
      wide.df;
    }

    ##------------------------------
    ## channel.labels  (documented here)
    ##------------------------------
    ,channel.labels = function() {
      return(private$.channel.labels)
    }

    ##------------------------------
    ## channel.labels  (documented here)
    ##------------------------------
    ,green.channels = function() {
      return(private$.green.channels)
    }

    ##------------------------------
    ## model.list  (documented here)
    ##------------------------------
    ,model.list = function() {

      model.names <- names(private$.model)
      model.list <- list()
      for (model.ix in seq_along(model.names)) {
        model.list[[model.names[model.ix]]] <- private$.model[[model.ix]][['call']]
      }
      model.list
    }

    ##------------------------------
    ## model.list  (documented here)
    ##------------------------------
    ,RGGB.indices = function() {
        private$.RGGB.indices
    }

  ), #active

  public = list(

    ##------------------------------
    ## initialize  (documented here)
    ##------------------------------
    initialize = function(
         channel.labels = imgnoiser.option('channel.labels')
        ,green.channels = imgnoiser.option('green.channels')
        ,has.RGGB.pattern = imgnoiser.option('has.RGGB.pattern')
        ,avg.green.label = imgnoiser.option('avg.green.channel.label')
        ,RGGB.indices = if (has.RGGB.pattern == TRUE) c(1L,2L,3L,4L) else NULL
    ){
      # browser()
      if (has.RGGB.pattern == TRUE) {
        if (all(channel.labels == imgnoiser.default.options[['channel.labels']]))
          channel.labels <- imgnoiser.option('RGGB.channel.labels')
        # (green.channels == NA) indicates green averages are not desired
        if (is.null(green.channels)) green.channels <- c(2L,3L)
        private$.RGGB.indices <- c(1L,2L,3L,4L)

      } else if (!is.null(RGGB.indices)) {
        # This assignment validates the RGGB.indices
        RGGB.indices <- vector.alike(RGGB.indices, 4L, type='i', valid.range=c(1L,4L))
        private$.RGGB.indices <- RGGB.indices
        if (all(channel.labels == imgnoiser.default.options[['channel.labels']]))
          channel.labels <- imgnoiser.default.options[['RGGB.channel.labels']][RGGB.indices]
        if (is.null(green.channels)) green.channels <- RGGB.indices[c(2,3)]
      }

      if (is.given(green.channels) && is.null(RGGB.indices) && !has.RGGB.pattern)
        green.channels <- valid.green.channels(green.channels)

      private$.channel.labels <- valid.channel.labels(channel.labels, avg.green.label)
      private$.green.channels <- green.channels
    }

    ##------------------------------
    ## fit.model  (documented here)
    ##------------------------------
    ,fit.model = function(
         noise.obj
        ,model.name
        ,model.family
        ,degree
        ,formula = NULL
        ,conf.level
        ,model.data.name
        ,...
      ) {

      if (is.empty.df(private$.var.df) & is.empty.df(private$.cov.df))
        stop("There is not var data to build a model.")

      # Get the 'training' data for the regression
      get.model.src.data.func <- imgnoiser.option('get.model.src.data')
      model.src.data <- get.model.src.data.func(model.data.name, noise.obj)

      x <- model.src.data[['data']]$x
      y <- model.src.data[['data']]$y
      split.by <- model.src.data[['data']]$split.by

      if (length(x) == 0 | length(y) == 0)
        stop(paste('Missing x or y components in the model', sQuote(model.name),"."))

      if (length(x) != length(y))
        stop(paste("The x and y components in the model", sQuote(model.name),
                   "don't have the same length"))

      if (length(x) != length(split.by))
        stop(paste("The x and split.by model components in the model",
                   sQuote(model.name),"don't have the same length"))
      # Validate the confidence level
      vector.alike(conf.level, 1, type='n', valid.range=c(0,1))
      # Get the model fitter function
      model.fitter.func <- imgnoiser.option('fit.model')
      # Get the model value predictor function
      model.predictor.func <- imgnoiser.option('get.model.predictions')
      # Prepare the model formula
      if (!is.null(formula)) formula <- lazy(formula)
      formula <- get.model.formula(model.src.data, formula, degree, model.family)

      # Placeholder for the model(s)
      model.objects <- list()
      # Placeholder for the split values
      split.values <- vector()
      # Placeholder for the model fitted data
      predict.df <- data.frame()

      splitted.x <- split(x, split.by)
      splitted.y <- split(y, split.by)

      ix_model <- 0L
      for (split.value in names(splitted.y)) {
        # Ignore the split if it has not enough data to fit a model
        # if (length(splitted.x[[split.value]]) <= 1L) next
        # @TODO: Handle cases when the data make crash the fitting function
        fitted.model <- util.fit.model(model.src.data, split.value, formula, model.family, degree, ...)
        model.call.txt <- fitted.model[['call']]
        model.obj <- fitted.model[['model']]
        lazy.dots <- fitted.model[['lazy.dots']]

        grid <- build.model.grid(splitted.x[[split.value]])
        # browser()
        predictions <- model.predictor.func(model.src.data, model.obj, conf.level,
                                            model.family, split.value, grid, lazy.dots)
        predict.df <- data.table::rbindlist(list(
                           predict.df
                          ,predictions
                      ))

        ix_model <- ix_model + 1L
        # Pack the model object as a list item named as the split value
        model.objects[[split.value]] <- model.obj
        split.values[ix_model] <- split.value
      }

      # Set the data source name in the call text
      model.call.txt <- sub('<%MODEL%>', dQuote(model.src.data[['name']]), model.call.txt, fixed = TRUE)

      #--- Save the model
      if (model.name %in% private$.model) private$.model[model.name] <- NULL
      model.src.data[['split.values']] <- split.values
      private$.model[[model.name]] <- list(
                     'source'       = model.src.data
                    ,'predictions'  = predict.df
                    ,'fit'          = model.objects
                    ,'model.family' = model.family
                    ,'call'         = model.call.txt
                    ,'conf.level'   = conf.level
                    ,'lazy.dots'    = lazy.dots
                )

      msg('The model ', dQuote(model.name),' was successfully fitted using:')
      msg(model.call.txt)
      invisible(self);
    }

    ##------------------------------
    ## get.model.predictions (documented here)
    ##------------------------------
    ,get.model.predictions = function(
        model.name = imgnoiser.option('fit.model.name')
        ,x = NULL
        ,select = NULL
        ,conf.level = NULL
        ,...
      ) {

      # browser()
      # Validate the model name
      private$check.model.name(model.name)

      if (is.null(x))
        private$.model[[model.name]][['predictions']]
      else {
        # Validate x is a numeric vector
        x <- vector.alike(x, 1L, Inf, type='n')

        # Get the model structure
        model.str <- private$.model[[model.name]]
        # Get the model data source
        model.src.data <- model.str[['source']]
        # Get the model objects
        model.objs <-  model.str[['fit']]
        # Lazy dots
        lazy.dots <- model.str[['lazy.dots']]
        # Get the model family
        model.family <-  model.str[['model.family']]
        if (is.null(conf.level))
          # Get the the confidence level
          conf.level <- model.str[['conf.level']]
        else
          # Validate the confidence level
          vector.alike(conf.level, 1, type='n', valid.range=c(0,1))


        # pick the selected models
        if (!is.null(select))
          model.objs <- private$select.model(model.objs, select)

        # Get the model value predictor function
        model.predictor.func <- imgnoiser.option('get.model.predictions')
        # Prepare the grid
        grid <- dplyr::tbl_df(data.frame('x' = x, row.names = NULL))
        # placeholder for the result
        predictions <- data.frame();

        for (split.value in names(model.objs)) {
          preds <- model.predictor.func(model.src.data, model.objs[[split.value]],
                                 conf.level, model.family, split.value, grid, lazy.dots)
          # Collect the predictions
          predictions <- data.table::rbindlist(list(
                              predictions,
                              preds
                          ))
        }
        predictions;
      }

    }

    ##------------------------------
    ## print.model.summary (documented here)
    ##------------------------------
    ,print.model.summary = function(
         model.name = imgnoiser.option('fit.model.name')
        ,select=NULL
        ,...
      ) {

      private$check.model.name(model.name)

      ##----------------
      ## Returns the last lines of the regular summary report: from
      ## the part showing the Residuals five numbers to the end
      ##----------------
      get.model.summary <- function(fit, ...) {

        model.summary <- capture.output(summary(fit, ...))
        len <- length(model.summary)

        for(line.ix in len:1L) {
         if (grepl('Residuals:', model.summary[line.ix], ignore.case = TRUE))
           break
        }

        # If the last line is not blank, add one
        last.line <- trim(model.summary[len])
        if (last.line != '') {
         len <- len + 1L
         model.summary[len] <- ''
        }

        # Return the selected lines
        model.summary[line.ix:len]
      }
      ##----------

      ## try({

      # Use the local show.signif.stars option
      old.option.value <- base::options('show.signif.stars')[[1L]]
      options('show.signif.stars' = package.option('show.signif.stars')[[1L]])

      model.str <- private$.model[[model.name]]
      models <- model.str[['fit']]

      # Subset the models satisfying the select argument
      models <- private$select.model(models, select)

      # Get the model data source
      model.src.data <- model.str[['source']]

      # Get the text with the call used for the fitting
      call.txt <- model.str[['call']]

      # Get the split variable name and values
      split.name <- model.src.data[['label']][['term']]['split.by']
      split.values <- names(models)

      #List header
      writeLines('#-------')
      writeLines(paste0("# Model Name: ", dQuote(model.name)))
      writeLines('#')
      writeLines('# Call:')
      writeLines(paste0("# ", call.txt))
      writeLines('#-------\n')

      for (model.ix in seq_along(models)) {
        # New split
        if (is.given(split.values[model.ix]))
          cat("##-- ", split.name, " : ", dQuote(split.values[model.ix]), "--##\n\n")

        # Print the summary
        writeLines(get.model.summary(models[[model.ix]], ...))
      }

      options('show.signif.stars' = old.option.value)
      invisible(self);
    }

    ##------------------------------
    ## exists.model (documented here)
    ##------------------------------
    ,exists.model = function(
        model.name = imgnoiser.option('fit.model.name')
      ) {
      model.list <- self$model.list
      if (model.name %in% names(model.list)) {
        message(model.list[model.name])
        TRUE;
      }
      else {
        message('The model', dQuote(model.name), ' does not exists.')
        FALSE;
      }
    }

    ##------------------------------
    ## get.model (documented here)
    ##------------------------------
    ,get.model = function(
         model.name = imgnoiser.option('fit.model.name')
        ,select = NULL
      ) {

      private$check.model.name(model.name)
      models <- private$.model[[model.name]][['fit']]

      private$select.model(models, select);
    }

    ##------------------------------
    ## remove.model
    ##------------------------------
    ,remove.model = function(
        model.name = stop("A 'model.name' argument is required.")
      ) {
      private$check.model.name(model.name)
      private$.model[[model.name]] <- NULL
      msg('The model with name', dQuote(model.name),'has been removed.')
    }

    ##------------------------------
    ## plot
    ##------------------------------
    ,plot = function(
         model.name = FALSE
        ,obs = TRUE
        ,print = TRUE
        ,fit = FALSE
        ,pred.int = FALSE
        ,x = NULL
        ,y = NULL
        ,tlab = NULL
        ,slab = NULL
        ,xlab = NULL
        ,ylab = NULL
        ,xlim = NULL
        ,ylim = NULL
        ,warnings = FALSE
    ) {

      # The model.name argument is a single element atomic vector
      vector.alike(model.name, 1L, type='?')
      pred.int <- vector.alike(pred.int, 1L, type='l')
      print <- vector.alike(print, 1L, type='l')
      fit <- vector.alike(fit, 1L, type='l')
      # Get user defaults
      if ((fit == TRUE | pred.int==TRUE) & (model.name == FALSE)) model.name <- TRUE
      if (model.name == TRUE & pred.int != TRUE & fit != TRUE) fit <- TRUE
      if (model.name == TRUE) model.name <- imgnoiser.option('fit.model.name')
      # Get the user color pallete
      color.pallette <- imgnoiser.option('color.pallette')

      if (model.name == FALSE) {
        model.src <- private$.std.src.data
        if (obs != TRUE) warning("There is nothing to plot selected in the arguments.")
      } else {
        # Validate the model name
        private$check.model.name(model.name)
        if (pred.int != TRUE & fit != TRUE) fit <- TRUE
        model.str <- private$.model[[model.name]]
        model.src <- model.str[['source']]
      }

      label <- model.src[['label']]
      split.variable <- label[['term']]['split.by']

      # Get the labels
      set.null.if.na <- function(x, v) {
        if (is.null(x)) v
        else if (is.na(x[1])) NULL
            else x
      }
      t.lab <- tlab
      s.lab <- slab
      x.lab <- xlab
      y.lab <- ylab
      t.lab <- set.null.if.na(t.lab, label$main)
      s.lab <- set.null.if.na(t.lab, NULL)
      x.lab <- set.null.if.na(t.lab, label$xlab)
      y.lab <- set.null.if.na(t.lab, label$ylab)

      # Initialize the plot
      p <- ggplot2::ggplot()

      # Plotting data environment
      plot.envir <- new.env()
      # Try lazy evals
      try_eval <- function(exp.lazy, data) {
        tryCatch(lazyeval::lazy_eval(exp.lazy, data),
                 error = function(c) NULL,
                 warning = function(c) NULL
        )
      }
      # Get the transformation as lazy expressions
      lazy.x <- lazyeval::lazy(x)
      lazy.y <- lazyeval::lazy(y)

      if (obs == TRUE) {
        model.df <- model.src[['data']]
        # Merge the model source data
        plot.envir$data.df <- data.frame('split.by' = model.df$split.by)

        # Use the model names
        data.table::setnames(model.df, 1L:3L, label$term[1:3])
        plot.x <- try_eval(lazy.x, model.df)
        plot.y <- try_eval(lazy.y, model.df)

        # Reset the original XY names
        data.table::setnames(model.df, 1L:3L, names(label$term)[1:3])

        if (is.null(plot.x))
          plot.envir$data.df$x <- model.df$x
        else {
          plot.envir$data.df$x <- plot.x
          if (is.null(xlab)) x.lab <- lazy.x$expr
        }

        if (is.null(plot.y))
          plot.envir$data.df$y <- model.df$y
        else {
          plot.envir$data.df$y <- plot.y
          if (is.null(ylab)) y.lab <- lazy.y$expr
          if (is.null(tlab)) t.lab <- NULL
        }

        point.size <- imgnoiser.option('plot.point.size')
        point.opacity <- imgnoiser.option('plot.point.opacity')
        p <- p + ggplot2::geom_point(ggplot2::aes(x=x, y=y, group=split.by, color=split.by),
                                     data=plot.envir$data.df,
                                     size=point.size,
                                     alpha=I(point.opacity))
      }

      # browser()
      if (fit == TRUE | pred.int == TRUE) {
        model.family <- model.str[['model.family']]
        # Get the model predictions
        model.predictions.df <- as.data.frame(private$.model[[model.name]][['predictions']]);
        # This is the data to plot
        plot.envir$pred.df <- model.predictions.df
        # Use XY names
        names(plot.envir$pred.df)[1L:3L] <- names(label$term)[1L:3L]

        # Transform the x axis
        pred.x <- try_eval(lazy.x, model.predictions.df)
        if (!is.null(pred.x)) {
          plot.envir$pred.df$x <- pred.x
          if (is.null(xlab)) x.lab <- lazy.x$expr
        }
      }

      if (fit == TRUE) {
        pred.y <- try_eval(lazy.y, model.predictions.df)

        if (!is.null(pred.y)) {
          plot.envir$pred.df$y <- pred.y
          if (is.null(ylab)) y.lab <- lazy.y$expr
          if (is.null(tlab)) t.lab <- NULL
        }

        # Use user line width
        line.width <- imgnoiser.option('plot.line.width')
        p <- p + ggplot2::geom_line(ggplot2::aes(x=x, y=y, group=split.by, colour=split.by),
                                    data=plot.envir$pred.df, size=line.width)
      }

      if (pred.int == TRUE) {

        if (model.family =='smooth.spline')
          warning("The 'smooth.spline' model doesn't allow prediction intervals.")
        else {

          # A temporal copy of the fitted model variables
          plot.envir$temp.df <- model.predictions.df[,c(1:3)]

          #-- Transform the upper and lower prediction limits
          plot.envir$temp.df[,2] <- model.predictions.df$lpl
          pred.lpl <- try_eval(lazy.y, plot.envir$temp.df)

          if (!is.null(pred.lpl)) {
            plot.envir$pred.df$lpl <- pred.lpl
            plot.envir$temp.df[,2] <- model.predictions.df$upl
            plot.envir$pred.df$upl <- try_eval(lazy.y, plot.envir$temp.df)
          }
          #--

          # Get the user color pallete
          ribbon.opacity <- imgnoiser.option('plot.ribbon.opacity')
          p <- p + ggplot2::geom_ribbon(ggplot2::aes(x=x,
                                                     ymin=lpl, ymax=upl,
                                                     fill=split.by),
                                        alpha=I(ribbon.opacity),
                                        data=plot.envir$pred.df, stat="identity")
          # Change the label
          if (!is.null(split.variable))
            p <- p + ggplot2::labs(fill=split.variable)
          # Use the user color pallette to fill the ribbon
          if (!is.null(color.pallette))
            p <- p + ggplot2::scale_fill_manual(values=color.pallette)
        }
      }

      # Set the axes labels
      if (!is.null(x.lab)) p <- p + ggplot2::xlab(x.lab)
      if (!is.null(y.lab)) p <- p + ggplot2::ylab(y.lab)

      #-- Set the titles
      if (!is.null(t.lab) & !is.null(s.lab))
        p <- p + ggplot2::ggtitle(bquote(atop(.(t.lab), atop(italic(.(s.lab)), ""))))
      else
        if (!is.null(t.lab))
          p <- p + ggplot2::ggtitle(t.lab)
      #--

      # Change the label
      if (!is.null(split.variable))
        p <- p + ggplot2::labs(colour=split.variable)

      if (!is.null(color.pallette))
        p <- p + ggplot2::scale_colour_manual(values=color.pallette)

      # Fit new axis limits
      if (!is.null(xlim) & length(xlim) >= 2L) p <- p + ggplot2::xlim(xlim[1L], xlim[2L])
      if (!is.null(ylim) & length(ylim) >= 2L) p <- p + ggplot2::ylim(ylim[1L], ylim[2L])

      if (print == TRUE)
        if (warnings == FALSE)
          suppressWarnings(print(p))
        else
          print(p)

      invisible(p);

    }

  ) # public
)

#**********************
# Begin of Documentation
#**********************
noise.var.doc <- list()

#----------------------------------------------
#' Get the channel labels
#'
#' A vector with the channel labels. This labels are set when an instance of the
#' class is constructed using the \code{new} function.
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$channel.labels
#'
#'   vvm$channel.labels
#'  }
#'
#' @return A character vector with five values. The first two are for the first
#'   row in the 2x2 Bayer pattern, the second pair are for the second row, and
#'   the fifth to label computations merging both green channels.
#'
#' @examples
#' \dontrun{
#'
#' # Print the channel labels
#' my.hvdvm$channel.labels
#' #> [1] "Blue"    "Green B" "Green R" "Red"
#' }
#'
#' @seealso \code{\link{hvdvm$new}}, \code{\link{vvm$new}}
#'
#' @aliases vvm$channel.labels hvdvm$channel.labels
#' @name hvdvm$channel.labels
#----------------------------------------------
noise.var.doc$channel.labels <- function()
  NULL

#----------------------------------------------
#' Get the list of models
#'
#' Get a list with the currently fitted models.
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$model.list
#'
#'   vvm$model.list
#'  }
#'
#' @return A list where the names are those given to the fitted models and the
#'   values are a character value with the call used to fit the model.
#'
#' @examples
#' \dontrun{
#'
#' # Print the list of models
#' my.hvdvm$model.list
#' #> $standard
#' #> [1] "lm(formula = var ~ mean, data = model.src('std.var'))"
#' #>
#' #> $weighted
#' #> [1] "lmrob(formula = var ~ mean, data = model.src('std.var'), weights = 1/mean^2)"
#' }
#'
#' @seealso \code{\link{hvdvm$fit.model}}, \code{\link{vvm$fit.model}}
#'
#' @aliases vvm$model.list hvdvm$model.list
#' @name hvdvm$model.list
#----------------------------------------------
noise.var.doc$model.list <- function()
  NULL

#----------------------------------------------
#' Create a new class instance
#'
#' Creates and initializes a new hvdvm or vvm class instance. The arguments and
#' semantics of this function are the same in both classes. All arguments
#' have defaults based on option values, so you can set those option and call
#' this initialization functions without arguments.
#'
#' With the \code{channel.labels} argument you can specify the labels for the
#' corresponding photosites color filters starting at the top left corner, of
#' each of your samples. You will find an introduction to the topic, and a
#' procedure to identify the color of each channel, in the section \emph{"Sample
#' Borders and Channel Identification"} of the \emph{"Collecting Image Samples"}
#' vignette.
#'
#' With the \code{green.channels} argument you can mark which of the labels in
#' the \code{channel.labels} vector correspond to the green channels. For
#' example, if you set the \code{channel.labels} as \code{c('Verde A', 'Azul',
#' 'Rojo', 'Verde R')} (in spanish), you should mark the first and the last
#' channels ('verde' is the spanish word for green) as those corresponding to
#' the green channels. Knowing which are the green channels the class can
#' compute ditional statistics averaging both green channels.
#'
#' If the photosites colors in your samples has the RGGB pattern, set to
#' \code{TRUE} the \code{has.RGGB.pattern} argument. In that case, the channel
#' labels are taken from the option \code{imgnoiser.RGGB.channel.labels}, which
#' by default contains the values \code{c('Red', 'Green R', 'Green B', 'Blue')}.
#' If you don't like those labels you can change the value of that option before
#' calling this function.
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$new(
#'      channel.labels = imgnoiser.option('channel.labels'),
#'      green.channels = imgnoiser.option('green.channels'),
#'      has.RGGB.pattern = imgnoiser.option('has.RGGB.pattern'),
#'      avg.green.label = imgnoiser.option('avg.green.channel.label')
#'      )
#'
#'   vvm$new(
#'      channel.labels = imgnoiser.option('channel.labels'),
#'      green.channels = imgnoiser.option('green.channels'),
#'      has.RGGB.pattern = imgnoiser.option('has.RGGB.pattern'),
#'      avg.green.label = imgnoiser.option('avg.green.channel.label')
#'      )
#'  }
#'
#' @param channel.labels A vector of characters for the labeling of each one of
#'   the photosites colors in the (raw) image samples. Its default value is
#'   taken from \code{\link{imgnoiser.option}('channel.labels')}.
#'
#' @param green.channels A vector with two integers, indices to the
#'   \code{channel.labels} vector, marking them as the green channels. By
#'   default is the value of \code{\link{imgnoiser.option}('green.channels')},
#'   which is \code{NA}
#'
#' @param has.RGGB.pattern A logical value indicating if the image samples has
#'   the RGGB pattern in the photosites colors. Its default value is taken from
#'   \code{\link{imgnoiser.option}('has.RGGB.pattern')}, which by default is
#'   \code{FALSE}, meaning the values passed for the \code{channels.labels} and
#'   \code{green.channels} arguments are used as explained above.
#'
#'   When it is \code{TRUE} the value for the \code{channels.labels} argument is
#'   taken from \code{\link{imgnoiser.option}('RGGB.channel.labels')} and the
#'   \code{green.channels} argument is set to \code{c(2L,3L)}. In this case the
#'   actual values passed for the \code{channels.labels} and
#'   \code{green.channels} arguments are silently ignored.
#'
#' @param avg.green.label A single character value with the label for the
#'   averaged green channel. It's defaulr value is taken from
#'   \code{\link{imgnoiser.option}('avg.green.channel.label')}
#'
#' @return The new and initialized \code{hvdvm} class instance.
#'
#' @examples
#' # Create an instance accepting all the default argument values
#' my.hvdvm <- hvdvm$new()
#'
#' # Create an instance for samples with the RGGB pattern
#' my.hvdvm <- hvdvm$new(has.RGGB.pattern = TRUE)
#'
#' # Specify the channel labels in Italian and mark which of those
#' # are the green channels, both as options, and initialize the class
#' # without passing arguments.
#' imgnoiser.option('channel.labels', c('Verde B', 'Blu', 'Rosso', 'Verde R'))
#' imgnoiser.option('green.channels', c(1,4))
#' my.hvdvm <- hvdvm$new()
#'
#' @seealso See the section \emph{"Sample Borders and Channel Identification"}
#'   in the \emph{"Collecting Image Samples"} vignette, \code{\link{hvdvm}},
#'   \code{\link{imgnoiser.option}}
#'
#' @aliases vvm$new
#' @name hvdvm$new
#----------------------------------------------
noise.var.doc$initialize <- function()
  NULL

#----------------------------------------------
#' Fit a model to the noise data
#'
#' Fits a model through specific variables in the data computed and collected by
#' the \code{digest} function. The arguments and semantics of this function are
#' the same in both, the \code{\link{hvdvm}} and the
#' \code{\link{vvm}} class.
#'
#' The model concept bundles variables selected from those computed by the
#' \code{\link{hvdvm$digest}} or the \code{\link{vvm$digest}} function, to an
#' optional fitting model, that will be fitted over that selected data. Then
#' -for example- we can plot the data in the model with the predictions from the
#' fitted model.
#'
#' All the parameters have sensible default values that you can customize as
#' options with \code{\link{imgnoiser.option}()} function.
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$fit.model(
#'      model.name = imgnoiser.option('fit.model.name'),
#'      model.family = imgnoiser.option('fit.model.family'),
#'      degree = 1L,
#'      formula = NULL,
#'      model.data.name = imgnoiser.option('fit.model.data'),
#'      ...
#'      )
#'
#'   vvm$fit.model(
#'      model.name = imgnoiser.option('fit.model.name'),
#'      model.family = imgnoiser.option('fit.model.family'),
#'      degree = 2L,
#'      formula = NULL,
#'      model.data.name = imgnoiser.option('fit.model.data'),
#'      ...
#'      )
#'  }
#'
#' @param model.name The name you assign to the model this function will
#'   create.
#'
#'   You can build many models and use later this name to refer to any of them.
#'   You will use this name, for example, to get predicted values from the
#'   fitted model or to plot the data in the model or the predictions. If there
#'   is a model with this name, it will be over-written with the result of this
#'   new one.
#'
#' @param model.family Name of one of the available models that will be fit to
#'   the data addressed by the \code{model.data} argument. If you use
#'   \code{NULL} for this argument, no model will be fitted over that data.
#'
#'   The available model families are:
#'
#'    \itemize{
#'      \item \code{lm} Linear models supported by the \code{stats::lm}
#'      function.
#'
#'      \item \code{lmrob} MM-type estimators for linear models, supported by
#'      the \code{stats::lmrob} function.
#'
#'      \item \code{smooth.spline} Cubic smoothing spline fitting, supported by
#'      the \code{stats::smooth.spline} function.
#'    }
#'
#' @param degree The degree of a polynomial of \code{x} that will be fit over
#'   the data. This is assuming the model familly supports that kind of fitting,
#'   which is not the case for the \code{smooth.spline} model.
#'
#' @param formula An optional formula to be used in the fitting proccess. If it
#'   is not provided, the formula will be constructed based on the \code{degree}
#'   argument. If a formula is provided the \code{degree} argument will be
#'   ignored.
#'
#' @param conf.level Confidence level to compute the prediction intervals
#'
#' @param model.data.name Identifies the specific data that will be part of the
#'   model.
#'
#'   By default, there is only one model, named \emph{std.var} with variance data
#'   (from the hvdvm$var.df). This model has the \emph{mean} as predictor (x)
#'   and the \emph{var} as predicted (y) variable. Those variables are grouped
#'   by the corresponding \code{channel} variable (factor).
#'
#'   You can add more models changing the option \code{get.model.src.data} using
#'   the \code{\link{imgnoiser.option}} function.
#'
#' @param ... Additional fitting model arguments that will be directly passed to
#'   function supporting the model referred by the \code{model.family}
#'   argument.
#'
#'   By default, the functions supporting the model families are called with the
#'   model formula (or just \code{x} and \code{y} for the case of smooth.spline)
#'   as the only argument. All the additional arguments, supported by the
#'   function behind each model family, can be specified as part of this
#'   \code{...} argument, they are passed directly as received here to the
#'   corresponding model family function.
#'
#' @return The calling objects instance in invisible way (check the
#'   \code{\link{invisible}} function).
#'
#'   This return value is irrelevant when the class is used in interactive
#'   way. It can be useful for developers extending the class.
#'
#' @seealso The \code{fit.model}, \code{get.model.predictions} and
#'   \code{get.model.src.data} options. The \code{hvdvm$digest()},
#'   \code{\link{vvm$digest}()} functions.
#'
#' @examples
#' \dontrun{
#'
#' # Fit a model using all default values.
#' my.hvdvm$fit.model()
#' #> The model was succesfully fitted using:
#' #> lm(formula = var ~ mean, data = model.src("std.var"))
#'
#' # Fit a robust weighted model with the name 'weighted'
#' library(robustbase)
#' my.hvdvm$fit.model(model.name = 'weighted', model.family = 'lmrob', weights=1/mean^2)
#' #> The model was succesfully fitted using:
#' #> lmrob(formula = var ~ mean, data = model.src("std.var"), weights = 1/mean^2)
#' }
#'
#' @aliases vvm$fit.model hvdvm$fit.model
#' @name hvdvm$fit.model
#----------------------------------------------
noise.var.doc$fit.model <-
      function(
         model.name = imgnoiser.option('fit.model.name')
        ,model.family = imgnoiser.option('fit.model.family')
        ,degree = 1L
        ,formula = NULL
        ,conf.level = imgnoiser.option('conf.level')
        ,model.data.name = imgnoiser.option('fit.model.data')
        , ...
      )
  NULL

#----------------------------------------------
#' Get the values predicted by a fitted model
#'
#' Get the values predicted from a given fitted model.
#'
#' @section Usage:
#'  \preformatted{
#'   vvm$get.model.predictions(
#'      model.name = imgnoiser.option('fit.model.name'),
#'      x = NULL,
#'      select = NULL,
#'      conf.level,
#'      ...
#'      )
#'
#'   hvdvm$get.model.predictions(
#'      model.name = imgnoiser.option('fit.model.name'),
#'      x = NULL,
#'      select = NULL,
#'      conf.level,
#'      ...
#'      )
#'  }
#'
#' @param model.name The name of the model whose predicitions are desired.
#'
#' @param x The predictor values for which the predictions are desired. If this
#'   argument is omitted, the predictions for selected points in the range
#'   of the source data used to build the model are used.
#'
#' @param select A vector with the channels whose predictions we want. They can
#'   be their labels or their indices.
#'
#' @param conf.level The confidence level to get the predictions interval. It
#'   must be in the [0,1] range
#'
#' @return A data frame with the predictions from the model. In the following
#'   descriptions, we will call \code{pred.df} to this result.
#'
#'    \itemize{
#'      \item \code{pred.df[,1]} The predictor variable in the data model. For
#'      the default model (\code{std.var} ), this corresponds to the
#'      \code{mean} column in the variance data.
#'
#'      The range of values in this column spans the range of the original data
#'      used to fit the model. It contains 20 equally spaced values along the
#'      range of the source data, plus 10 values logarithmically equally spaced.
#'      If the range of the original data contains the zero or negative values,
#'      the predictor variable will contain 30 equally spaced values.
#'
#'      \item \code{pred.df[,2]} The predicted variable according to the fitted
#'      model. For the \code{std.var} default model, this corresponds to the
#'      \code{var} column in the variance data.
#'
#'      \item \code{pred.df[,3]} The variable used to group the prediitions. For
#'      the \code{std.var} default model, this corresponds to the
#'      \code{channel} column in the variance data.
#'
#'      \item \code{fit.se} The prediction standard error.
#'
#'      \item \code{lpl} The \emph{lower prediction confidence limit}.
#'
#'      \item \code{upl} The \emph{upper prediction confidence limit}.
#'    }
#'
#' @examples
#' \dontrun{
#'
#' # Get the 'standard' model predictions
#' preds.df <- my.hvdvm$get.model.predictions()
#' # Set the lcl and ucl confidence limits using +/- 2.5 fit standard error
#' preds.df$lcl <- preds.df[,2L] - 2.5*preds.df$fit.se
#' preds.df$ucl <- preds.df[,2L] + 2.5*preds.df$fit.se
#' }
#'
#' @seealso \code{\link{hvdvm$fit.model}}, \code{\link{vvm$fit.model}}
#'
#' @aliases vvm$get.model.predictions
#' @name hvdvm$get.model.predictions
#----------------------------------------------
noise.var.doc$get.model.predictions <- function() NULL

#----------------------------------------------
#' Print a fitted model summary
#'
#' Print the summary of a fitted model.
#'
#' In the default (\code{std.var}) model, the variance (\code{var}) is predicted by
#' the \emph{mean} for each \emph{channel} in the image samples. Therefore,
#' there is a model fitted for each channel, and consequently the summary
#' printed by this function contains four subsections, with the summary of the
#' model fitted for each channel.
#'
#' @section Usage:
#'  \preformatted{
#'   vvm$print.model.summary(
#'      model.name = imgnoiser.option('fit.model.name'),
#'      select = NULL,
#'      ...
#'      )
#'
#'   hvdvm$print.model.summary(
#'      model.name = imgnoiser.option('fit.model.name'),
#'      select = NULL,
#'      ...
#'      )
#'  }
#'
#' @param model.name The name of the fitted model whose summary is desired.
#'
#' @param select A vector with the indices or labels of the channels whose
#'   summary is desired.
#'
#' @param ... Additional parameters for the \code{summary} which is
#'   called by this function to get the model fitting summary information.
#'
#' @return Prints a report with four summaries, one for each fitted channel. The
#'   report starts with a header with the model name and the call sentence used
#'   to fit the models. Each summary is subtitled naming the corresponding
#'   channel label. The exact summary content depends on the model family used
#'   to fit the model.
#'
#' @examples
#' \dontrun{
#'
#' # Print the model summary for the 'standard' fitting
#' my.hvdvm$print.model.summary()
#' # Print the model for the 'weigthed' model including the correlation matrix
#' my.hvdvm$print.model.summary('weigthed', correlation = TRUE)
#' }
#'
#' @seealso \code{\link{hvdvm$fit.model}}, \code{\link{vvm$fit.model}}
#' @aliases vvm$print.model.summary hvdvm$print.model.summary
#' @name hvdvm$print.model.summary
#----------------------------------------------
noise.var.doc$print.model.summary <- function(
             model.name = imgnoiser.option('fit.model.name')
            ,select = NULL
            ,...
          )
  NULL

##------------------------------
#' Find if a model exists
#'
#' Find if a model with a given name has already been fitted.
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$exists.model(
#'      model.name = imgnoiser.option('fit.model.name')
#'      )
#'
#'   vvm$exists.model(
#'      model.name = imgnoiser.option('fit.model.name')
#'      )
#'  }
#'
#' @param model.name A character vector with the name of the models we want to
#'   know if already exists.
#'
#' @return A logical value. Returns TRUE only if the model given in
#'   \code{model.name} exists, otherwise returns false.
#'
#' @examples
#' \dontrun{
#'
#' # Check if there is a model named 'log-sqrt'
#' my.hvdvm$exists.model('log-sqrt')
#' #> [1] FALSE
#' # This function is "syntactic sugar" for the equivalent
#' ('log-sqrt' %in% names(my.hvdvm$model.list))
#' }
#'
#' @seealso \code{\link{hvdvm$fit.model}}, \code{\link{vvm$fit.model}}
#'
#' @aliases vvm$exists.model hvdvm$exists.model
#' @name hvdvm$exists.model
##------------------------------
noise.var.doc$exists.model <- function(
    model.name = imgnoiser.option('fit.model.name')
  )
  NULL

#----------------------------------------------
#' Get a fitted model object
#'
#' Returns a list with the objects fitted in a model with a
#' given name.
#'
#' The R functions that actually fit the models, like lm() or lmrob(), return an
#' object which can be used in other R functions supporting them. As one model
#' is fitted to the data from each channel there are four channel models for
#' each single model fitted in call to the \code{fit.model} function. This
#' function returns a list of those channel model objects.
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$get.model(
#'      model.name = imgnoiser.option('fit.model.name')
#'      )
#'
#'   hvdvm$get.model(
#'      model.name = imgnoiser.option('fit.model.name')
#'      )
#'  }
#' @param model.name The model name whose object is desired.
#'
#' @param select A vector with the indices or labels of the channels whose
#'   model is desired.
#'
#' @return A list whose names are the channel labels and the values are the
#'   objects fitted for each channel.
#'
#' @examples
#' \dontrun{
#'
#' # Get a list with the 'weighted' model fitting objects
#' ch.models <- my.hvdvm$get.model('weighted')
#' #> class(ch.models)
#' #> [1] "list"
#' #> names(ch.models)
#' #> [1] "Blue"    "Green B" "Green R" "Red"
#' }
#'
#' @aliases vvm$get.model hvdvm$get.model
#' @name hvdvm$get.model
#----------------------------------------------
noise.var.doc$get.model <- function(
  model.name = imgnoiser.option('fit.model.name')
  ,select = NULL
)
  NULL

#----------------------------------------------
#' Remove a fitted model
#'
#' Remove, permanently delete, a model fitted with a given name.
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$remove.model(
#'      model.name = stop('A model name argument is required.')
#'      )
#'
#'   vvm$remove.model(
#'      model.name = stop('A model name argument is required.')
#'      )
#'  }
#'
#' @param model.name The name of the model that will be removed (deleted).
#'
#' @examples
#' \dontrun{
#'
#' my.hvdvm$remove.model('weighted')
#' #> The model with name "standard" has been removed.
#' }
#'
#' @aliases vvm$remove.model hvdvm$remove.model
#' @name hvdvm$remove.model
#----------------------------------------------
noise.var.doc$remove.model = function(
  model.name = stop('A model name argument is required.')
  )
  NULL

#----------------------------------------------
#' Plot a fitted model and its source data
#'
#' @section Usage:
#'  \preformatted{
#'   hvdvm$plot(
#'      model.name = FALSE,
#'      obs = TRUE,
#'      print = TRUE,
#'      fit = FALSE,
#'      pred.int = FALSE,
#'      x = NULL,
#'      y = NULL,
#'      tlab = NULL,
#'      slab = NULL,
#'      xlab = NULL,
#'      ylab = NULL,
#'      xlim = NULL,
#'      ylim = NULL,
#'      warnings = FALSE
#'      )
#'
#'   vvm$plot(
#'      model.name = FALSE,
#'      obs = TRUE,
#'      print = TRUE,
#'      fit = FALSE,
#'      pred.int = FALSE,
#'      x = NULL,
#'      y = NULL,
#'      tlab = NULL,
#'      slab = NULL,
#'      xlab = NULL,
#'      ylab = NULL,
#'      xlim = NULL,
#'      ylim = NULL,
#'      warnings = FALSE
#'      )
#'  }
#'
#' @param model.name Name of the model whose prediction (see the \code{fit}
#'   argument) or confidence area (see the \code{confid} argument) are desired
#'   in the plot.
#'
#' @param obs If TRUE the observations will be included in the plot.
#'
#' @param print If TRUE the plot will be rendered in the output device. If you
#'   want to customize the plot before printing it, set this parameter to FALSE
#'   and use the returned ggplot2 value.
#'
#' @param fit If TRUE the prediction intervals of the model referred with the
#'   \code{model.name} argument, will be plotted as a line for each channel.
#'
#' @param x
#' @param y The default model uses the \code{mean} and \code{var} variables in
#'   the \code{var.df} data frame as the variables to plot in the x and y axes.
#'   However, you can change the plot to use in those axes the expressions you
#'   want. For example, you may want to plot the SNR (signal to noise ratio), in
#'   such case use \code{y = mean/sqrt(var)}.
#'
#' @param pred.int If TRUE the area between the predictions confidence limits, of
#'   the model referred with the \code{model.name} argument, will be drawn with
#'   a semi-transparent color.
#'
#' @param tlab The plot main title. If it is not given, the title in the model
#'   data source will be used. A NULL value means no title is desired.
#'
#' @param slab The plot subt-title. If it is not given the plot won't have a
#'   sub-title.
#'
#' @param xlab The plot x axis title. If it is not given, the corresponding
#'   label in the model data source will be used. A NULL value means no axis
#'   title is desired.
#'
#' @param ylab The plot y axis title. If it is not given, the corresponding
#'   label in the model data source will be used. A NULL value means no axis
#'   title is desired.
#'
#' @param xlim A vector with two values, where the second is greater than the
#'   first. The limits for the x axis. See \code{warnings} to avoid the warnings
#'   when using this parameter.
#'
#' @param ylim A vector with two values, where the second is greater than the
#'   first. The limits for the y axis. See \code{warnings} to avoid the warnings
#'   when using this parameter.
#'
#' @param warnings If TRUE, suppress the warnings when printing the plot.
#'
#' When using the \code{xlim} or \code{ylim} argument, ggplot2 throws some
#' warnings about the data not being included in the plot because of those
#' limits. That can be annoying, and by default all the warnings are turned off.
#' You may want to change this default value if for some reason you want to get
#' any warning that is being suppressed by this default.
#'
#' @return Invisibly returns a ggplot2 object which can be used to customize the
#'   plot.
#'
#' @examples
#' \dontrun{
#'
#' # Plot the observations in the standrad data model
#' my.hvdvm$plot()
#'
#' # Plot the model named 'standard' using a customized title
#' my.hvdvm$plot(
#'    model.name = TRUE,
#'    main='Nikon D7000 - ISO 100'
#'    subt = 'Half Var Delta versus Mean'
#'    )
#' }
#'
#' @aliases vvm$plot hvdvm$plot
#' @name hvdvm$plot
#----------------------------------------------
noise.var.doc$plot <- function()   NULL

rm(noise.var.doc)
#**********************
# End of Documentation
#**********************

