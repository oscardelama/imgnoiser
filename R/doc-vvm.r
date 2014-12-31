#'  Analysis of the half variance of delta images versus mean
#'
#' The \code{hvdvm} class contains tools for the preparations, analysis and
#' plotting of the half variance of delta images for the study of noise in raw
#' image files.
#'
# ' @export
# xyz <- setRefClass("xyz",
#
#    fields = list(
#      channel.labels = "character",
#      green.channels = "numeric"
#    ),
#
#    methods = list(
#
#      new = function(
#         channel.labels = imgnoiser.option('channel.labels')
#        ,green.channels = imgnoiser.option('green.channels')
#        ,has.RGGB.pattern = imgnoiser.option('has.RGGB.pattern')
#        ,avg.green.label = imgnoiser.option('avg.green.channel.label')
#      ) {
#        "Create a new instance of the hvdvm class."
#        NULL
#      }
#
#      ,digest = function(
#         img.file.name = stop("The 'img.file.name' argument is required")
#        ,img.file.count = NULL
#        ,file.path = './'
#        ,img.file.name.ext = '.fit'
#      ) {
#        "Collect statistics from the raw images files."
#        NULL
#      }
#
#      ,fit.model = function(
#         model.name = imgnoiser.option('fit.model.name')
#        ,model.family = imgnoiser.option('fit.model.family')
#        ,degree = 2L
#        ,formula = NULL
#        ,model.data.name = imgnoiser.option('fit.model.data')
#        , ...
#         ) {
#        "Fit a model to the noise data."
#        NULL
#      }
#
#      ,get.model.predictions = function(
#        model.name = imgnoiser.option('fit.model.name')
#      ) {
#        "Get the values predicted by a fitted model."
#        NULL
#      }
#
#      ,print.model.summary = function(
#         model.name = imgnoiser.option('fit.model.name')
#        ,select=NULL
#        ,...
#      ) {
#       "Print a fitted model summary."
#       NULL
#      }
#
#      ,exists.model = function(
#        model.name = imgnoiser.option('fit.model.name')
#      ) {
#        "Find if a model exists."
#        NULL
#      }
#
#      ,get.model = function(
#        model.name = imgnoiser.option('fit.model.name')
#        ,select = NULL
#      ) {
#        "Get a fitted model object."
#        NULL
#      }
#
#     ,remove.model = function(
#        model.name = stop('A model name argument is required.')
#     ) {
#       "Remove a fitted model."
#       NULL
#     }
#
#     ,plot = function(
#         model.name = FALSE
#        ,obs = TRUE
#        ,print = TRUE
#        ,fit = TRUE
#        ,confid = FALSE
#        ,main = NA
#        ,subt = NA
#        ,xlab = NA
#        ,ylab = NA
#        ,xlim = NULL
#        ,ylim = NULL
#        ,warnings = FALSE
#      ) {
#       "Plot a fitted model and its source data."
#       NULL
#      }
#
#     ,digest = function(
#         img.file.name = stop("The 'img.file.name' argument is required")
#        ,img.file.count = NULL
#        ,file.path = './'
#        ,img.file.name.ext = '.fit'
#      ) {
#        "Process the image samples."
#        NULL
#      }
#
#     ,var.df = function(
#     ) {
#       "Get the resulting noise variance data."
#       NULL
#     }
#
#     ,cov.df = function(
#     ) {
#       "Get the resulting noise covariance data."
#       NULL
#     }
#
#   )
# )

## ==> Remove/destroy the RC class <===
# rm(xyz)
