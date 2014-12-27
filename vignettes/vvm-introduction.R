## ----echo = FALSE, message = FALSE---------------------------------------
  library(imgnoiser)
  library(printr)
  library(ggplot2)
  options(digits=4)
  knitr::opts_chunk$set(
     comment = "##"
    ,prompt = FALSE
    ,collapse = TRUE
    ,dev = 'png'
    ,tidy = FALSE
  )
#render("./vignettes/vvm-introduction.Rmd", output_format='md_document')

## ----eval=FALSE----------------------------------------------------------
#  # Create an 'vvm' instance object and save it in the 'my.vvm.object' variable
#  my.vvm.object <- vvm.new()
#  # Starting from here we access to all the class functions and variables using the
#  # instance object:
#  my.vvm.object$digest('crop_', 40)

## ----eval=FALSE----------------------------------------------------------
#  help('vvm$new')

## ------------------------------------------------------------------------
my.vvm <- vvm$new(has.RGGB.pattern = TRUE)

## ------------------------------------------------------------------------
my.vvm

## ----echo=FALSE, message=FALSE, results="hide"---------------------------
imgnoiser.option('show.progress', FALSE)
my.vvm$digest('crop_', 240, '//localhost/data-raw/')
# imgnoiser.save(my.vvm, '//localhost/data-raw/my.vvm', stop.overwrite=FALSE)
# my.vvm <- imgnoiser.load('../data-raw/my.vvm')

## ----eval=FALSE----------------------------------------------------------
#  # Turn off the progress bar
#  imgnoiser.option('show.progress', FALSE)
#  # The image files are in the './samples/' folder
#  my.vvm$digest('crop_', 240, './samples/')
#  ## 240 image samples were successfully processed.

## ----eval=FALSE----------------------------------------------------------
#  # Get the computed variance
#  head(my.vvm$var.df)

## ----, echo=FALSE, comment=NA--------------------------------------------
knitr::kable(head(my.vvm$var.df), caption='**Variance and Mean results**', align=c('l','c','r','r'), digits=4)

## ----fig.width=6.5, fig.height=6, fig.align='center'---------------------
# Plot the data in the standard model
my.vvm$plot()

## ----eval=FALSE----------------------------------------------------------
#  # Get the computed covariance
#  head(my.vvm$cov.df)

## ----, echo=FALSE, comment=NA--------------------------------------------
knitr::kable(head(my.vvm$cov.df), caption='**Covariance result**', align=c('l','c','c','r'), digits=2)

