## ----echo = FALSE, message = FALSE---------------------------------------
library(imgnoiser)
library(printr)
options(digits=4)

knitr::opts_chunk$set(
     comment = "##"
    ,prompt = FALSE
    ,collapse = TRUE
    ,dev = 'png'
    ,tidy = FALSE
  )
#render("./vignettes/hvdvm-introduction.Rmd", output_format='md_document')

## ------------------------------------------------------------------------
choose(6, 2)

## ----, echo = FALSE, message = FALSE, results = "hide"-------------------
csv.df <- read.csv('//localhost/data-raw/iso100White.csv')
csv.df <- csv.df[1:6,c('crop.file.name', 'photo.file.name', 'lighting', 'ISO', 'shutter.speed', 'aperture', 'focal.length')]

## ----, echo = FALSE, comment=" "-----------------------------------------
knitr::kable(head(csv.df), caption='**iso100White.csv input file example**', align=c('c','c','c','c','r','c','r'))

## ----eval=FALSE----------------------------------------------------------
#  # Create an 'hvdvm' instance object and save it in the 'my.hvdvm.object' variable
#  my.hvdvm.object <- hvdvm.new()
#  # Starting from here we access to all the class functions and variables using the
#  # instance object
#  my.hvdvm.object$digest('iso100White.csv')

## ----eval=FALSE----------------------------------------------------------
#  help('hvdvm$new')

## ------------------------------------------------------------------------
my.hvdvm <- hvdvm$new(has.RGGB.pattern = TRUE)

## ------------------------------------------------------------------------
my.hvdvm

## ----, echo=FALSE, message=FALSE, results="hide"-------------------------
csv.df <- read.csv('//localhost/data-raw/iso100White.csv')
csv.df <- csv.df[1:6,c('crop.file.name', 'photo.file.name', 'lighting', 'ISO', 'shutter.speed', 'aperture', 'focal.length')]

## ----, echo=FALSE, comment=" "-------------------------------------------
knitr::kable(head(csv.df), caption='**iso100White.csv input file example**', align=c('c','c','c','c','r','c','r'))

## ----echo=FALSE, message=FALSE, results="hide"---------------------------
imgnoiser.option('show.progress', FALSE)
my.hvdvm$digest('//localhost/data-raw/iso100White.csv', '//localhost/data-raw/')
#my.hvdvm <- imgnoiser.load('../data-raw/my.hvdvm')

## ----eval=FALSE----------------------------------------------------------
#  imgnoiser.option('show.progress', FALSE)
#  # The image files are in the './img/' folder
#  my.hvdvm$digest('iso100White.csv', './img/')
#  # 610 different sample combinations were processed.

## ----eval=FALSE----------------------------------------------------------
#  # Get the computed variance
#  head(my.hvdvm$var.df)

## ----, echo=FALSE, comment=" "-------------------------------------------
knitr::kable(head(my.hvdvm$var.df), caption='**Variance and Mean results**', align=c('c','l','l','r','r','r'), digits=2)

## ----eval=FALSE----------------------------------------------------------
#  # Get the different photographic conditions
#  head(my.hvdvm$photo.conditions.df)

## ----, echo=FALSE, comment=" "-------------------------------------------
knitr::kable(head(my.hvdvm$photo.conditions.df), caption='**Photographic Conditions**', align=c('c','c','c','r','c','r','c'))

## ----fig.width=6.5, fig.height=6, fig.align='center'---------------------
# Plot the data in the standard model
my.hvdvm$plot(warnings=TRUE)

## ----eval=FALSE----------------------------------------------------------
#  # Get the computed covariance
#  head(my.hvdvm$cov.df)

## ----, echo=FALSE, comment=" "-------------------------------------------
knitr::kable(head(my.hvdvm$cov.df), caption='**Covariance result**', align=c('c','r','r','r','r','r'), digits=2)

