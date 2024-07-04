"_PACKAGE"

#' biplotEZ: EZ-to-Use Biplots
#'
#' \if{html}{\figure{logo.png}{options: style='float: right' alt='logo' width='120'}}
#' 
#' The goal of biplotEZ is to provide users an EZ-to-use platform for visually representing 
#' their data with biplots. Currently, this package includes principal component analysis 
#' (PCA) and canonical variate analysis (CVA) biplots. This is accompanied by various 
#' formatting options for the samples and axes. Alpha-bags and concentration ellipses are 
#' included for visual enhancements and interpretation.
#'
#' @section Details:
#'
#' \tabular{ll}{
#'    Package: \tab biplotEZ \cr
#'    Type: \tab Package \cr
#'    Version: \tab 2.0\cr
#'    Date: \tab 05-04-2024\cr
#'    License: \tab MIT \cr
#'    LazyLoad: \tab TRUE\cr
#'  }
#' @section Author(s):
#'
#' \itemize{
#'  \item{Sugnet Lubbe (Maintainer, muvisu@sun.ac.za)}
#'  \item{Niël le Roux}
#'  \item{Johané Nienkemper-Swanepoel}
#'  \item{Raeesa Ganey}
#'  \item{Ruan Buys}
#'  \item{Zoë-Mae Adams}
#'  \item{Peter Manefeldt}
#'
#' }
#'
#' @section Core Functions:
#' \itemize{
#'  \item{\link{biplot}}
#'  \item{\link{PCA}}
#'  \item{\link{CVA}}
#'  \item{\link{CA}}
#' }
#'
#' @section Code Availability:
#'
#' The newest version of the package can be obtained on
#' GitHub: \url{https://github.com/MuViSU/biplotEZ}
#'
#' @name biplotEZ

## usethis namespace: start
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics points
#' @importFrom graphics text
#' @importFrom grDevices colorRampPalette
## usethis namespace: end
NULL
