#' @name stkMed
#'
#' @title stkMed dataset
#'
#' @description The \code{stkMed} data generated with the with FLIBM.
#' The stock has a seasonal recruitment pattern, with interannual
#' variation in recruitment strength.
#' See example for script used to generate data.
#'
#'
#' @docType data
#' @format FLIBM class object
#' @usage data(stk1)
#' @keywords datasets
#' @examples
#'
#' library(FLIBM)
#'
#' # to load and plot
#' data(stkMed)
#' plot(stkMed$stock.a@stock.n)
#'
#' \donttest{
#' set.seed(42) # for reproducibility
#'
#' stkMed <- create.FLIBM(
#'   length = seq(0,70,2), age = 0:10,
#'   year = ac(1980:2099), season = ac(1:12),
#'   n.units = "1e3", wt.units = "kg"
#' )
#'
#' ### SRR
#'
#' # pulsed recruitment (March-May)
#' stkMed$rec$params$season_wt[] <- 0
#' stkMed$rec$params$season_wt[3:5] <- c(0.25, 1, 0.25)
#'
#' # SRR params
#' stkMed$rec$params['rmax'] <- 1e4
#' stkMed$rec$params['beta'] <- 1
#'
#' # addl. variability
#' # add log-normal noise to rec covar (one value per year)
#' stkMed$rec$covar[] <- rlnorm(n = dim(stkMed$rec$covar)[2],
#'   meanlog = 0, sdlog = 0.5)
#'
#'
#'
#' ### Growth
#' stkMed$growth$params$popLinf <- 64.6
#' stkMed$growth$params$Linf.cv <- 0.1
#' stkMed$growth$params$popK <- 0.21
#' stkMed$growth$params$K.cv <- 0.1
#' stkMed$growth$params$LWa <- 0.018
#' stkMed$growth$params$LWb <- 2.895
#' stkMed$growth$params$Lmat <- 34
#' stkMed$growth$params$Lmat_w <- 6.8
#'
#' ### Mortality
#' stkMed$m$model <- function (length) {return(0.32)}
#' stkMed$harvest$params$L50 <- 11
#' stkMed$harvest$params$wqs <- 2.2
#' stkMed$harvest$params$FM <- 0.16
#'
#'
#' # Fbar ages
#' # range(stkMed$stock.a)[c("minfbar", "maxfbar")] <- c(1,3)
#'
#' # Spin-up
#' stkMed <- spinup.FLIBM(obj = stkMed, nyearsmax = 30, nyearsslope = 5)
#'
#' # Advance first 20 years
#' stkMed <- adv.FLIBM(obj = stkMed, years = ac(1980:1999))
#'
#' # plot stock numbers
#' plot(stkMed$stock.a@stock.n[,ac(1980:1999)])
#' plot(stkMed$stock.a@harvest[,ac(1980:1999)])
#'
#' # save(stkMed, file = "data/stkMed.rda")
#' }
#'
#'
#'
NULL
