#' @name stk1
#' @title stk1 dataset
#'
#'
#' @description The \code{stk1} data generated with the with FLIBM.
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
#' library(FLCore)
#' library(ggplotFL)
#' library(data.table)
#'
#' # to load and plot
#' data(stk1)
#' plot(stk1$stock.a@stock.n)
#'
#' \donttest{
#' # data generation
#' set.seed(42)
#' stk1 <- create.FLIBM(
#'   length = 0:85, age = 0:6,
#'   year = ac(1960:2000), season = ac(1:12),
#'   n.units = "1e3", wt.units = "kg"
#' )
#'
#' # pulsed recruitment (March-May)
#' stk1$rec$params$season_wt[] <- 0
#' stk1$rec$params$season_wt[3:5] <- c(0.25, 1, 0.25)
#'
#' # SRR params
#' stk1$rec$params['rmax'] <- 1e4
#'
#' # add log-normal noise to rec covar (one value per year)
#' stk1$rec$covar[] <- rlnorm(n = dim(stk1$rec$covar)[2],
#'   meanlog = 0, sdlog = 0.5)
#'
#' # Fbar ages
#' range(stk1$stock.a)[c("minfbar", "maxfbar")] <- c(1,3)
#'
#' # historical F
#' yrs <- 1960:2000
#' steepness <- 0.75
#' FMmax <- 1
#' FMs <- FMmax / (1 + exp(-steepness * (yrs - 1990) ))
#' plot(yrs, FMs)
#'
#' # Advance
#' for(yr in seq(yrs)){
#'   stk1$harvest$params$FM <- FMs[yr]
#'   stk1 <- adv.FLIBM(obj = stk, year = ac(yrs[yr]))
#' }
#'
#' # plot stock numbers
#' plot(stk1$stock.a@stock.n)
#'
#' # trim
#' summary(stk1)
#' ymin <- 1980
#' ymax <- 1999
#'
#' stk1$stock.a <- trim(stk1$stock.a, year = 1980:1999)
#' stk1$stock.l <- trim(stk1$stock.l, year = 1980:1999)
#' stk1$length.a <- trim(stk1$length.a, year = 1980:1999)
#' stk1$age.l <- trim(stk1$age.l, year = 1980:1999)
#'
#' # plot stock numbers
#' plot(stk1$stock.a@stock.n)
#' }
#'
#'
#'
NULL
