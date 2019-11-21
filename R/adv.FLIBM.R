#' @title Advance FLIBM stock
#'
#' @param obj object of class FLIBM
#' @param years character vector. years to cycle for advancement.
#'   Defaults to full length of dimension. Vector should follow chronological order.
#' @param seasons character vector. seasons to cycle for advancement.
#'   Defaults to full length of dimension. Vector should follow chronological order.
#'   If shorter than a full year, then the year argument should be for a single year.
#' @param monitor logical. Should progression be printed.
#' @param purgeProb probability of purging empty inds during time step
#'   (between 0 and 1)
#'
#' @return  FLIBM object
#'
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#'
#' stk <- create.FLIBM(years=ac(2000:2009))
#' stk$rec$params$season_wt[] <- c(0,0,0.25,1,0.25,0,0,0,0,0,0,0)
#' dimnames(stk$stock.l@stock.n)['year']
#' nrow(stk$inds)
#' stk$stock.a@stock.n[,'2000',,,,]
#'
#' set.seed(1)
#' stk <- adv.FLIBM(obj = stk, years = ac(2000:2009))
#' plot(stk$stock.a@stock.n)
#'
#' # plot length-frequency
#' DIM <- dim(stk$stock.l@catch.n)
#' DIMNAMES <- dimnames(stk$stock.l@catch.n)
#'
#' lfq <- flquant2lfq(stk$stock.l@catch.n)
#'
#' pal <- colorRampPalette(c("grey30",5,7,2), bias=1.5)
#' with(lfq, image(x=dates, y=midLengths, z=t(catch), col=pal(100)))
#'
#'
#' # numbers by age at start of the year
#' stk$stock.a@range[c("minfbar", "maxfbar")] <- c(2,4)
#' stk.yr <- simplifySeason(stk)
#' stk.yr <- stk.yr[2:dim(stk.yr)[1],] # removes age=0 class
#' plot(stk.yr)
#'
#' # Biomass at age
#' plot(stk$stock.a@stock.n[,,,1,,]*stk$stock.a@stock.wt[,,,1,,])
#'
#' # catch curve
#' tmp <- stk.yr@catch.n[,"2009"]
#' tmp
#' plot(1:6, log(c(tmp)))
#' df <- data.frame(logN = log(c(tmp))[1:4], age = 1:4)
#' fit <- lm(logN ~ age, df)
#' fit
#' abline(fit, col=2)
#'
#'
adv.FLIBM <- function(
  obj,
  years = NULL,
  seasons = NULL,
  monitor = TRUE,
  purgeProb = 0.05
){

  DIMNAMES <- dimnames(obj$stock.l@stock.n)
  if(is.null(seasons)){seasons <- DIMNAMES$season}

  if(is.null(years)){stop("'years' argument must be defined")}
  if(length(years) > 1 & length(seasons) < length(DIMNAMES$season)){
    stop("'years' argument should be for a single year when not advancing the full 'season' dimension")
  }

  for(year in years){
    for(season in seasons){
      # year = DIMNAMES$year[1]; season = DIMNAMES$season[3]
      # year;season
      obj <- update.inds(obj = obj, year = year, season = season)
      obj <- reproduce.inds(obj = obj, year = year, season = season)
      obj <- update.inds(obj = obj, year = year, season = season)
      obj <- die.inds(obj = obj, year = year, season = season)
      obj <- record.inds(obj = obj, year = year, season = season)
      obj <- remove.inds(obj = obj, year = year, season = season, purgeProb = purgeProb)
      obj <- grow.inds(obj = obj, year = year, season = season)
      if(monitor){
        cat(paste("year =", year, "| season =", season),"\n")
        flush.console()
      }
    }
  }
  return(obj)
}
