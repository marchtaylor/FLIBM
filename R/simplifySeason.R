#' Collapse season in age-based FLStock in FLIBM object
#'
#' @description The \code{simplifySeason} function takes an FLIBM object, which
#' typically includes a season dimension, and returns an age-based FLStock
#' object with the season dimension collapsed. The FLCore function
#' \code{simplify} is first called in order to set up the yearly object,
#' followed by additional changes to slots. Numbers slots are based on values
#' from the beginning of the year, while average weight slots are weighted
#' averages by year with numbers at season as the weighting. \code{m.spwn}
#' and \code{harvest.spwn} are based on the seasonal spawning pattern weights
#' as defined by \code{obj$rec$params$season_wt}.
#'
#' @param obj
#'
#' @return FLStock object with season dimension collapsed
#' @export
#'
#' @examples
#' data(stk1)
#' plot(stk1$stock.a@stock.n)
#'
#' stkYr <- simplifySeason(stk1)
#' stkYr <- stkYr[ac(1:range(stkYr)["max"]),] # remove age 0
#' plot(stkYr)
#'
#'
simplifySeason <- function(obj){

  stockSeas <- obj$stock.a

  # collapse season dimension and remove years with few to no catches
  stockYr <- FLCore::simplify(stockSeas, dims = "season", stock.season = ac(1))

  # correct with true mortality
  harvest(stockYr) <- as(apply(harvest(stockSeas), c(1:3,5:6), sum, na.rm=TRUE), "FLQuant")
  harvest(stockYr)@units <- harvest(stockSeas)@units
  harvest(stockYr) <- replace(harvest(stockYr), harvest(stockYr)==Inf, NA)

  m(stockYr) <- as(apply(m(stockSeas), c(1:3,5:6), sum, na.rm=TRUE), "FLQuant")
  m(stockYr)@units <- m(stockSeas)@units
  m(stockYr) <- replace(m(stockYr), m(stockYr)==Inf, NA)

  # correct .wt slots (weighted mean)
  stock.wt(stockYr) <- as(
    apply(stock.wt(stockSeas) * stock.n(stockSeas), c(1:3,5:6), sum, na.rm=TRUE) /
      apply(stock.n(stockSeas), c(1:3,5:6), sum, na.rm=TRUE), "FLQuant")

  catch.wt(stockYr) <- as(
    apply(catch.wt(stockSeas) * catch.n(stockSeas), c(1:3,5:6), sum, na.rm=TRUE) /
      apply(catch.n(stockSeas), c(1:3,5:6), sum, na.rm=TRUE), "FLQuant")

  landings.wt(stockYr) <- as(
    apply(landings.wt(stockSeas) * landings.n(stockSeas), c(1:3,5:6), sum, na.rm=TRUE) /
      apply(landings.n(stockSeas), c(1:3,5:6), sum, na.rm=TRUE), "FLQuant")

  discards.wt(stockYr) <- as(
    apply(discards.wt(stockSeas) * discards.n(stockSeas), c(1:3,5:6), sum, na.rm=TRUE) /
      apply(discards.n(stockSeas), c(1:3,5:6), sum, na.rm=TRUE), "FLQuant")

  stock(stockYr) <- FLCore::computeStock(stockYr)
  catch(stockYr) <- FLCore::computeCatch(stockYr)
  landings(stockYr) <- FLCore::computeLandings(stockYr)
  discards(stockYr) <- FLCore::computeDiscards(stockYr)

  spwn_wt <- weighted.mean(0:11/12, obj$rec$params$season_wt)
  m.spwn(stockYr) <- c(spwn_wt)
  harvest.spwn(stockYr) <- c(spwn_wt)

  return(stockYr)
}
