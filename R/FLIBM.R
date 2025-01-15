

#' expand FLIBM object years
#'
#' @param obj FLIBM object
#' @param nyears numeric. Number of years to expand
#'
#' @return FLIBM object
#' @export
#'
#' @examples
#' data(stk1)
#' dimnames(stk1$stock.a)$year
#' stk1 <- window.FLIBM(stk1, start = 2007, end = 2009)
#' plot(stk1$stock.a@stock.n)
#'
#'
window.FLIBM <- function(obj,
  start = dims(obj$stock.a)$minyear,
  end = dims(obj$stock.a)$maxyear
){
  DIMS <- dims(obj$stock.a)
  newyears <- c(start:end)[which(!c(start:end) %in% DIMS$minyear:DIMS$maxyear)]
  obj$stock.l <- window(obj$stock.l, start = start, end = end)
  obj$stock.a <- window(obj$stock.a, start = start, end = end)
  obj$age.l <- window(obj$age.l, start = start, end = end)
  obj$length.a <- window(obj$length.a, start = start, end = end)
  obj$rec$rec <- window(obj$rec$rec, start = start, end = end)
  obj$rec$covar <- window(obj$rec$covar, start = start, end = end)
  if(length(newyears)>0){
    obj$rec$rec[,ac(newyears)] <- 0
    obj$rec$covar[,ac(newyears)] <- 1
  }
  return(obj)
}

#' trim FLIBM dimensions
#'
#' @param obj FLIBM object
#' @param year bla
#' @param age bla
#' @param unit bla
#' @param season bla
#' @param area bla
#' @param iter bla
#' @param length bla
#'
#' @return FLIBM object
#' @export
#'
#' @examples
#' data(stk1)
#' dimnames(stk1$stock.a)$year
#' stk1 <- trim.FLIBM(stk1, year = 1980:1985)
#' dimnames(stk1$stock.a)$year
#'
#'
trim.FLIBM <- function(obj,
  year = dimnames(obj$stock.a)$year,
  age = dimnames(obj$stock.a)$age,
  unit = dimnames(obj$stock.a)$unit,
  season = dimnames(obj$stock.a)$season,
  area = dimnames(obj$stock.a)$area,
  iter = dimnames(obj$stock.a)$iter,
  length = dimnames(obj$stock.l)$age
  ){

  obj$stock.l <- trim(obj$stock.l,
    year = year, length = length, unit = unit, season = season, area = area, iter = iter)
  obj$age.l <- trim(obj$age.l,
    year = year, length = length, unit = unit, season = season, area = area, iter = iter)

  obj$stock.a <- trim(obj$stock.a,
    year = year, age = age, unit = unit, season = season, area = area, iter = iter)
  obj$length.a <- trim(obj$length.a,
    year = year, age = age, unit = unit, season = season, area = area, iter = iter)

  return(obj)
}



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

