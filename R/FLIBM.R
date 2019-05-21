

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
#' stk1 <- window.FLIBM(stk1, nyears = 3)
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
  length = dimnames(obj$stock.l)$length
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
