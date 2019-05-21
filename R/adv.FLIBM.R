#' @title Advance FLIBM stock
#'
#' @param obj object of class FLIBM
#' @param years character vector. years to cycle for advancement.
#'   Defaults to full length of dimension. Vector should follow chronological order.
#' @param units character vector. units to cycle for advancement,
#'   Defaults to full length of dimension.
#' @param seasons character vector. seasons to cycle for advancement.
#'   Defaults to full length of dimension. Vector should follow chronological order.
#'   If shorter than a full year, then the year argument should be for a single year.
#' @param areas character vector. areas to cycle for advancement.
#'   Defaults to full length of dimension.
#' @param iters character vector. iters to cycle for advancement.
#'   Defaults to full length of dimension.
#' @param monitor logical. Should progression be printed.
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
#' nrow(stk$inds[[1]][[1]][[1]])
#' stk$stock.a@stock.n[,'2000',,,,]
#'
#' stk <- adv.FLIBM(obj = stk, years = ac(2000:2009))
#' plot(stk$stock.a@stock.n)
#'
#' # plot length-frequency
#' DIM <- dim(stk$stock.l@catch.n)
#' DIMNAMES <- dimnames(stk$stock.l@catch.n)
#'
#' lfq <- FLIBM2lfq(stk)
#'
#' pal <- colorRampPalette(c("grey30",5,7,2), bias=1.5)
#' with(lfq, image(x=dates, y=midLengths, z=t(catch), col=pal(100)))
#'
#'
#' # numbers by age at start of the year
#' stk$stock.a@range[c("minfbar", "maxfbar")] <- c(1,4)
#' stk.yr <- simplifySeason(stk)
#' stk.yr <- stk.yr[2:dim(stk.yr)[1],]
#' plot(stk.yr)
#'
#' # Biomass
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
  units = NULL,
  seasons = NULL,
  areas = NULL,
  iters = NULL,
  monitor = TRUE
){

  DIMNAMES <- dimnames(obj$stock.l@stock.n)
  if(is.null(units)){units <- DIMNAMES$unit}
  if(is.null(seasons)){seasons <- DIMNAMES$season}
  if(is.null(areas)){areas <- DIMNAMES$area}
  if(is.null(iters)){iters <- DIMNAMES$iter}

  if(is.null(years)){stop("'years' argument must be defined")}
  if(length(years) > 1 & length(seasons) < length(DIMNAMES$season)){
    stop("'years' argument should be for a single year when not advancing the full 'season' dimension")
  }

  for(unit in units){
    for(area in areas){
      for(iter in iters){
        for(year in years){
          for(season in seasons){
            # year = DIMNAMES$year[1]; unit = DIMNAMES$unit[1]; season = DIMNAMES$season[1]; area = DIMNAMES$area[1]; iter = DIMNAMES$iter[1]
            # year;unit;season;area;iter
            obj <- update.inds(obj = obj, year = year, unit = unit, season = season, area = area, iter = iter)
            obj <- reproduce.inds(obj = obj, year = year, unit = unit, season = season, area = area, iter = iter)
            obj <- update.inds(obj = obj, year = year, unit = unit, season = season, area = area, iter = iter)
            obj <- die.inds(obj = obj, year = year, unit = unit, season = season, area = area, iter = iter)
            obj <- record.inds(obj = obj, year = year, unit = unit, season = season, area = area, iter = iter)
            obj <- remove.inds(obj = obj, year = year, unit = unit, season = season, area = area, iter = iter)
            obj <- grow.inds(obj = obj, year = year, unit = unit, season = season, area = area, iter = iter)
            if(monitor){
              cat(paste("year =", year, "| unit =", unit, "| season =", season, "| area =", area, "| iter =", iter),"\n")
              flush.console()
            }
          }
        }
      }
    }
  }
  return(obj)
}
