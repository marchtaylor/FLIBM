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
  # stockYr <- FLCore::simplify(stockSeas, dims = "season", spwn.season = ac(1))
  stockYr <- FLCore::noseason(stockSeas, spwn.season = 1)

  # correct with true mortality
  harvest(stockYr) <- as(apply(harvest(stockSeas), c(1:3,5:6), sum, na.rm=TRUE), "FLQuant")
  harvest(stockYr)@units <- harvest(stockSeas)@units
  harvest(stockYr) <- replace(harvest(stockYr), harvest(stockYr)==Inf, NA)

  m(stockYr) <- as(apply(m(stockSeas), c(1:3,5:6), sum, na.rm=TRUE), "FLQuant")
  m(stockYr)@units <- m(stockSeas)@units
  m(stockYr) <- replace(m(stockYr), m(stockYr)==Inf, NA)

  # seasonal spawning weighting
  spwn_wt <- array(0, dim = dim(stock.n(stockSeas)))
  spwn_wt <- aperm(a = spwn_wt, perm = c(4,1,2,3,5,6))
  spwn_wt[] <- obj$rec$params$season_wt
  spwn_wt <- aperm(a = spwn_wt, perm = c(2,3,4,1,5,6))

  # get numbers
  catch.n(stockYr) <- as(apply(catch.n(stockSeas), c(1:3,5:6), sum, na.rm=TRUE), "FLQuant")
  landings.n(stockYr) <- catch.n(stockYr)
  discards.n(stockYr)[] <- 0
  stock.n(stockYr)[] <- as(array(stock.n(stockSeas)[,,,1,,], dim = dim(stock.n(stockYr))), "FLQuant")

  ## get mean weights
  catch(stockYr) <- apply(stockSeas@catch.n * stockSeas@catch.wt, 2, sum, na.rm = T)
  landings(stockYr) <- catch(stockYr)
  discards(stockYr)[] <- 0

  # use catch proportions to reconstruct catch.wt <- catch.prop / catch.n
  tmp <- apply(catch.n(stockSeas) * catch.wt(stockSeas), c(1:3,5:6), sum, na.rm=TRUE)
  tmp <- apply(tmp, 2, FUN = function(x){x/sum(x, na.rm = TRUE)})
  tmpdf1 <- as.data.frame(tmp)
  names(tmpdf1)[7] <- "prop"
  tmpdf2 <- as.data.frame(catch(stockYr))
  names(tmpdf2)[7] <- "catch"
  tmpdf3 <- as.data.table(catch.n(stockYr))
  names(tmpdf3)[7] <- "naa"
  tmpdf <- merge(x = tmpdf1[,c("age", "year", "prop")], y = tmpdf2[,c("year", "catch")], all.x = T)
  tmpdf <- merge(x = tmpdf, y = tmpdf3[,c("age", "year", "naa")], all.x = T)
  tmpdf$data <- tmpdf$catch * tmpdf$prop / tmpdf$naa
  tmpdf <- tmpdf[order(tmpdf$age, tmpdf$year),]
  catch.wt(stockYr) <- as.FLQuant(tmpdf[,c("age", "year", "data")])

  landings.wt(stockYr) <- catch.wt(stockYr)
  discards.wt(stockYr)[] <- 0


  # stock.wt is yearly mean, but weighted by numbers during spawning activity (spwn_wt)
  stock.wt(stockYr) <- as(
    apply(stock.wt(stockSeas) * stock.n(stockSeas) * spwn_wt, c(1:3,5:6), sum, na.rm=TRUE) /
      apply(stock.n(stockSeas) * spwn_wt, c(1:3,5:6), sum, na.rm=TRUE), "FLQuant")

  # add slots on mortality before spawning
  spwn_wt2 <- weighted.mean(0:11/12, obj$rec$params$season_wt)
  m.spwn(stockYr) <- c(spwn_wt2)
  harvest.spwn(stockYr) <- c(spwn_wt2)

  return(stockYr)
}
