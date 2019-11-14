#' Convert FLIBM to lfq object
#'
#' @param obj FLIBM object
#'
#' @return lfq class (\code{\link[TropFishR]{plot.lfq}}) object
#' @export
#'
#' @examples
#' stk <- create.FLIBM(
#'   length = 0:85, age = 0:6,
#'   year = ac(2000:2002), unit = "all",
#'   season = ac(1:12), area = "all", iter = "all",
#'   n.units = "1e3", wt.units = "kg"
#' )
#'
#' # recruitment changes
#' stk$rec$params$season_wt[] <- 0
#' stk$rec$params$season_wt[4] <- 1
#' stk$rec$params['rmax'] <- 1e4
#'
#'
#' stk <- spinup.FLIBM(obj = stk, nyearsslope = 5)
#'
#' stk <- adv.FLIBM(stk, year = ac(2000:2002), monitor = TRUE)
#' dim(stk$inds)
#' plot(simplify(stk$stock.a, dims = "season"))
#' plot(stk$stock.a@stock.n)
#'
#' lfq <- flq2lfq(flq = stk$stock.l@catch.n)
#' pal <- colorRampPalette(c("grey30",5,7,2), bias=2)
#' with(lfq, image(x=dates, y=midLengths, z=t(catch), col=pal(100)))
#'
#' # Further plotting functionality with TropFishR
#' \donttest{
#' library(TropFishR)
#' plot(lfq, Fname = "catch", hist.sc = 0.8)
#'
#' lfq2 <- lfqModify(lfq, bin_size = 2)
#' plot(lfq2, Fname = "catch", hist.sc = 0.8)
#'
#' lfq2 <- lfqRestructure(lfq2, MA=5)
#' plot(lfq2, Fname = "rcounts", hist.sc = 0.5)
#' }
#'
flquant2lfq <- function(flq){
  # plot length-frequency
  DIM <- dim(flq)
  DIMNAMES <- dimnames(flq)

  lfq <- list()
  lfq$catch <- t(array(
    aperm(flq, c(4,2,1,3,5,6)),
    dim = c(DIM[4]*DIM[2],DIM[1])
  ))
  lfq$catch <- replace(lfq$catch, is.na(lfq$catch), 0)

  length.min <- as.numeric(DIMNAMES$length)
  length.span <- diff(length.min)
  midLengths <- c(length.min[-length(length.min)] + length.span/2,
   length.min[length(length.min)] + mean(length.span)/2)

  time <- c(outer(
    Y = as.numeric(DIMNAMES$year),
    X = (as.numeric(DIMNAMES$season)-1) / length(DIMNAMES$season),
    FUN = "+"))

  lfq$midLengths <- midLengths
  lfq$dates <- yeardec2date(time)

  class(lfq) <- "lfq"
  return(lfq)
}
