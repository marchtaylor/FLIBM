
#' Simulate a single cohort
#' @param obj FLIBM object.
#' @param ssbfec numeric value. Single value describing constant spawning stock
#'   biomass (or fecundity), used in determining recruitment with \code{obj$rec}
#'   during the first year of simulation.
#' @param FM numeric value. Fishing mortality to be applied.
#' @param years character vector. Years to use in simulation
#'   (from \code{ dimnames(obj$stock.a@stock.n)} )
#' @param seed seed value for random number reproducibility (Default: 1).
#'   When seed = NULL, a random seed is used during each FM run.
#'
#'
#' @return a list
#' @export
#'
#' @examples
#'
#'
#' data(stkMed)
#'
#' # obj$rec parameters should result in high recruitment for the given
#' # ssbfec setting (e.g. many thousands of individuals will result in
#' # smoother ypr pattern)
#' stkMed$rec$params$rmax <- 1e5
#' obj <- stkMed
#'
#' res <- cohort.FLIBM(obj, ssbfec = 5e5, FM = 0.3, seed = 1,
#'   return.FLIBM = TRUE)
#'
#' plot(res$obj$stock.a@stock.n[,ac(1980:1995)])
#'
#' stkYr <- simplifySeason(res$obj)
#' stkYr <- stkYr[-1,ac(1980:2000)]
#' plot(stkYr)
#'
#'
cohort.FLIBM <- function(
  obj = NULL,
  ssbfec = 1e6,
  FM = 0.3,
  years = dimnames(obj$stock.a)$year,
  seed = 1,
  monitor = FALSE,
  return.FLIBM = FALSE
){

  obj.x <- obj
  RAN <- range(obj.x$stock.a)
  DIM <- dim(obj.x$stock.a@stock.n)
  DIMNAMES <- dimnames(obj.x$stock.a@stock.n)
  obj.x$harvest$params$FM <- FM
  obj.x$stock.a@stock.n[] <- NaN
  obj.x$stock.a@catch.n[] <- NaN
  obj.x$stock.a@harvest[] <- NaN
  obj.x$stock.l@stock.n[] <- NaN
  obj.x$stock.l@catch.n[] <- NaN
  obj.x$stock.l@harvest[] <- NaN
  obj.x$inds <- data.table::data.table()
  obj.x$rec$covar[] <- 0
  obj.x$rec$covar[,FLCore::ac(years[1])] <- 1
  recr.season <- seq(DIM[4])*NaN

  set.seed(seed)

  for (year in years) {
    for (season in DIMNAMES$season) {
      if (year == years[1]) {
        yeardec <- as.numeric(year) + (as.numeric(season) -
          1)/dim(obj.x$stock.l@stock.n)[4]
        date <- FLIBM::yeardec2date(yeardec)
        if (season == dim(obj.x$stock.l@stock.n)[4]) {
          yeardec2 <- as.numeric(year) + 1
          date2 <- FLIBM::yeardec2date(yeardec2)
        }  else {
          yeardec2 <- as.numeric(year) + (as.numeric(season))/dim(obj.x$stock.l@stock.n)[4]
          date2 <- FLIBM::yeardec2date(yeardec2)
        }
        tincr <- yeardec2 - yeardec
        ARGS.x <- list(yeardec = yeardec, yeardec2 = yeardec2,
          date = date, tincr = tincr,
          year = year, season = season,
          ssbfec = ssbfec)
        ARGS.x <- c(ARGS.x, obj.x$rec$params)
        args.incl <- which(names(ARGS.x) %in% names(formals(obj.x$rec$model)))
        ARGS.x <- ARGS.x[args.incl]
        n.recruits <- ceiling(c(do.call(obj.x$rec$model,
          ARGS.x)))
        recr.season[match(season, DIMNAMES$season)] <- n.recruits
        if (n.recruits > 0) {
          newinds <- obj.x$make.inds(n = n.recruits,
            obj = obj.x)
          obj.x$inds <- data.table::rbindlist(list(obj.x$inds,
            newinds))
        }
      }
      if (nrow(obj.x$inds) > 0) {
        obj.x <- FLIBM::adv.FLIBM(obj = obj.x,
          year = year, season = season,
          monitor = monitor)
      }
    }
  }

  # calculate Lopt
  stock(obj.x$stock.l) <- computeStock(obj.x$stock.l)
  # plot(stock(obj.x$stock.l)[,ac(1980:2000)])
  maxB <- which.max(stock(obj.x$stock.l))
  Ls <- as.numeric(dimnames(obj.x$stock.l)$length)
  Lmids <- Ls + diff(Ls)[1]/2
  Lmat <- obj.x$stock.l@stock.n * NaN
  Lmat[] <- Lmids
  Lmu <- as(apply(Lmat * stock.n(obj.x$stock.l), c(2,4), sum, na.rm=TRUE) /
    apply(stock.n(obj.x$stock.l), c(2,4), sum, na.rm=TRUE), "FLQuant")
  # plot(Lmu[,ac(1980:2000)])
  Lopt <- c(Lmu)[maxB]

  objYr.x <- FLIBM::simplifySeason(obj.x)
  Ca.x <- sum(objYr.x@catch[, years], na.rm = TRUE)
  SSB.x <- sum(FLCore::ssb(objYr.x[, years]), na.rm = TRUE)
  Recr.x <- sum(recr.season, na.rm = TRUE)
  res <- list(FM = FM, Catch = Ca.x, SSB = SSB.x,
      Recr = Recr.x, Lopt = Lopt, seed = seed)
  if(return.FLIBM) res$obj <- obj.x
  return(res)
}

