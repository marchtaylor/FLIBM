#' Perform a yeild-per-recruit (YPR) analysis on an FLIBM object
#'
#' @description bla
#'
#' @param obj FLIBM object.
#' @param ssbfec numeric value. Single value describing constant spawning stock
#'   biomass (or fecundity), used in determining recruitment with \code{obj$rec}
#'   during the first year of simulation.
#' @param FMs numeric vector. Sequence of maximum fishing mortalities to
#'   be applied.
#' @param years character vector. Years to use in simulation
#'   (from \code{ dimnames(obj$stock.a@stock.n)} )
#' @param parallel logical. Simulations run in parallel.
#' @param no_cores numeric value. Number of cores to use when
#'   \code{parallel = TRUE} (Default: \code{no_cores = detectCores() - 1})
#' @param clusterType (Default: `clusterType = "PSOCK"`)
#' @param outfile character. Text file name (Default: `outfile = "output.txt"`)
#'   which will records the progress of the permutation completions.
#' @param seed seed value for random number reproducibility (Default: 1).
#'   When seed = NULL, a random seed is used during each FM run.
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' data(stkMed)
#'
#' # obj$rec parameters should result in high recruitment for the given
#' # ssbfec setting (e.g. many thousands of individuals will result in
#' # smoother ypr pattern)
#' stkMed$rec$params$rmax <- 1e4
#'
#'
#'
#' system.time(
#'   resdf <- ypr.FLIBM(obj = stkMed, ssbfec = 1e6, parallel = TRUE,
#'     FMs = seq(0,0.6,len=12), seed = 1)
#' )
#'
#' plot(SSB ~ FM, resdf, t = "o")
#' plot(Catch/Recr ~ FM, resdf, t = "o")
#' plot(Lopt ~ FM, resdf, t = "o")
#'
#' # with refptPlot
#' calcRefpts(resdf, ypr=TRUE, spar=0.3)
#'
#'
ypr.FLIBM <- function(
  obj = NULL,
  ssbfec = 1e6,
  FMs = seq(0,1,0.05),
  years = dimnames(obj$stock.a)$year,
  parallel = TRUE,
  no_cores = detectCores() - 1,
  clusterType = "PSOCK",
  outfile = "output.txt",
  seed = 1
){

  if(!0 %in% FMs){
    warning("FMs vector does not contain the value
      zero, needed in estimating virgin SSB")}

  if(!is.null(outfile)) {
    unlink(outfile)
  }

  if(is.null(seed)){
    seed <- round(runif(n = length(FMs), min = 1, max = 1e6))
  }

  if(length(seed) < length(FMs)){
    seed <- rep_len(x = seed, length.out = length(FMs))
  }

  parFun <- function(x) {
    library(FLIBM)
    set.seed(seed[x])
    obj.x <- obj
    RAN <- range(obj.x$stock.a)
    DIM <- dim(obj.x$stock.a@stock.n)
    DIMNAMES <- dimnames(obj.x$stock.a@stock.n)
    obj.x$harvest$params$FM <- FMs[x]
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

    for (year in years) {
      for (season in DIMNAMES$season) {
        # unit <- DIMNAMES$unit[1]
        # area <- DIMNAMES$area[1]
        # iter <- DIMNAMES$iter[1]

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
            # unit = unit, area = area, iter = iter,
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
            # unit = unit, area = area, iter = iter,
            monitor = FALSE)
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
    if (!is.null(outfile)) {
      sink(file = outfile, append = TRUE)
      print(paste(x, "of", length(FMs), "completed @",
        Sys.time()))
      sink()
    }
    return(c(unlist(list(FM = FMs[x], Catch = Ca.x, SSB = SSB.x,
        Recr = Recr.x, Lopt = Lopt, seed = seed[x]))))
  }

  if (parallel) {
    ARGS <- list("obj", "ssbfec", "FMs", "years", "parallel",
        "no_cores", "seed", "outfile")

    cl <- parallel::makeCluster(no_cores, type = clusterType)
    nn <- split(1:length(FMs), 1:length(FMs))
    parallel::clusterExport(cl, varlist = ARGS, envir = environment())
    res <- parLapply(cl, nn, parFun)
    stopCluster(cl)
  }

  if (!parallel) {
    res <- vector("list", length(FMs))
    for (x in seq(res)) {
      res[[x]] <- parFun(x)
    }
  }

  ret <- as.data.frame(do.call("rbind", res))
  return(ret)
}
