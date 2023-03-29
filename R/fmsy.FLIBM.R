#' Estimate maximum sustainable yeild (Fmsy) of an FLIBM object
#'
#' @description bla
#'
#' @param obj bla
#' @param FMs bla
#' @param years bla
#' @param yearsCompare bla
#' @param parallel bla
#' @param no_cores bla
#' @param clusterType bla
#' @param outfile bla
#' @param seed bla
#' @param resDir path to intermediate objects (default = \code{tempdir()}).
#'   Change directory if objects should be kept. Otherwise temporary directory
#'   is used, which is automatically deleted following the R session.
#' @param cleanup logical. Default TRUE.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \donttest{
#' data(stkMed)
#' stkMed$rec$params$rmax <- 1e2
#' resdf <- fmsy.FLIBM(stkMed, years = ac(2000:2009),
#'   yearsCompare = ac(2005:2009), no_cores = 3,
#'   FMs = seq(0,1,length.out = 6),
#'   cleanup = TRUE,
#'   parallel = TRUE
#' )
#' calcRefpts(resdf, ypr = FALSE, spar=0.15)
#' }
#'
#'
fmsy.FLIBM <- function(
  obj = NULL,
  FMs = seq(0,1,0.1),
  years = dimnames(obj$stock.a)$year,
  yearsCompare = dimnames(obj$stock.a)$year,
  parallel = TRUE,
  no_cores = detectCores() - 1,
  clusterType = "PSOCK",
  outfile = "output.txt",
  seed = 1,
  resDir = tempdir(),
  cleanup = TRUE
){

  if(!all(yearsCompare %in% years)) stop("all 'yearsCompare' must be in 'years'")

  if(!is.null(outfile)){unlink(outfile)} # delete old outfile

  if(is.null(seed)){
    seed <- round(runif(n = length(FMs), min = 1, max = 1e6))
  }

  if(length(seed) < length(FMs)){
    seed <- rep_len(x = seed, length.out = length(FMs))
  }

  parFun <- function(x){
    if(!is.null(outfile)){
      sink(file=outfile, append = TRUE)
      print(paste(x, "of", length(FMs), "started @", Sys.time()))
      sink()
    }

    library(data.table)
    set.seed(seed[x])

    # make copy
    obj.x <- obj
    RAN <- range(obj.x$stock.a)
    DIM <- dim(obj.x$stock.a@stock.n)
    DIMNAMES <- dimnames(obj.x$stock.a@stock.n)

    # update FM
    obj.x$harvest$params$FM <- FMs[x]

    # run
    obj.x <- FLIBM::adv.FLIBM(obj = obj.x, year = years, monitor = FALSE)
    fname.x <- tempfile(pattern=paste0(as.character(x),"_"), tmpdir = resDir,
      fileext = ".RData")

    save(obj.x, file = fname.x)

    if(!is.null(outfile)){
      sink(file=outfile, append = TRUE)
      print(paste(x, "of", length(FMs), "completed @", Sys.time()))
      sink()
    }

    return(fname.x)
  }

  if(parallel){ # Parallel version
    ARGS <- list(
      "obj",
      "FMs", "years",
      "parallel", "no_cores",
      "seed", "outfile", "resDir"
    )

    cl <- parallel::makeCluster(no_cores, type=clusterType)
    nn <- split(1:length(FMs), 1:length(FMs))
    parallel::clusterExport(cl, varlist = ARGS, envir=environment())
    res <- parLapply(cl, nn, parFun)
    stopCluster(cl)

  }

  if (!parallel) {
    res <- vector("list", length(FMs))
    for (x in seq(res)) {
      res[[x]] <- parFun(x)
    }
  }

  # setup FLStock for merging results

  objYr <- simplifySeason(obj)
  DIM <- dim(objYr)
  DIMNAMES <- dimnames(objYr)
  DIM[6] <- length(FMs)
  DIMNAMES[[6]] <- as.character(seq(FMs))
  tmpYr <- FLQuant(array(NaN, dim=DIM, dimnames=DIMNAMES))
  dim(tmpYr)
  objAllYr <- FLStock(tmpYr)

  for(x in seq(FMs)){
    nam <- load(file = res[[x]], verbose = TRUE)
    tmp.x <- simplifySeason(get(nam))
    objAllYr[,,,,,x] <- tmp.x
  }
  units(objAllYr@harvest) <- "f"

  save(objAllYr, file = file.path(resDir, "objAllYr.RData"))

  # plot(ssb(objAllYr))

  Ca <- apply(objAllYr@catch[,yearsCompare], MARGIN = 6, FUN = sum, na.rm = TRUE)
  SSB <- apply(ssb(objAllYr[,yearsCompare]), MARGIN = 6, FUN = mean, na.rm = TRUE)
  Ca <- replace(c(Ca), is.na(c(Ca)), 0)
  SSB <- replace(c(SSB), is.na(c(SSB)), 0)

  ret <- data.frame(FM = FMs, Catch = c(Ca), SSB = c(SSB), fname = unlist(res), seed = seed)


  if(cleanup){
    file.remove(as.character(ret$fname))
  }

  return(ret)
}
