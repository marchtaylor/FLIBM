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
#' @param purgeProb numeric Value between 0 and 1, indicating the frequency
#'  that obj$inds are purged of empty rows (passed to `remove.ids`).
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \donttest{
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
#' calcRefpts(resdf, ypr=TRUE, spar=0.2)
#' }
#'
ypr.FLIBM <- function(
  obj = NULL,
  ssbfec = 1e6,
  FMs = seq(0,1,0.05),
  years = dimnames(obj$stock.a)$year,
  purgeProb = 0.2,
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

    res <- cohort.FLIBM(obj = obj, ssbfec = ssbfec,
      FM = FMs[x], years = years, seed = seed[x], purgeProb = purgeProb,
      return.FLIBM = FALSE)

    if (!is.null(outfile)) {
      sink(file = outfile, append = TRUE)
      print(paste(x, "of", length(FMs), "completed @",
        Sys.time()))
      sink()
    }

    return(c(unlist(res)))
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
