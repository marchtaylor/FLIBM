#' Calculate FM based on target catch
#'
#' @param obj bla
#' @param Ctarget bla
#' @param FMStart bla
#' @param lower bla
#' @param upper bla
#' @param year bla
#' @param unit bla
#' @param season bla
#' @param area bla
#' @param iter bla
#' @param seed bla
#' @param abstol numeric. Absolute tolerance for quota
#'
#' @return value
#'
#' @import FLash
#' @import FLCore
#' @importFrom stats optim
#'
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' stk <- create.FLIBM(year=ac(2000:2009))
#' range(stk$stock.a)[c("minfbar", "maxfbar")] <- c(1,3)
#'
#' stk <- spinup.FLIBM(stk, nyearsmax = 20, nyearsslope = 5)
#' stk <- adv.FLIBM(obj = stk, year = ac(2000:2004))
#' plot(stk$stock.a@stock.n)
#' plot(simplifySeason(stk))
#'
#'
#' # optim version
#' system.time(res <- calcFM(obj = stk, Ctarget = 1200, year = "2005", seed = 1))
#' res$par # best val
#' res$value # difference to target
#'
#' # check
#' set.seed(1)
#' stk2 <- stk
#' stk2$harvest$params$FM <- c(res$par)
#' stk2 <- adv.FLIBM(obj = stk2, year = "2005")
#'
#' # total catches
#' apply(stk2$stock.a@catch.n * stk2$stock.a@catch.wt, 2, sum, na.rm=TRUE)
#'
#' # fishing mortality
#' apply(harvest(stk2$stock.a),1:2, sum, na.rm=TRUE)
#' fbar(simplifySeason(stk2))
#'
#' }
#'
calcFM <- function(
  obj,
  Ctarget = 1000,
  FMStart = NULL,
  lower = 0.01,
  upper = 2,
  year = NULL,
  seed = 1,
  # popSize = 10, maxiter = 20,
  abstol = 5
){

  if(is.null(FMStart)){
    stkYr <- simplifySeason(obj)
    ran <- range(stkYr)
    prevYr <- ac(as.numeric(year)-1)
    stkYr <- stkYr[ac(1:ran["max"]),ac(ran["minyear"]:(as.numeric(year)-1))]

    mean_rec <- exp(mean(log(rec(stkYr))))
    stkYr_sr <- as.FLSR(stkYr, model="geomean")
    params(stkYr_sr)['a',] <- mean_rec

    stkYr_stf <- stf(stkYr, nyears=1, wts.nyears=3, na.rm=TRUE)
    ctrl_target <- fwdControl(data.frame(
      year = as.numeric(year),
      quantity = "catch",
      val = Ctarget))

    stkYr_stf <- fwd(stkYr_stf, ctrl_target, sr = stkYr_sr)
    fratio <- c(fbar(stkYr_stf)[,year] /  fbar(stkYr_stf)[,prevYr])

    FMStart <- obj$harvest$params$FM * fratio
  }

  objFun <- function(
    FM = 1
  ){
    obj.t2 <- obj
    obj.t2$harvest$params$FM <- FM
    set.seed(seed)
    obj.t2 <- adv.FLIBM(obj.t2,
      years = year,
      monitor = FALSE)
    Ca <- apply(obj.t2$stock.a@catch.n * obj.t2$stock.a@catch.wt,
      2, sum, na.rm=TRUE)
    return(abs(c(Ca[,year]) - Ctarget)) # optim cost
    # return(-abs(c(Ca[,year]) - Ctarget)) # ga fitness
  }

  # optim version
  res <- optim(FMStart, objFun,
    lower = FMStart*0.5, upper = FMStart*1.5,
    method = "Brent", control = list(trace = 4, abstol = abstol))

  # # GA version
  # res <- ga(type = "real-valued", fitness = objFun,
  #   lower = FMStart*0.5, upper = FMStart*1.5,
  #   suggestions = matrix(FMStart, 1, 1),
  #   seed = seed, popSize = popSize, maxiter = maxiter,
  #   maxFitness = -abstol)

  return(res)
}

