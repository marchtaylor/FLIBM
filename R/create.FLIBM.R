#' @title Create initial FLIBM object
#'
#' @param lengths vector. Lengths to define the length extent of
#'   the length-based FLStock object (lower bin extent).
#' @param ages vector. Ages to define the age extent of
#'   the age-based FLStock object (lower bin extent).
#' @param years vector. Years to define the 'year' extent of
#'   the FLStock object.
#' @param units vector. Unitss to define the 'unit' extent of
#'   the FLStock object.
#' @param seasons vector. Seasons to define the 'season' extent of
#'   the FLStock object.
#' @param areas vector. Areas to define the 'area' extent of
#'   the FLStock object.
#' @param iters vector. Iters to define the 'iter' extent of
#'   the FLStock object.
#' @param name character. Name of stock.
#' @param desc character. Description of stock.
#' @param inds data.frame. (Super-) individuals (rows) of the current
#'   population state with their attributes (column variables).
#' @param n.units character. Units decribing the scale for numbers
#'   used in the FLStock object. If value is higher than "1" (e.g. "1e3"),
#'   then individuals in the IBM are also considered as
#'   "super-individuals", representing a aggregate of individuals
#'   with like attributes. (Default="1e3")
#' @param wt.units character. Units decribing the scale for individual
#'   weights used in the FLStock object, and in the IBM. (Default="kg").
#'
#' @return FLIBM object
#'
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' stk <- create.FLIBM()
#' summary(stk)
#'
create.FLIBM <- function(
  lengths = seq(0,85),
  ages = 0:6,
  years = ac(2000:2009),
  units = "unique",
  seasons = ac(1:12),
  areas = "unique",
  iters = "1",
  n.units = "1e3",
  wt.units = "kg",
  name = "synStock",
  desc = "A synthetic stock",
  inds = NULL
){

  obj <- list()

  # empty FLQuant slots -------------------------------------------------------

  # non-length- & non-age-based
  flq <- FLCore::FLQuant(
    NaN,
    dim = c(1, length(years), length(units), length(seasons), length(areas), length(iters)),
    dimnames = list(age = "all", year = years, unit = units, season = seasons, area = areas, iter = iters)
  )

  # age-based
  flq.a <- FLCore::FLQuant(
    NaN, quant = "age",
    dim = c(length(ages), length(years), length(units), length(seasons), length(areas), length(iters)),
    dimnames = list(age = ages, year = years, unit = units, season = seasons, area = areas, iter = iters)
  )

  # length-based
  flq.l <- FLCore::FLQuant(
    NaN, quant = "length",
    dim = c(length(lengths), length(years), length(units), length(seasons), length(areas), length(iters)),
    dimnames = list(length = lengths, year = years, unit = units, season = seasons, area = areas, iter = iters)
  )





  # FLStock objects ---------------------------------------------------------

  # length-based FLStock
  stock.l <- FLCore::FLStock(flq.l)
  stock.l@stock.n@units <- stock.l@catch.n@units <-
    stock.l@landings.n@units <- stock.l@discards.n@units <- ac(n.units)
  stock.l@stock.wt@units <- stock.l@catch.wt@units <-
    stock.l@landings.wt@units <- stock.l@discards.wt@units <- ac(wt.units)
  stock.l@harvest@units <- "f"


  # age-based FLStock
  stock.a <- FLCore::FLStock(flq.a)
  stock.a@stock.n@units <- stock.a@catch.n@units <-
    stock.a@landings.n@units <- stock.a@discards.n@units <- ac(n.units)
  stock.a@stock.wt@units <- stock.a@catch.wt@units <-
    stock.a@landings.wt@units <- stock.a@discards.wt@units <- ac(wt.units)
  stock.a@harvest@units <- "f"



  # IBM-related slots -------------------------------------------------------

  # growth - growth model and parameters
  obj$growth$model <- function(Linf, K, ts, C, length, t1, t2){
    dt <- (Linf - length) *
    {1 - exp(-(
      K*(t2-t1)
      - (((C*K)/(2*pi))*sin(2*pi*(t1-ts)))
      + (((C*K)/(2*pi))*sin(2*pi*(t2-ts)))
    ))}
    return(length + dt)
  }
  obj$growth$params <- list(
    popLinf = 80, popK = 0.5, C = 0, ts = 0.5, Linf.cv = 0.05, K.cv = 0.05,
    LWa = 0.01/1e3, LWb = 3, Lmat = 40, Lmat_w = 6, L0 = 0, L0.cv = 0
  )

  # rec - recruitment model and parameters
  obj$rec$model <- function(
    rmax = 1e4, beta = 500,
    ssbfec = 100, season = "1",
    season_wt = array(1, dim = length(seasons),
      dimnames = list(season=ac(seasons)))
  ){
    repro_wt <- season_wt[season]/sum(season_wt)
    nrecr <- (rmax*ssbfec/(beta+ssbfec))*repro_wt
    return(nrecr)
  }

  obj$rec$rec <- flq
  obj$rec$rec[] <- 0

  obj$rec$params <- list(rmax = 1e4, beta = 1,
    season_wt = array(1, dim = length(seasons),
      dimnames = list(season=ac(seasons))))
  obj$rec$covar <- replace(flq, is.na(flq), 1)

  obj$rec$lag <- 0

  # m - mortality model and parameters
  obj$m$model <- function(length){return(0.7)}
  obj$m$params <- NULL

  # harvest - fishing mortality model and parameters
  obj$harvest$model <- function(length, L50, wqs, FM){
    pSel <- 1 / (1 + exp(-(length - L50) /
        (wqs / ( log(0.75/(1-0.75)) - log(0.25/(1-0.25)) ))) )
    harvest <- FM * pSel
    return(harvest)
  }
  obj$harvest$params <- list(L50=20, wqs=4, FM=0.7)

  # record units for numbers and weight
  obj$n.units <- n.units
  obj$wt.units <- wt.units

  ### make.inds
  obj$make.inds <- function(
    n = 100, age = 0,
    obj
  ){
    inds <- data.table( # DT
      unit = factor(sample(units, size = n, replace = TRUE)),
      area = factor(sample(areas, size = n, replace = TRUE)),
      age = rep(age, n),
      length = 0, wt = 0,
      mat = 0, fec = 1,
      Lmat = NaN,
      m = 0, harvest = 0,
      Fd = 0, alive = 1,
      Linf = NaN, K = NaN
    )

    ### express inds
    # length
    inds$length <- obj$growth$params[["L0"]] * rlnorm(nrow(inds), 0, obj$growth$params[["L0.cv"]])
    # growth pars
    inds$Linf <- obj$growth$params[["popLinf"]] * rlnorm(nrow(inds), 0, obj$growth$params[["Linf.cv"]])
    inds$K <- obj$growth$params[["popK"]] * rlnorm(nrow(inds), 0, obj$growth$params[["K.cv"]])
    # weight
    inds$wt <- obj$growth$params[["LWa"]]*inds$length^obj$growth$params[["LWb"]]
    # maturity
    inds$Lmat <- rnorm(nrow(inds), mean=obj$growth$params[["Lmat"]],
      sd=obj$growth$params[["Lmat_w"]]/diff(qnorm(c(0.25, 0.75))))
    return(inds)
  }


  ### inds
  if(is.null(inds)){
    obj$inds <- obj$make.inds(n=1e3, obj = obj)
  }

  # length-at-age -----------------------------------------------------------
  length.a <- flq.a

  # age-at-length
  age.l <- flq.l

  # add FLStock objects
  obj$stock.l <- stock.l
  obj$stock.a <- stock.a

  # add other FLQuants
  obj$length.a <- flq.a # length-at-age
  obj$age.l <- flq.l # age-at-length


  class(obj) <- "FLIBM"
  return(obj)

}
