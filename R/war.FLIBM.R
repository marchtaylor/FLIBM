#' Calculate width at recruitment (WAR) of a single cohort
#'
#' @description The function `war.FLIBM` simulates a single cohort
#'   (i.e. spawning year) and estimates the width of the cohort length
#'   distribution through time (width between defined quantiles).
#'   The width at recruitment (`$war`) is then the width of the cohort when
#'   the lower quantile surpasses the length of recruitment to the fishery
#'   (`L50`). Results provide a way of estimating the `MA` setting that should
#'   be used within the function \code{\link[TropFishR]{lfqRestructure}}
#'   of the TropFishR package.
#'
#' @param obj FLIBM object
#' @param ssbfec numeric value. Single value describing constant spawning stock
#'   biomass (or fecundity), used in determining recruitment with \code{obj$rec}
#'   during the first year of simulation.
#' @param FM numeric value ofmaximum fishing mortality to be applied
#'   (Default: `FM = 0.2`).
#' @param years character vector. Years to use in simulation
#'   (from \code{ dimnames(obj$stock.a@stock.n)} )
#' @param qs numeric vector of length. Definesthe lower and upper quantiles to
#'   use in calculating `war` (Default: `qs = c(0.05, 0.95)`)
#' @param minN numeric value. Defines the minimum number of individuals
#'   required to estimate quantiles at a given time.
#' @param monitor logical. Should progression be printed.
#' @param plot logical. Should summary plot be drawn.
#'
#' @return list. Contains a summary data.frame with statistics over time
#'   over time (`$df`), and estimates of width at recruitment (`$war`),
#'   moving average (`MA`), and length at first capture (`L50`).
#'
#' @export
#'
#' @examples
#'
#' ## load data
#' data(stkMed)
#' stkMed$rec$params$rmax <- 1e4
#'
#' ## war analysis
#' set.seed(1111)
#' res <- war.FLIBM(obj = stkMed, FM = 0.2, years = ac(1980:1985),
#'   monitor = FALSE, plot = TRUE)
#'
#' ## estimated values
#' res$war # width at recruitment
#' res$MA # moving average setting given current bin size
#' res$L50 # length at first capture
#'
war.FLIBM <- function(
  obj = NULL,
  ssbfec = 1e6,
  FM = 0.2,
  years = dimnames(obj$stock.a)$year,
  qs = c(0.05, 0.95),
  minN = 100,
  monitor = FALSE,
  plot = TRUE
){

  obj <- window.FLIBM(obj = obj, start = as.numeric(years[1]),
    end = as.numeric(years[length(years)]))
  RAN <- range(obj$stock.a)
  DIM <- dim(obj$stock.a@stock.n)
  DIMNAMES <- dimnames(obj$stock.a@stock.n)
  obj$harvest$params$FM <- FM
  obj$stock.a@stock.n[] <- NaN
  obj$stock.a@catch.n[] <- NaN
  obj$stock.a@harvest[] <- NaN
  obj$stock.l@stock.n[] <- NaN
  obj$stock.l@catch.n[] <- NaN
  obj$stock.l@harvest[] <- NaN

  obj$inds[[1]][[1]][[1]] <- data.table::data.table()
  obj$rec$covar[] <- 0
  obj$rec$covar[,FLCore::ac(years[1])] <- 1
  recr.season <- seq(DIM[4])*NaN

  qlower <- qupper <- obj$rec$covar * NaN

  for (year in years) {
    for (season in DIMNAMES$season) {
      unit <- DIMNAMES$unit[1]
      area <- DIMNAMES$area[1]
      iter <- DIMNAMES$iter[1]

      if (year == years[1]) {
        yeardec <- as.numeric(year) + (as.numeric(season) -
          1)/dim(obj$stock.l@stock.n)[4]
        date <- FLIBM::yeardec2date(yeardec)
        if (season == dim(obj$stock.l@stock.n)[4]) {
          yeardec2 <- as.numeric(year) + 1
          date2 <- FLIBM::yeardec2date(yeardec2)
        }  else {
          yeardec2 <- as.numeric(year) + (as.numeric(season))/dim(obj$stock.l@stock.n)[4]
          date2 <- FLIBM::yeardec2date(yeardec2)
        }
        tincr <- yeardec2 - yeardec
        ARGS.x <- list(yeardec = yeardec, yeardec2 = yeardec2,
          date = date, tincr = tincr, year = year,
          unit = unit, season = season, area = area,
          iter = iter, ssbfec = ssbfec)
        ARGS.x <- c(ARGS.x, obj$rec$params)
        args.incl <- which(names(ARGS.x) %in% names(formals(obj$rec$model)))
        ARGS.x <- ARGS.x[args.incl]
        n.recruits <- ceiling(c(do.call(obj$rec$model,
          ARGS.x)))
        recr.season[match(season, DIMNAMES$season)] <- n.recruits
        if (n.recruits > 0) {
          newinds <- obj$make.inds(n = n.recruits,
            obj = obj)
          obj$inds[[1]][[1]][[1]] <- data.table::rbindlist(list(obj$inds[[1]][[1]][[1]],
            newinds))
        }
      }
      if (nrow(obj$inds[[1]][[1]][[1]]) > 0) {
        obj <- FLIBM::adv.FLIBM(obj = obj, year = year,
          season = season, unit = unit, area = area,
          iter = iter, monitor = monitor)

        qres <- quantile(obj$inds[[1]][[1]][[1]]$length, prob = qs, na.rm=TRUE)
        qlower[, year, unit, season, area, iter] <- qres[1]
        qupper[, year, unit, season, area, iter] <- qres[2]
      }
    }
  }

  qlower.df <- as.data.frame(qlower)
  qupper.df <- as.data.frame(qupper)
  stock.n <- as.data.frame(apply(obj$stock.a@stock.n, 2:6, sum, na.rm = TRUE))
  names(qlower.df)[7] <- "qlower"
  names(qupper.df)[7] <- "qupper"
  names(stock.n)[7] <- "N"

  qsdf <- merge(qlower.df, qupper.df)
  qsdf <- merge(qsdf, stock.n)

  qsdf$width <- qsdf$qupper - qsdf$qlower
  qsdf$yeardec <- date2yeardec(as.Date(paste(qsdf$year, qsdf$season, "01", sep="-")))
  qsdf <- qsdf[order(qsdf$yeardec),]
  qsdf$width[qsdf$N < minN] <- NaN

  Ls <- as.numeric(dimnames(obj$stock.l@stock.n)$length)
  maxL <- max(Ls)

  ARGS.x <- list()
  ARGS.x <- c(ARGS.x, obj$harvest$params)
  args.incl <- which(names(ARGS.x) %in% names(formals(obj$harvest$model)))
  ARGS.x <- ARGS.x[args.incl]
  ARGS.x$length <- seq(0,maxL,0.1)

  pcap <- do.call(obj$harvest$model, ARGS.x)
  L50 <- ARGS.x$length[min(which(pcap >= 0.5*(max(pcap))))]

  # results list
  res <- list(df = qsdf, L50 = L50)

  # estimate war and MA
  recruited <- which(qsdf$qlower >= L50)
  if(length(recruited)>0){
    war <- qsdf$width[min(which(qsdf$qlower >= L50))]
    MA <- round(war / (Ls[2]-Ls[1]))
    if(MA%%2 == 0) MA <- MA+1
    if(MA < 5) MA <- 5
  } else {
    warning("'war' and 'MA' could not be estimated as the stock did not fully recruit to the fishery")
    war = NaN
    MA = NaN
  }

  # update results list
  res <- c(res, list(war = war, MA = MA))

  if(plot){
    plot(qupper ~ yeardec, res$df, col = 1, t = "l", lty = 2, ylab = "length")
    lines(qlower ~ yeardec, res$df, col = 1, t = "l", lty = 2)
    suppressWarnings(rug(x = Ls, side = 2))
    abline(h = res$L50, col = 2)
    if(!is.na(res$war)){
      hit <- min(which(res$df$qlower >= res$L50))
      segments(x0 = res$df$yeardec[hit], x1 = res$df$yeardec[hit],
        y0 = res$df$qlower[hit], y1 = res$df$qupper[hit], col=4, lwd=2)
    }
    usr <- par()$usr
    cxy <- par()$cxy
    text(x = usr[1]+cxy[1], y = usr[4]-2*cxy[2],
      labels = paste(c("war =", "MA ="),
        c(round(res$war, 1), res$MA),
        collapse = "\n"),
       pos = 4, col = 4)
    text(x = usr[2]-cxy[1], y = usr[3]+cxy[2], labels = paste("L50 =", res$L50),
       pos = 2, col = 2)

  }

  return(res)
}
