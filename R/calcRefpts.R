#' Plor results of ypr.FLIBM or Fmsy.FLIBM
#'
#' @param resdf bla
#' @param ypr bla
#' @param spar bla
#' @param y1lab bla
#' @param y2lab bla
#' @param xlab bla
#' @param mar bla
#' @param mgp bla
#' @param ps bla
#' @param Flab character Label used for y-axis. If `ypr = TRUE`, `Flab = "Fmax"`,
#'  else `Flab = "Fmsy"`
#' @param SPRlab character Label used for y-axis (default `SPRlab = "SPR"`)
#' @param plot logical Should plot be drawn (default `plot = TRUE`)
#'
#' @return list containing results and (optional) plot
#' @export
#'
#' @examples
#' data(stkMed)
#' stkMed$rec$params$rmax <- 1e4
#' resdf <- ypr.FLIBM(stkMed, ssbfec = 1e6, parallel = FALSE,
#'   FMs = seq(0,0.5,len=6), seed = 1)
#'
#' calcRefpts(resdf, ypr=TRUE, spar=0.3)
#'
calcRefpts <- function(
  resdf,
  ypr = TRUE,
  spar = 0.4,
  y1lab = NULL,
  y2lab = "SPR",
  xlab = "FM",
  Flab = NULL,
  SPRlab = "SPR",
  plot = TRUE,
  mar = c(3,3,1,3),
  mgp = c(2,0.5,0),
  ps=10
){

  if(!0 %in% resdf$FM) warning("FM vector does contain the value zero,
    needed for estimating virgin biomass and SPR")
  virginSSB <- mean(resdf$SSB[which(resdf$FM == min(resdf$FM))], na.rm = TRUE)
  resdf$SPR <- resdf$SSB / virginSSB

  if(ypr){
    if(!"Recr" %in% names(resdf)){stop("resdf must contain variable 'Recr' for ypr = TRUE")}
    resdf$CA <- resdf$Catch/resdf$Recr
    if(is.null(y1lab)) y1lab <- "YPR"
    if(is.null(Flab)) Flab <- "Fmax"
  } else {
    resdf$CA <- resdf$Catch
    if(is.null(y1lab)) y1lab <- "Catch"
    if(is.null(Flab)) Flab <- "Fmsy"
  }

  splCA <- smooth.spline(x = resdf$FM, y = resdf$CA, spar = spar)
  splSPR <- smooth.spline(x = resdf$FM, y = resdf$SPR, spar = spar)

  newdat <- data.frame(FM=seq(min(resdf$FM, na.rm = TRUE),
    max(resdf$FM, na.rm = TRUE),length.out = 500))
  newdat$CA <- predict(splCA, x=newdat$FM)$y
  newdat$CA.d1 <- predict(splCA, x=newdat$FM, deriv = 1)$y
  newdat$SPR <- predict(splSPR, x=newdat$FM)$y

  Fmax <- newdat[which.max(newdat$CA),]
  F01 <- newdat[which.min(((newdat$CA.d1/newdat$CA.d1[1])-0.1)^2),]

  if(plot){
    op <- par(mar = mar, mgp = mgp, ps = ps)
    plot(CA ~ FM, resdf, col=adjustcolor(1,0.3),
      ylim = c(0, max(resdf$CA, na.rm=TRUE)*1.2),
      yaxt="n", ylab = "", xaxs="i", yaxs="i", bty="[")
    lines(CA ~ FM, newdat, col=1)
    axis(2, col=1, col.axis = 1)
    mtext(y1lab, line = 2, side = 2, col=1)
    points(CA ~ FM, Fmax, pch=20, col=4)
    segments(x0 = Fmax$FM, y0 = Fmax$CA, x1 = Fmax$FM, y1 = 0, lty=2, col=4)
    text(x = Fmax$FM, y = Fmax$CA*0.5,
      labels = paste(Flab, "=", round(Fmax$FM, 3)), pos = 2, col=4, srt = 90)
    if(ypr){
      points(CA ~ FM, F01, pch=20, col=3)
      segments(x0 = F01$FM, y0 = F01$CA, x1 = F01$FM, y1 = 0, lty=2, col=3)
      text(x = F01$FM, y = F01$CA*0.5,
        labels = paste("F01 =", round(F01$FM, 3)), pos = 2, col=3, srt = 90)
    }

    par(new = TRUE)
    plot(SPR ~ FM, resdf, col=adjustcolor(2,0.2), bty="]",
      yaxt="n", ylab = "", yaxs="i", xaxs = "i", ylim = c(0,1))
    axis(4, col=2, col.axis = 2)
    mtext("SPR", line = 2, side = 4, col=2)
    lines(SPR ~ FM, newdat, col=2)
    points(SPR ~ FM, Fmax, pch=20, col=4)
    segments(x0 = Fmax$FM, y0 = Fmax$SPR, x1 = 100, y1 = Fmax$SPR, lty=2, col=4)
    text(x = mean(c(Fmax$FM, max(resdf$FM))), y = Fmax$SPR,
      labels = paste("SPR =", round(Fmax$SPR, 3)), pos = 3, col=4)
    if(ypr){
      points(SPR ~ FM, F01, pch=20, col=3)
      segments(x0 = F01$FM, y0 = F01$SPR, x1 = 100, y1 = F01$SPR, lty=2, col=3)
      text(x = mean(c(F01$FM, max(resdf$FM))), y = F01$SPR,
        labels = paste(SPRlab, "=", round(F01$SPR, 3)), pos = 3, col=3)
    }

    par(op)
  }


  if(ypr){
    res <- list(df = resdf, Fmax = Fmax, F01 = F01)
  } else {
    res <- list(df = resdf, Fmsy = Fmax)
  }
  return(res)

}
