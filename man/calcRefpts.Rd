% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcRefpts.R
\name{calcRefpts}
\alias{calcRefpts}
\title{Plor results of ypr.FLIBM or Fmsy.FLIBM}
\usage{
calcRefpts(
  resdf,
  ypr = TRUE,
  spar = 0.4,
  y1lab = NULL,
  y2lab = "SPR",
  xlab = "FM",
  Flab = NULL,
  SPRlab = "SPR",
  plot = TRUE,
  mar = c(3, 3, 1, 3),
  mgp = c(2, 0.5, 0),
  ps = 10
)
}
\arguments{
\item{resdf}{bla}

\item{ypr}{bla}

\item{spar}{bla}

\item{y1lab}{bla}

\item{y2lab}{bla}

\item{xlab}{bla}

\item{Flab}{character Label used for y-axis. If `ypr = TRUE`, `Flab = "Fmax"`,
else `Flab = "Fmsy"`}

\item{SPRlab}{character Label used for y-axis (default `SPRlab = "SPR"`)}

\item{plot}{logical Should plot be drawn (default `plot = TRUE`)}

\item{mar}{bla}

\item{mgp}{bla}

\item{ps}{bla}
}
\value{
list containing results and (optional) plot
}
\description{
Plor results of ypr.FLIBM or Fmsy.FLIBM
}
\examples{
data(stkMed)
stkMed$rec$params$rmax <- 1e4
resdf <- ypr.FLIBM(stkMed, ssbfec = 1e6, parallel = FALSE,
  FMs = seq(0,0.5,len=6), seed = 1)

calcRefpts(resdf, ypr=TRUE, spar=0.3)

}
