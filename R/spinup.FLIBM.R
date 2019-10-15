#' @title Spin up of inds in FLIBM object
#'
#' @param obj FLIBM object
#' @param nyearsmax numeric. Maximum number of years for spin-up (Default=20)
#' @param nyearsslope numeric. Number of years for used to determin if spawning-
#' stock biomass is in equilibrium (via linear model of log SSB) (Default=5).
#' @param monitor logical. Should progression be printed.
#'
#' @return FLIBM object
#' @export
#'
#' @examples
#' stk <- create.FLIBM()
#' round(sum(stk$inds$wt * stk$inds$mat),3)
#' nrow(stk$inds)
#'
#' stk <- spinup.FLIBM(stk)
#' round(sum(stk$inds$wt * stk$inds$mat, na.rm=TRUE),3)
#' nrow(stk$inds)
#'
#'
#'
spinup.FLIBM <- function(
  obj,
  nyearsmax = 20,
  nyearsslope = 5,
  monitor = TRUE
){
  if(nyearsslope < 3) stop("'nyearsslope' must be >= 3")
  obj.copy <- obj
  DIMNAMES <- dimnames(obj$stock.l@stock.n)

  # for(u in unit){
  #   for(a in area){
  #     for(i in iter){
        # u = unit[1]; a = area[1]; i = iter[1]
        res <- data.frame(year = seq(nyearsmax))
        res$ssb <- NaN
        res$slope <- NaN
        res$trend <- 1
        for(y in res$year){
          obj.copy <- adv.FLIBM(obj = obj.copy,
            year = DIMNAMES$year[1],
            # unit = u, area = a, iter = i,
            monitor = FALSE
          )
          # test slope of ssb
          res$ssb[y] <- round(sum(obj.copy$inds$wt * obj.copy$inds$mat, na.rm=TRUE),3)
          if(y >= nyearsslope){
            dat <- subset(res, year %in% seq(y, (y - nyearsslope + 1)) & ssb > 0)
            fit0 <- lm(log(ssb) ~ 1, data = dat)
            fit1 <- lm(log(ssb) ~ 1 + year, data = dat)
            res$slope[y] <- round(fit1$coefficients[2],3)
            A <- AIC(fit0, fit1)
            if((A$AIC[2]+2) > A$AIC[1]){res$trend[y] <- 0}
          }
          if(monitor){
            cat(paste("year =", res$year[y], "| ssb =", res$ssb[y], "| trend =", res$trend[y]),"\n")
            flush.console()
          }
          # if no significant slope, end spin up
          if(res$trend[y] == 0){break()}
    #     }
    #   }
    # }
  }

  obj$inds <- obj.copy$inds
  return(obj)
}
