#' Calculate length quantile for a given logistic ogive
#'
#' @param L50 Length at prob 50\%
#' @param wqs width between 25\% and 75\% quantiles
#' @param targQ target probability quantile (Default = 95\%)
#'
#' @return numeric, length value corresponding to target quantile (`targQ`).
#' @export
#'
#' @examples
#' LQ(L50 = 50, wqs = 5, targQ = 0.95)
#'
#'
LQ <- function(L50, wqs, targQ=0.95){
  sc <- (wqs / ( log(0.75/(1-0.75)) - log(0.25/(1-0.25)) ) ) # scaling factor
  wqsTarg <- sc*(log(targQ/(1-targQ)) - log((1-targQ)/(targQ)) )
  return(L50+(wqsTarg/2))
}

