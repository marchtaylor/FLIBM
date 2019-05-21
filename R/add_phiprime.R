#' Add Phi' isolines to a Linf/K plot
#'
#' @param gridsize bla
#' @param ... bla
#'
#' @return contour layer
#' @export
#'
 add_phiprime <- function(gridsize = 20, ...){
    usr <- par()$usr
    if(par()$`xlog`){usr[1:2] <- 10^usr[1:2]}
    if(par()$`ylog`){usr[3:4] <- 10^usr[3:4]}

    usr <- replace(usr, which(usr < 0), 0)
    Linf <- seq(usr[1], usr[2], length.out = gridsize)
    K <- seq(usr[3], usr[4], length.out = gridsize)
    Linf <- Linf[which(Linf>=0)]
    K <- K[which(K>=0)]
    grd <- expand.grid(
      Linf = Linf,
      K = K
    )
    grd$phiL <- log10(grd$K) + 2 * log10(grd$Linf)

    M <- list(x = Linf, y = K, z = matrix(grd$phiL, nrow = gridsize, ncol = gridsize))
    contour(x = M, add = TRUE, ...)
  }
