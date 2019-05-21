#' calc_t
#'
#' @param par bla
#' @param Ltnew bla
#' @param Lt bla
#' @param t bla
#' @param tincr bla
#'
#' @return numeric year decimal value
#' @export
#'
#' @examples
#'
#' plot(1)
#'
#'
calc_t <- function(
  par = NULL,
  Ltnew = NULL,
  Lt = NULL,
  t = NULL,
  tincr = 0.01
){
  # if(Lrecr > par$Linf){stop("Error: 'Lrecr' must be lower than 'par$Linf'")}
  # if(Lt > par$Linf){stop("Error: 'Lt' must be lower than 'par$Linf'")}
  if(Ltnew == Lt | Ltnew > par$Linf | Lt > par$Linf | Lt < 0){
    trecr <- NA
  }else{
    Lt.i <- Lt
    if(class(t)=="Date"){
      t.i <- date2yeardec(t)
    }else{
      if(class(t)=="numeric"){
        t.i <- t
      }else{
        stop("Error: t must be of class 'Date' or 'numeric'" )
      }
    }
    seasonalized <- !is.null(par$C)
    if(!seasonalized){par$ts <- 0; par$C <- 0}

    if(Ltnew < Lt.i){
      while(Lt.i > Ltnew){
        t2 <- t.i
        t1 <- t.i-tincr
        slope <- {1 - exp(-(
          par$K*(t2-t1)
          - (((par$C*par$K)/(2*pi))*sin(2*pi*(t1-par$ts)))
          + (((par$C*par$K)/(2*pi))*sin(2*pi*(t2-par$ts)))
        ))}
        Lt.i <- (par$Linf*slope - Lt.i) / (slope - 1)
        t.i <- t1
      }
      trecr <- t.i
    }

    if(Ltnew > Lt){
      while(Lt.i < Ltnew){
        t2 <- t.i+tincr
        t1 <- t.i
        slope <- {1 - exp(-(
          par$K*(t2-t1)
          - (((par$C*par$K)/(2*pi))*sin(2*pi*(t1-par$ts)))
          + (((par$C*par$K)/(2*pi))*sin(2*pi*(t2-par$ts)))
        ))}
        Lt.i <- (par$Linf*slope + Lt.i) / (slope + 1)
        t.i <- t2
      }
      tnew <- t.i
    }
  }

  # return result
  return(tnew)
}
