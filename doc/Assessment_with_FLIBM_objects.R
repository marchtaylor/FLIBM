## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  cache=FALSE, cache.path='cache/',
  fig.path  ='tex/assess-',
  fig.align='center', comment=NA,
  message=FALSE, warning=FALSE, echo=TRUE,
  fig.width=6, fig.height=4.5)

## -----------------------------------------------------------------------------
# required packages
library(FLIBM)
library(FLa4a)
library(FLBRP)
library(spict)
library(TropFishR)
library(MLZ)

## -----------------------------------------------------------------------------
data(stk1) # load
stk <- stk1 # rename
plot(stk$stock.a@stock.n) # plot stock numbers

## -----------------------------------------------------------------------------
# collapse season dimension of age-based FLStock
stkTrue <- simplifySeason(stk)

# exclude zero age class & redefine plusgroup to collapse rare ages
stkTrue <- stkTrue[ac(1:range(stkTrue)["max"]),] 
stkTrue <- setPlusGroup(stkTrue, plusgroup = 4, na.rm=TRUE) 

# create time series of SSB, and compare to yearly estimates
tmp2 <- as.data.frame(apply((stkTrue@stock.n * stkTrue@stock.wt * stkTrue@mat), 2, sum, na.rm = TRUE)) #as.data.frame(ssb(stkTrue))
# spwn_wt <- weighted.mean(0:11/12, stk$rec$params$season_wt)
tmp2$date <- yeardec2date(tmp2$year)#+spwn_wt)
tmp3 <- as.data.frame(apply((stk$stock.a@stock.n * stk$stock.a@stock.wt * stk$stock.a@mat), 
  MARGIN = c(2:6), sum, na.rm=TRUE))
tmp3$date <- yeardec2date(tmp3$year + (an(tmp3$season)-1)/12)
ggplot(data = tmp3) + aes(x = date, y = data) + geom_line() + 
  geom_point(mapping = aes(x=date, y=data), data = tmp2)


# fill in missing slots
stkTrue@catch.n[] <- replace(stkTrue@catch.n, is.na(stkTrue@catch.n), 0)
stkTrue@landings.n <- stkTrue@catch.n
stkTrue@landings.wt <- stkTrue@catch.wt
stkTrue@discards.n[] <- 0
stkTrue@discards.wt[] <- 0
stock(stkTrue) <- computeStock(stkTrue)
landings(stkTrue) <- computeLandings(stkTrue)
catch(stkTrue) <- computeCatch(stkTrue)
discards(stkTrue) <- computeDiscards(stkTrue)

# plot yearly FLStock
plot(stkTrue)

## -----------------------------------------------------------------------------
# Create Index
set.seed(1)
idx <- FLIndex(index=stock.n(stkTrue)/1000, type="numbers")
idx@catch.n <- idx@index
idx@effort[] <- 1
range(idx)[c("startf","endf")] <- c(0,0.01)
# add observation error
index(idx) <- index(idx)*exp(rnorm(1, index(idx)-index(idx), 0.1)) 
plot(idx)

## -----------------------------------------------------------------------------
fmod <-  formula( ~ factor(age) + s(year, k=10) ) # fishing mortality submodel
srmod <- formula( ~ s(year, k=6)) # recruitment submodel
qmod <- list( ~ factor(age)) # survey catchability submodel

tmp <- sca(stkTrue, idx, srmodel = srmod, fmodel = fmod, qmodel = qmod)
stk_a4a <- stkTrue + tmp

# plot
plot(FLStocks(true=stkTrue, a4a=stk_a4a))
wireframe(harvest(stk_a4a), zlab="F")

## ----srr, results="hide"------------------------------------------------------
srbh <- fmle(as.FLSR(stk_a4a, model="bevholt"), method="L-BFGS-B", 
         lower=c(1e-6, 1e-6), upper=c(max(rec(stk_a4a)) * 3, Inf))
plot(srbh) 

## ----refpts-------------------------------------------------------------------
# Calculate the reference points
brp <- brp(FLBRP(stk_a4a, srbh))
Fmsy <- c(refpts(brp)["msy","harvest"])
msy <- c(refpts(brp)["msy","yield"])
Bmsy <- c(refpts(brp)["msy","ssb"])
Bpa <- 0.5*Bmsy
Blim <- Bpa/1.4
plot(brp)

## -----------------------------------------------------------------------------
## Catches
Caa <- apply(stk$stock.a@catch.n * stk$stock.a@catch.wt,
  1:2, sum, na.rm=TRUE)
Cyear <- as.data.frame(apply(Caa, 2, sum))


## Biomass indices
# Baa <- stk$stock.a@stock.n * stk$stock.a@stock.wt
# # sum exploited part (ages > 1)
# Bseas <- apply(Baa[2:7,],c(2,4), sum, na.rm=TRUE)
# Bseas <- as.data.frame(Bseas) 
# Bseas$season <- as.numeric(as.character(Bseas$season))
# Bseas$yeardec <- Bseas$year + 
#   ((Bseas$season-1)/max(Bseas$season))
# Bseas <- Bseas[order(Bseas$yeardec),]
# Bseas$yeardec <- round(Bseas$yeardec, 3)

# scaled fishing mortality
Fsc <- apply(stk$stock.a@harvest, MARGIN = c(2:6), 
    FUN = function(x){x/max(x, na.rm = TRUE)})

tmp <- apply(stk$stock.a@harvest, MARGIN = c(2,4), 
    FUN = function(x){x/max(x, na.rm = TRUE)})
tmp <- apply(tmp, MARGIN = 1, sum, na.rm = TRUE)
tmp <- tmp/max(tmp)

tmp2 <- stk$stock.a@stock.n * stk$stock.a@stock.wt * c(tmp)
Bseas <- as.data.frame(apply(tmp2, c(2,4), sum, na.rm = T))
Bseas$season <- as.numeric(as.character(Bseas$season))
Bseas$yeardec <- Bseas$year +
  ((Bseas$season-1)/max(Bseas$season))
Bseas <- Bseas[order(Bseas$yeardec),]
Bseas$yeardec <- round(Bseas$yeardec, 3)

# start of year
idx1 <- subset(Bseas, season == 1)
# mid-year (only for second half of time-series)
idx2 <- subset(Bseas, season == 7 & year %in% 1990:1999)

## build input
inp <- list()
# catches
inp$timeC <- Cyear$year
inp$obsC <- Cyear$data
inp$dtc <- 1
# indices
inp$timeI <- list(idx1$yeardec, idx2$yeardec)
inp$obsI <- list(idx1$data, idx2$data)
inp$dteuler <- 1/12

# necessary to fix catchability, to indicate that index 
# represents absolute biomass
inp$ini$logq <- log(c(1,1))
inp$phases$logq <- -1
# inp$dtpredc <- 1

# check for errors
inp <- check.inp(inp)

# fit SPiCT and plot summary
fit <- fit.spict(inp)

# plot summary
op <- par(mfcol=c(2,2), mar = c(3,3,2,4), mgp = c(2,0.5,0), ps=8)
plotspict.biomass(fit)
plotspict.f(fit)
plotspict.production(fit)
plotspict.fb(fit)
par(op)


## -----------------------------------------------------------------------------
tmp <- as.data.frame(get.par("logB", fit, exp = TRUE))
tmp$yeardec <- round(as.numeric(rownames(tmp)),3)
tmp <- merge(x = Bseas, y = tmp, all.x=TRUE)

op <- par(mar = c(4,4,1,4))
plot(data ~ yeardec, tmp, t="l", ylim = c(0, max(tmp$data)))
grid()
text(max(tmp$yeardec), 0.95*max(tmp$data), 
  labels = paste("r.sq =", 
  round(cor(tmp$est, tmp$data)^2,4)),
  pos = 2)
par(new = TRUE)
plot(est ~ yeardec, tmp, t="l", ylim = c(0, max(tmp$est)), 
  col=2, lty=2, axes=FALSE, xlab="", ylab="")
axis(4, col=2, col.axis=2); mtext("est", col=2, side=4, 
  line=3)
par(op)

## -----------------------------------------------------------------------------
lfq <- flquant2lfq(stk$stock.l@catch.n)
pal <- colorRampPalette(c("grey30",5,7,2), bias=2)
with(lfq, image(x=dates, y=midLengths, z=t(catch), col=pal(100)))

lfq2 <- TropFishR::lfqModify(lfq, bin_size = 2, years = 1998:1999)
lfq2 <- TropFishR::lfqRestructure(lfq2)

lfq2 <- ELEFAN_GA(lfq2, seasonalised = FALSE, 
  low_par = list(Linf = 60, K = 0.1, ta = 0),
  up_par = list(Linf = 100, K = 2, ta = 1), 
  popSize = 80, maxiter = 100, pmutation = 0.2, 
  run = 20, monitor = FALSE, 
  MA = 5, parallel = TRUE, seed = 1)
unlist(lfq2$par)

plot(lfq2, Fname = "rcounts")

## ----results="hide"-----------------------------------------------------------
# make MLZ class
lfq3 <- TropFishR::lfqModify(lfq, aggregate = "year", bin_size = 2)
dim(lfq3$catch); length(lfq3$midLengths); length(lfq3$dates)
lfq3$par <- lfq2$par
plot(lfq3, Fname = "catch")

new.dataset <- new("MLZ_data", 
  Year = as.numeric(format(lfq3$dates, "%Y")),
  Len_bins = lfq3$midLengths,
  Len_matrix = t(lfq3$catch),
  length.units = "cm")

new.dataset@Lc <- stk$harvest$params$L50
new.dataset <- calc_ML(new.dataset)

# convert t_anchor to t0
t_anchor2t0 <- function(par = NULL, Lrecr = NULL){
  if(is.null(par) | is.null(Lrecr)){
    stop("Error: must provide both 'par' and 'Lrecr'")
  }
  if(Lrecr > par$Linf){stop("Error: 'Lrecr' must be lower than 'par$Linf'")}

  # add seasonalized pars if needed
  seasonalized <- !is.null(par$C)
  if(!seasonalized){par$ts <- 0; par$C <- 0}

  t <- seq(0, 3, 0.01)
  Lt <- VBGF(param = par, t = t)
  trecr <- t[min(which((Lt - Lrecr)>0))]
  # calc_trecr(par = par, Lrecr = Lrecr, Lt = tail(Lt,1), t = tail(t,1)) # same
  t0 <- par$t_anchor - trecr
  if(seasonalized){
    ts <- (par$ts - trecr)%%1
  } else {
    ts <- 0
  }
  par_age <- par
  par_age$t_anchor <- NULL
  par_age$t0 <- t0
  par_age$ts <- ts
  
  return(par_age)
}  

# new.dataset@MeanLength
# new.dataset@ss
new.dataset@vbLinf <- c(lfq3$par$Linf)
new.dataset@vbK <- lfq3$par$K[1]
new.dataset@vbt0 <- t_anchor2t0(par = lfq3$par, Lrecr = 0)$t0
plot(new.dataset)

model1 <- ML(new.dataset, ncp = 0, figure = FALSE)
model2 <- ML(new.dataset, ncp = 1, figure = FALSE)
model3 <- ML(new.dataset, ncp = 2, figure = FALSE)
compare_models(list(model1, model2, model3))
summary(model3)

# With Effort
yrs <- 1960:1999
steepness <- 0.75
FMmax <- 1
FMs <- FMmax / (1 + exp(-steepness * (yrs - 1985) ))
new.dataset@Effort <- FMs[which(yrs >= 1980)]
new.dataset@M <- 0.7


pred <- data.frame(Year = yrs[yrs >= 1980])
breaks <- c(-Inf, 
  model3@estimates[(model3@n.changepoint+2):
    (model3@n.changepoint+2+model3@n.changepoint-1),1], 
  Inf)
Zs <- model3@estimates[seq(model3@n.changepoint+1),1]
pred$Z <- Zs[cut(pred$Year, breaks = breaks)]

plot(yrs, FMs+0.7, t = "o", ylim = c(0,max(c(pred$Z, FMs+0.7))))
lines(Z ~ Year, pred, t="o", col=2)
legend("topleft", legend = c("True", "MLZ est."), col=1:2, pch=1, lty = 1)

# Mean length with effort mortality estimator (not working yet)
# MLeffort(new.dataset, start = list(q = 0.1, M = 0.7), n_age = 6, estimate.M = FALSE)


