## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  cache=TRUE, cache.path='cache/',
  fig.path  ='tex/assess-',
  fig.align='center', comment=NA,
  message=FALSE, warning=FALSE, echo=TRUE,
  fig.width=6, fig.height=4.5)

## ------------------------------------------------------------------------
# required packages
library(FLIBM)
library(FLCore)
library(ggplotFL)
library(data.table)
library(FLa4a)
library(FLXSA)
library(FLAssess)
library(FLBRP)
library(spict)
library(TropFishR)
library(MLZ)

## ------------------------------------------------------------------------
data(stk1) # load
stk <- stk1 # rename
plot(stk$stock.a@stock.n) # plot stock numbers

## ------------------------------------------------------------------------
# collapse season dimension of age-based FLStock
stkTrue <- simplifySeason(stk)

# create time series of SSB, and compare to yearly estimates
tmp2 <- as.data.frame(ssb(stkTrue), date=TRUE)
spwn_wt <- weighted.mean(0:11/12, stk$rec$params$season_wt)
tmp2$date <- as.POSIXct(yeardec2date(tmp2$year+spwn_wt))
plot(  apply(stk$stock.a@stock.n * stk$stock.a@stock.wt * stk$stock.a@mat,
  c(2,4),sum, na.rm=TRUE)) + 
  geom_point(mapping = aes(x=date, y=data), data = tmp2)

# exclude zero age class & redefine plusgroup to collapse rare ages
stkTrue <- stkTrue[ac(1:range(stkTrue)["max"]),] 
stkTrue <- setPlusGroup(stkTrue, plusgroup = 4, na.rm=TRUE) 

# fill in missing slots
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

## ------------------------------------------------------------------------
# Create Index
set.seed(1)
idx <- FLIndex(index=stock.n(stkTrue)/1000, type="numbers")
idx@catch.n <- idx@index
idx@effort[] <- 1
range(idx)[c("startf","endf")] <- c(0,0.01)
# add observation error
index(idx) <- index(idx)*exp(rnorm(1, index(idx)-index(idx), 0.1)) 
plot(idx)

## ------------------------------------------------------------------------
fmod <-  formula( ~ factor(age) + s(year, k=10) ) # fishing mortality submodel
srmod <- formula( ~ s(year, k=6)) # recruitment submodel
qmod <- list( ~ factor(age)) # survey catchability submodel

tmp <- sca(stkTrue, idx, srmodel=srmod, fmodel = fmod, qmodel = qmod)
stk_a4a <- stkTrue + tmp

# plot
plot(FLStocks(true=stkTrue, a4a=stk_a4a))
wireframe(harvest(stk_a4a), zlab="F")

## ------------------------------------------------------------------------
xsaCtrl <- FLXSA.control(tol = 1e-09, maxit=99, min.nse=0.3, fse=2.0,
  rage = -1, qage = range(stkTrue)["max"]-1, shk.n = TRUE, shk.f = TRUE,
  shk.yrs = 5, shk.ages= 5, window = 100, tsrange = 99, tspower = 0)

tmp <- FLXSA(stkTrue, idx, xsaCtrl)
stk_xsa <- stkTrue + tmp

# plot
plot(FLStocks(true=stkTrue, XSA=stk_xsa, a4a=stk_a4a))
wireframe(harvest(stk_xsa), zlab="F")

## ---- srr, results="hide"------------------------------------------------
srbh <- fmle(as.FLSR(stk_a4a, model="bevholt"), method="L-BFGS-B", 
         lower=c(1e-6, 1e-6), upper=c(max(rec(stk_a4a)) * 3, Inf))
plot(srbh) 

## ---- refpts-------------------------------------------------------------
# Calculate the reference points
brp <- brp(FLBRP(stk_a4a, srbh))
Fmsy <- c(refpts(brp)["msy","harvest"])
msy <- c(refpts(brp)["msy","yield"])
Bmsy <- c(refpts(brp)["msy","ssb"])
Bpa <- 0.5*Bmsy
Blim <- Bpa/1.4
plot(brp)

## ------------------------------------------------------------------------
## Catches
Caa <- apply(stk$stock.a@catch.n * stk$stock.a@catch.wt,
  1:2, sum, na.rm=TRUE)
Cyear <- as.data.frame(apply(Caa, 2, sum))


## Biomass indices
Baa <- stk$stock.a@stock.n * stk$stock.a@stock.wt
# sum exploited part (ages > 1)
Bseas <- apply(Baa[2:7,],c(2,4), sum, na.rm=TRUE)
Bseas <- as.data.frame(Bseas) 
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


## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
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

## ---- results="hide"-----------------------------------------------------
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

new.dataset@Lc <- 23
new.dataset <- calc_ML(new.dataset)

# new.dataset@MeanLength
# new.dataset@ss
new.dataset@vbLinf <- c(lfq3$par$Linf)
new.dataset@vbK <- lfq3$par$K[1]
new.dataset@vbt0 <- TropFishR::ta2t0(par = lfq3$par, L0 = 0, plot = FALSE)$t0
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


## ------------------------------------------------------------------------
lfq2 <- TropFishR::lfqCohort(lfq = lfq2, n.per.yr = 1, 
  calc_dt = TRUE)
lfq2 <- TropFishR::lfqRestructure(lfq2, MA = 5)

op <- par(mar = c(4,4,2,1), ps=10)
plot(lfq2, Fname = "rcounts",
  ylim = c(0, max(lfq2$midLengths)), image.col=NA)
PAL <- colorRampPalette(c("blue4", "cyan", "yellow", "red4"))
COL <- adjustcolor(PAL(length(unique(lfq2$cohort))), 0.7)
with(lfq2, image(x = dates, y = midLengths, z = t(cohort),
  col = COL,
  add=TRUE
))
par(op)


df <- data.frame(
  rel.age = c(lfq2$rel.age),
  n = c(lfq2$catch),
  cohort = c(lfq2$cohort),
  dt = c(lfq2$dt)
)
df$date <- as.Date(c(matrix(lfq2$dates, nrow = length(lfq2$midLengths), 
  ncol = length(lfq2$dates), byrow = TRUE)), origin = "1970-01-01")
df$midLength <- c(matrix(lfq2$midLengths, nrow = length(lfq2$midLengths), 
  ncol = length(lfq2$dates)))
df$year <- format(df$date, format = "%Y")

df$Ndt <- df$n/df$dt
agg <- aggregate(Ndt ~ rel.age + cohort, data = df, FUN = sum, na.rm=TRUE)
agg <- agg[-which(agg$Ndt == 0),]
agg$cohort <- factor(agg$cohort)


p <- ggplot(agg, aes(x = rel.age, y = log(Ndt))) + 
  geom_point(pch=1) +
  facet_wrap(~cohort, scales = "free") + 
  geom_smooth(method = "lm", se=FALSE,
    color="blue", lwd = 0.5, formula = y ~ x, 
    data = subset(agg, rel.age >= 1.5 & rel.age <= 7))
print(p)



PAL <- colorRampPalette(c("blue4", "cyan", "yellow", "red4"))
COL <- adjustcolor(PAL(length(levels(agg$cohort))), 0.7)
plot(log(Ndt)~rel.age, agg, col = COL[agg$cohort], pch=16)
legend("topright", legend = levels(agg$cohort), 
  col = COL, pch=16, title = "Cohort", bty = "n")
fit <- lm(log(Ndt)~rel.age+cohort, 
  subset(agg, rel.age >=1.5 & rel.age <= 5))
abline(fit)
summary(fit)
# anova(fit)
Zest <- coef(fit)["rel.age"]
Ztrue <- -1.7
((1-exp(Zest)) - (1-exp(Ztrue))) / (1-exp(Ztrue)) * 100 # perc error

# add prediction
L <- fit$xlevels
L$cohort <- factor(L$cohort, levels = levels(agg$cohort))
L$rel.age <- seq(0,6)
newdat <- expand.grid(L)
newdat$Ndt <- predict(fit, newdata = newdat)
for(i in levels(L$cohort)){
  tmp <- subset(newdat, cohort == i)
  if(nrow(tmp)>0){
    lines(Ndt ~ rel.age, data = tmp, col=COL[tmp$cohort], lwd=1)
  }
}
box()


