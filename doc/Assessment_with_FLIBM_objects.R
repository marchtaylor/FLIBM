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

