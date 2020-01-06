## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  cache=TRUE, cache.path='cache/',
  fig.path  ='tex/man-',
  fig.align='center', comment=NA,
  message=FALSE, warning=FALSE, echo=TRUE,
  fig.width=6, fig.height=4.5)

## ------------------------------------------------------------------------
df <- apply(stk$stock.a@harvest, 1:2, sum, na.rm=TRUE)
plot(df)

## ------------------------------------------------------------------------
# pulsed recruitment
stk$rec$params$season_wt[] <- 0
stk$rec$params$season_wt[3:5] <- c(0.25, 1, 0.25)

# seasonal growth oscillation
stk$growth$params['C'] <- 0.75

# fishing mortality
stk$harvest$params['FM'] <- 0.7

# spinup and advance
stk <- adv.FLIBM(stk, years = ac(2000:2009), monitor = FALSE)

# plot stock numbers
plot(stk$stock.a@stock.n)

## ------------------------------------------------------------------------
# summary stats
tmp <- simplifySeason(stk)
tmp <- tmp[ac(1:range(tmp)["max"]),] # remove age=0 group
plot(tmp)

## ------------------------------------------------------------------------
# plot length-frequency
lfq <- flquant2lfq(stk$stock.l@catch.n)
pal <- colorRampPalette(c("grey30",5,7,2), bias=2)
with(lfq, image(x=dates, y=midLengths, z=t(catch), col=pal(100)))

## ------------------------------------------------------------------------
# plot length-frequency
lfq <- flquant2lfq(stk$stock.l@catch.n)
pal <- colorRampPalette(c("grey30",5,7,2), bias=2)
with(lfq, image(x=dates, y=midLengths, z=t(catch), col=pal(100)))

