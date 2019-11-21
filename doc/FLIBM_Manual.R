## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  cache=TRUE, cache.path='cache/',
  fig.path  ='tex/man-',
  fig.align='center', comment=NA,
  message=FALSE, warning=FALSE, echo=TRUE,
  fig.width=6, fig.height=4.5)

## ---- load_packages------------------------------------------------------
library(FLIBM)
library(FLCore)
library(ggplotFL)
library(data.table)
set.seed(42) # for reproducibility

## ---- createFLIBM--------------------------------------------------------
stk <- create.FLIBM(
  length = 0:85, age = 0:6, 
  year = ac(2000:2009),
  season = ac(1:12),
  n.units = "1e3", wt.units = "kg"
)

## ---- summaryFLIBM-------------------------------------------------------
summary(stk)

## ---- parAdj-------------------------------------------------------------
stk$rec$params['rmax'] <- 1e4

## ---- range--------------------------------------------------------------
range(stk$stock.a); range(stk$stock.l)

## ---- fbarAdj------------------------------------------------------------
range(stk$stock.a)[c("minfbar", "maxfbar")] <- c(1,3)
range(stk$stock.a)

## ---- spinup, paged.print=TRUE-------------------------------------------
stk <- spinup.FLIBM(obj = stk, nyearsslope = 10)

## ---- histLFQ------------------------------------------------------------
ggplot(stk$inds, aes(x = length)) + 
  theme(text = element_text(size=12)) + 
  geom_histogram(color = "black", fill = "white", 
    na.rm=TRUE)

## ---- stkAdv-------------------------------------------------------------
stk <- adv.FLIBM(stk, years = ac(2000:2003), monitor = FALSE)

# plot ssb
df <- as.data.frame(stock.n(stk$stock.a) 
  * stock.wt(stk$stock.a) * mat(stk$stock.a))
df$date <- as.Date(paste(as.numeric(df$year), 
  as.numeric(as.character(df$season)), "01", sep = "-"), 
  format = "%Y-%m-%d")
df <- aggregate(x = df$data, by = list(date = df$date), 
  FUN = sum, na.rm=TRUE)
df$x[df$x == 0] <- NA

p <- ggplot(data = df, aes(x = date, y = x)) +
  geom_line() +
  scale_y_continuous(limits = c(0,NA))
  print(p)

## ---- stkN---------------------------------------------------------------
# plot stock numbers
plot(stk$stock.a@stock.n)

## ---- simplify-----------------------------------------------------------
plot(simplifySeason(stk))

## ---- LFQ1---------------------------------------------------------------
# plot length-frequency
lfq <- flquant2lfq(stk$stock.l@catch.n)
pal <- colorRampPalette(c("grey30",5,7,2), bias=2)
with(lfq, image(x=dates, y=midLengths, z=t(catch), col=pal(100)))

## ---- mortality----------------------------------------------------------
df <- data.frame(
  age = as.numeric(dimnames(stk$stock.a@catch.n)$age), 
  n = c(apply(window(stk$stock.a@catch.n, start=2003, end=2003), 
    1, sum, na.rm=TRUE)))

plot(log(n) ~ age, df)
fit <- lm(log(n) ~ age, df, subset = age %in% 2:5)
points(log(n) ~ age, df, subset = age %in% 2:5, pch=20)
abline(fit, col=4)
usr <- par()$usr
text(0.9*usr[2], 0.9*usr[4], 
  labels = bquote(italic(Z)==.(sprintf("%.2f",-round(coef(fit)[2], 2)))), 
  pos = 2, col=4)

## ---- Lmat---------------------------------------------------------------
df <- as.data.frame(apply(stk$stock.l@mat[,"2003"], 
  1, mean, na.rm=TRUE))
df <- na.omit(df)
df$length <- df$length+0.5 # bin mid-length
fit <- glm(data ~ length, data = df, 
  family = binomial(link = "logit"))
pred <- predict(fit, se.fit=TRUE)
df <- cbind(df, pred)
# Calculate confidence intervals
std <- qnorm(0.95 / 2 + 0.5)
df$ymin <- fit$family$linkinv(df$fit - std * df$se.fit)
df$ymax <- fit$family$linkinv(df$fit + std * df$se.fit)
df$fit <- fit$family$linkinv(df$fit)  # Rescale to 0-1

plot(data ~ length, data = df, t="n", ylab="proportion mature")
points(data ~ length, data = df, col=adjustcolor(1, 0.5))
lines(fit ~ length, df, col=1)
lrPerc <- function(alpha, beta, p) (log(p/(1-p))-alpha)/beta
L50 <- lrPerc(alpha=coef(fit)[1], beta=coef(fit)[2], p=0.5)
lines(x=c(L50,L50,0), y=c(-100,0.5,0.5), lty=2, col=2)
text(x=L50, y=0.5, labels = bquote(L[mat]==.(round(L50,2))), pos=4, col=2 )

## ---- meanL--------------------------------------------------------------
df <- expand.grid(
  length = as.numeric(dimnames(stk$stock.l@catch.n)[[1]])+0.5,
  year = as.numeric(dimnames(stk$stock.l@catch.n)[[2]])
)
tmp <- apply(stk$stock.l@catch.n, 1:2, sum, na.rm=TRUE)
df$n = c(tmp)
df$lengthProd <- df$length * df$n

agg1 <- aggregate(cbind(lengthProd, n) ~ year, data = df, 
  FUN = sum, na.rm=TRUE)
agg1$meanLength <- agg1$lengthProd / agg1$n
plot(meanLength ~ year, agg1, t="b", ylim=range(df$length))
abline(h = L50, lty=2, col=8)

## ---- stkHarvest---------------------------------------------------------
df <- apply(stk$stock.a@harvest, 1:2, sum, na.rm=TRUE)
plot(df)

## ---- harvestDev---------------------------------------------------------
year <- as.numeric(dimnames(stk$stock.a@stock.n)[[2]])
steepness <- 1
FMmax <- 0.7*1.5
FMs <- FMmax / (1 + exp(-steepness * (year - 2004) ))

# spin-up with starting F
stk$harvest$params$FM <- FMs[1]
stk <- spinup.FLIBM(obj = stk, nyearsslope = 5, monitor = FALSE)
for(y in seq(year)){
  stk$harvest$params$FM <- FMs[y]
  stk <- adv.FLIBM(obj = stk, year = ac(year[y]), monitor = FALSE)
}

# plot stock numbers
plot(stk$stock.a@stock.n)

## ---- SSB----------------------------------------------------------------
# B
dat1 <- as.data.frame(stock.n(stk$stock.a) * stock.wt(stk$stock.a),
  date=TRUE)
dat1$age <- factor(dat1$age)

# SSB
dat2 <- as.data.frame(stock.n(stk$stock.a) * stock.wt(stk$stock.a) *
    mat(stk$stock.a), date=TRUE)
dat2 <- aggregate(x = dat2$data, by = list(date = dat2$date), 
  FUN = sum, na.rm=TRUE)
dat2$age <- factor("0")

p <- ggplot2::ggplot(data = dat1, 
  aes(x=date, y=data, fill=age, group=age)) +
  theme_set(theme_gray(base_size = 9)) +
  geom_area(position = 'stack') +
  scale_fill_brewer(palette="Spectral") +
  geom_line(data = dat2, aes(x=date, y=x) ) +
  ylab("Biomass (t)") +
  theme(axis.text.x=element_text(angle = 90, hjust=1, vjust = 0.5))
  print(p)

## ---- meanL2, echo=FALSE-------------------------------------------------
df <- expand.grid(
  length = as.numeric(dimnames(stk$stock.l@catch.n)[[1]])+0.5,
  year = as.numeric(dimnames(stk$stock.l@catch.n)[[2]])
)
tmp <- apply(stk$stock.l@catch.n, 1:2, sum, na.rm=TRUE)
df$n = c(tmp)
df$lengthProd <- df$length * df$n

agg1 <- aggregate(cbind(lengthProd, n) ~ year, data = df, 
  FUN = sum, na.rm=TRUE)
agg1$meanLength <- agg1$lengthProd / agg1$n
plot(meanLength ~ year, agg1, t="b", ylim=range(df$length))
abline(h = L50, lty=2, col=8)

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

## ---- recr_var-----------------------------------------------------------
# add log-normal noise to rec covar (one value per year)
stk$rec$covar[] <- rlnorm(n = dim(stk$rec$covar)[2], meanlog = 0, sdlog = 0.5)

stk <- adv.FLIBM(stk, years = ac(2000:2009), monitor = FALSE)

# plot stock numbers
plot(stk$stock.a@stock.n)

## ------------------------------------------------------------------------
# plot length-frequency
lfq <- flquant2lfq(stk$stock.l@catch.n)
pal <- colorRampPalette(c("grey30",5,7,2), bias=2)
with(lfq, image(x=dates, y=midLengths, z=t(catch), col=pal(100)))

