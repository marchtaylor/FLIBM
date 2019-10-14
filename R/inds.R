#' Update variables of individuals
#'
#' @param obj bla
#' @param year bla
#' @param unit bla
#' @param season bla
#' @param area bla
#' @param iter bla
#'
#' @return FLIBM object
#' @export
#'
update.inds <- function(
  obj,
  year = NA,
  season = NA
){
  if(is.na(year) | is.na(season)){
    stop("Must specify 'year' and 'season' dimensions")
  }
  inds <- obj$inds

  ### dates and times
  yeardec <- as.numeric(year) + (as.numeric(season)-1)/dim(obj$stock.l@stock.n)[4]
  date <- FLIBM::yeardec2date(yeardec)
  if(season == dim(obj$stock.l@stock.n)[4]){ # if last season of year
    yeardec2 <- as.numeric(year)+1
    date2 <- FLIBM::yeardec2date(yeardec2)
  } else {
    yeardec2 <- as.numeric(year) + (as.numeric(season))/dim(obj$stock.l@stock.n)[4]
    date2 <- FLIBM::yeardec2date(yeardec2)
  }
  tincr <- yeardec2 - yeardec

  ### update m
  ARGS <- list()
  args.incl <- which(names(obj$m$params) %in% names(formals(obj$m$model)))
  if(length(args.incl)>0){ARGS <- c(ARGS, obj$m$params[args.incl])}
  args.incl <- names(inds)[names(inds) %in% names(formals(obj$m$model))]
  # if(length(args.incl)>0){ARGS <- c(ARGS, inds[args.incl])} # DF
  # inds$m <- do.call(obj$m$model, ARGS) # DF
  if(length(args.incl)>0){ARGS <- c(ARGS, inds[, args.incl, with = FALSE])} #DT
  inds[, m := do.call(obj$m$model, ARGS)] #DT

  ### update harvest
  ARGS <- list()
  args.incl <- which(names(obj$harvest$params) %in% names(formals(obj$harvest$model)))
  if(length(args.incl)>0){ARGS <- c(ARGS, obj$harvest$params[args.incl])}
  args.incl <- names(inds)[names(inds) %in% names(formals(obj$harvest$model))]
  # if(length(args.incl)>0){ARGS <- c(ARGS, inds[args.incl])} #DF
  # inds$harvest <- do.call(obj$harvest$model, ARGS) #DF
  if(length(args.incl)>0){ARGS <- c(ARGS, inds[, args.incl, with = FALSE])} #DT
  inds[, harvest := do.call(obj$harvest$model, ARGS)] #DT

  ### update mat
  # inds$mat <- ifelse((inds$length > inds$Lmat | inds$mat == 1), 1, 0) #DF
  new.mat <- ifelse((inds$length > inds$Lmat | inds$mat == 1), 1, 0) #DT
  inds[,mat := new.mat] #DT

  obj$inds <- inds
	return(obj)
}



#' Reproduce individuals
#'
#' @param obj bla
#' @param year bla
#' @param season bla
#'
#' @return FLIBM object
#' @export
#'
reproduce.inds <- function(
  obj,
  year = NA,
  season = NA
){
  if(is.na(year) | is.na(season)){
    stop("Must specify 'year' and 'season' dimensions")
  }
  inds <- obj$inds

  # dates and times
  yeardec <- as.numeric(year) + (as.numeric(season)-1)/dim(obj$stock.l@stock.n)[4]
  date <- FLIBM::yeardec2date(yeardec)
  if(season == dim(obj$stock.l@stock.n)[4]){ # if last season of year
    yeardec2 <- as.numeric(year)+1
    date2 <- FLIBM::yeardec2date(yeardec2)
  } else {
    yeardec2 <- as.numeric(year) + (as.numeric(season))/dim(obj$stock.l@stock.n)[4]
    date2 <- FLIBM::yeardec2date(yeardec2)
  }
  tincr <- yeardec2 - yeardec

  # calc. ssbfec
  ssbfec <- sum(inds$wt*inds$mat*inds$fec*inds$alive, na.rm=TRUE)

  # spawning calculation
  ARGS <- list(
    yeardec=yeardec, yeardec2=yeardec2,
    date=date, tincr=tincr,
    year=year, season=season,
    ssbfec=ssbfec)
  ARGS <- c(ARGS, obj$rec$params)
  args.incl <- which(names(ARGS) %in% names(formals(obj$rec$model)))
  ARGS <- ARGS[args.incl]
  n.recruits <- ceiling(
    c(do.call(obj$rec$model, ARGS) *
    obj$rec$covar[,year,1,season,1,1]))

  tseries <- expand.grid(
    year = as.numeric(dimnames(obj$rec$rec)[[2]]),
    season = (seq(dim(obj$rec$rec)[4])-1) / dim(obj$rec$rec)[4]
  )
  tseries <- tseries$year + tseries$season

  tmatch <- which.min((tseries - (yeardec+obj$rec$lag))^2)
  obj$rec$rec[1,,1,,1,1][[tmatch]] <- n.recruits

  n.recruits.now <- c(obj$rec$rec[,year,,season,,])
  if(n.recruits.now > 0){
    NAs <- which(is.na(inds$age)) # empty rows
    if(length(NAs) < n.recruits.now){ # if empty rows needed for new recruits, add
      addl <- n.recruits.now - length(NAs)
      # tmp <- as.data.frame(matrix(NA, nrow = addl*1.25, ncol=ncol(inds))) #DF
      # names(tmp) <- names(inds) #DF
      # inds <- rbind(inds, tmp) #DF

      tmp <- inds[rep(1, addl*1.10)]*NA #DT
      inds <- rbindlist(list(inds, tmp)) #DT
      NAs <- which(is.na(inds$age))
    }
    NAs_fill <- NAs[seq(n.recruits.now)] # empty rows to fill

    inds[NAs_fill,] <- obj$make.inds(n = n.recruits.now, age = obj$rec$lag, obj = obj)
  }

  obj$inds <- inds
	return(obj)
}





#' Die individuals
#'
#' @param obj bla
#' @param year bla
#' @param season bla
#'
#' @return FLIBM object
#' @export
#'
die.inds <- function(
  obj,
  year = NA,
  season = NA
){
  if(is.na(year) | is.na(season)){
    stop("Must specify 'year' and 'season' dimensions")
  }
  inds <- obj$inds

  # dates and times
  yeardec <- as.numeric(year) + (as.numeric(season)-1)/dim(obj$stock.l@stock.n)[4]
  date <- FLIBM::yeardec2date(yeardec)
  if(season == dim(obj$stock.l@stock.n)[4]){ # if last season of year
    yeardec2 <- as.numeric(year)+1
    date2 <- FLIBM::yeardec2date(yeardec2)
  } else {
    yeardec2 <- as.numeric(year) + (as.numeric(season))/dim(obj$stock.l@stock.n)[4]
    date2 <- FLIBM::yeardec2date(yeardec2)
  }
  tincr <- yeardec2 - yeardec

  # die?
  Z <- (inds$m + inds$harvest)
  # Z <- ifelse(inds$age == 0, 0, Z) ### don't apply mortality to new recruits
	pDeath <- 1 - exp(-Z*tincr)
	dead <- which(runif(nrow(inds)) < pDeath)
	# determine if death is natural or fished
	if(length(dead) > 0){
	  # inds$alive[dead] <- 0 #DF
	  inds[dead, alive := 0] #DT
	  # tmp <- cbind(inds$m[dead], inds$harvest[dead]) #DF
	  tmp <- inds[dead,.(m,harvest)] #DT
	  ### Fd = 1 for inds that died from fishing
	  Fd <- apply(tmp, 1, function(x){sample(c(0,1), size=1, prob=c(x[1], x[2]) )})
  	inds$Fd[dead] <- Fd
    rm(tmp)
	}

  obj$inds <- inds
	return(obj)
}





#' Record state of population
#'
#' @param obj bla
#' @param year bla
#' @param season bla
#'
#' @return FLIBM object
#' @export
#'
record.inds <- function(
  obj,
  year = NA,
  season = NA
){
  if(is.na(year) | is.na(season)){
    stop("Must specify 'year' and 'season' dimensions")
  }
  inds <- obj$inds

  # dates and times
  yeardec <- as.numeric(year) + (as.numeric(season)-1)/dim(obj$stock.l@stock.n)[4]
  date <- FLIBM::yeardec2date(yeardec)
  if(season == ac(dim(obj$stock.l@stock.n)[4])){ # if last season of year
    yeardec2 <- as.numeric(year)+1
    date2 <- FLIBM::yeardec2date(yeardec2)
  } else {
    yeardec2 <- as.numeric(year) + (as.numeric(season))/dim(obj$stock.l@stock.n)[4]
    date2 <- FLIBM::yeardec2date(yeardec2)
  }
  tincr <- yeardec2 - yeardec

  bday <- yeardec - inds$age
  cohort <- floor(yeardec) - floor(bday)

  # length-based bins
  inds$bin.l <- cut(
    inds$length,
    breaks = c(as.numeric(c(dimnames(obj$stock.l@stock.n)[[1]])), Inf),
    right = FALSE, include.lowest = TRUE
  )

  # age-based bins
  inds$bin.a <- cut(
    cohort,
    breaks = c(as.numeric(c(dimnames(obj$stock.a@stock.n)[[1]])), Inf),
    right = FALSE, include.lowest = TRUE
  )

  # dummy variable
  inds$n <- 1

  # died through natural mortality
  inds$Md <- ifelse(inds$alive==0 & inds$Fd == 0, 1, 0)

  # individuals alive at start of time step
  # indsStart <- subset(inds, age != 0) ### new recr exemption?

  # individuals that were fished
  indsFd <- subset(inds, Fd==1)

  ### length ###
  # n.l
  obj$stock.l@stock.n[,year,,season,,] <-
    tapply(inds$n,
      INDEX = list(length = inds$bin.l,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area=inds$area,
        iter = rep(dimnames(obj$stock.l@stock.n)$iter, nrow(inds))),
      FUN = sum, na.rm = TRUE, simplify = TRUE)
  # wt.l
  obj$stock.l@stock.wt[,year,,season,,] <-
    tapply(inds$wt,
      INDEX = list(length = inds$bin.l,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area=inds$area,
        iter = rep(dimnames(obj$stock.l@stock.n)$iter, nrow(inds))),
      FUN = mean, na.rm = TRUE, simplify = TRUE)
  # mat.l
  obj$stock.l@mat[,year,,season,,] <-
    tapply(inds$mat,
      INDEX = list(length = inds$bin.l,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area=inds$area,
        iter = rep(dimnames(obj$stock.l@stock.n)$iter, nrow(inds))),
      FUN = mean, na.rm = TRUE, simplify = TRUE)
  # catch.l
  obj$stock.l@catch.n[,year,,season,,] <-
    tapply(indsFd$Fd,
      INDEX = list(length = indsFd$bin.l,
        year = rep(factor(year), nrow(indsFd)),
        unit = indsFd$unit,
        season = rep(factor(season), nrow(indsFd)),
        area=indsFd$area,
        iter = rep(factor(dimnames(obj$stock.l@stock.n)$iter), nrow(indsFd))),
      FUN = sum, na.rm = TRUE, simplify = TRUE)
  # catch.wt.l
  obj$stock.l@catch.wt[,year,,season,,] <-
    tapply(indsFd$wt,
      INDEX = list(length = indsFd$bin.l,
        year = rep(factor(year), nrow(indsFd)),
        unit = indsFd$unit,
        season = rep(factor(season), nrow(indsFd)),
        area=indsFd$area,
        iter = rep(factor(dimnames(obj$stock.l@stock.n)$iter), nrow(indsFd))),
      FUN = mean, na.rm = TRUE, simplify = TRUE)

  # m & harvest rates
  # nStart <- tapply(inds$age != 0, inds$bin.l, sum, na.rm = TRUE) ###
  # nEnd <- tapply(inds$age != 0 & inds$alive, inds$bin.l, sum, na.rm = TRUE) ###
  # nStart <- tapply(inds$n, inds$bin.l, sum, na.rm = TRUE)
  # nEnd <- tapply(inds$alive, inds$bin.l, sum, na.rm = TRUE)

  n.l.End <-
    tapply(inds$alive,
      INDEX = list(length = inds$bin.l,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area=inds$area,
        iter = rep(dimnames(obj$stock.l@stock.n)$iter, nrow(inds))),
      FUN = sum, na.rm = TRUE, simplify = TRUE)

  n.l.Md <-
    tapply(inds$Md,
      INDEX = list(length = inds$bin.l,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area=inds$area,
        iter = rep(dimnames(obj$stock.l@stock.n)$iter, nrow(inds))),
      FUN = sum, na.rm = TRUE, simplify = TRUE)

  n.l.Fd <-
    tapply(inds$Fd,
      INDEX = list(length = inds$bin.l,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area=inds$area,
        iter = rep(dimnames(obj$stock.l@stock.n)$iter, nrow(inds))),
      FUN = sum, na.rm = TRUE, simplify = TRUE)

  # nMd <- tapply(inds$Md, inds$bin.l, sum, na.rm = TRUE)
  # nFd <- tapply(inds$Fd, inds$bin.l, sum, na.rm = TRUE)

  Z <- log( n.l.End / obj$stock.l@stock.n[,year,,season,,] ) * -1
  obj$stock.l@m[,year,,season,,] <- Z * ( n.l.Md / (n.l.Md + n.l.Fd) )
  obj$stock.l@harvest[,year,,season,,] <- Z * (n.l.Fd/(n.l.Md+n.l.Fd))

  # age-at-length
  # obj$age.l[,year,unit,season,area,iter] <-
    # tapply(indsStart$age, indsStart$bin.l, mean, na.rm = TRUE) ###
    # tapply(inds$age, inds$bin.l, mean, na.rm = TRUE)
  obj$age.l[,year,,season,,] <-
    tapply(inds$age,
      INDEX = list(length = inds$bin.l,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area=inds$area,
        iter = rep(dimnames(obj$stock.l@stock.n)$iter, nrow(inds))),
      FUN = mean, na.rm = TRUE, simplify = TRUE)




  ### age ###
  # n.a
  obj$stock.a@stock.n[,year,,season,,] <-
    tapply(inds$n,
      INDEX = list(length = inds$bin.a,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area=inds$area,
        iter = rep(dimnames(obj$stock.a@stock.n)$iter, nrow(inds))),
      FUN = sum, na.rm = TRUE, simplify = TRUE)
  # wt.a
  obj$stock.a@stock.wt[,year,,season,,] <-
    tapply(inds$wt,
      INDEX = list(length = inds$bin.a,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area=inds$area,
        iter = rep(dimnames(obj$stock.a@stock.n)$iter, nrow(inds))),
      FUN = mean, na.rm = TRUE, simplify = TRUE)
  # mat.a
  obj$stock.a@mat[,year,,season,,] <-
    tapply(inds$mat,
      INDEX = list(length = inds$bin.a,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area = inds$area,
        iter = rep(dimnames(obj$stock.a@stock.n)$iter, nrow(inds))),
      FUN = mean, na.rm = TRUE, simplify = TRUE)
  # catch.a
  obj$stock.a@catch.n[,year,,season,,] <-
    tapply(indsFd$Fd,
      INDEX = list(length = indsFd$bin.a,
        year = rep(factor(year), nrow(indsFd)),
        unit = indsFd$unit,
        season = rep(factor(season), nrow(indsFd)),
        area = indsFd$area,
        iter = rep(factor(dimnames(obj$stock.a@stock.n)$iter), nrow(indsFd))),
      FUN = sum, na.rm = TRUE, simplify = TRUE)
  # catch.wt.a
  obj$stock.a@catch.wt[,year,,season,,] <-
    tapply(indsFd$wt,
      INDEX = list(length = indsFd$bin.a,
        year = rep(factor(year), nrow(indsFd)),
        unit = indsFd$unit,
        season = rep(factor(season), nrow(indsFd)),
        area = indsFd$area,
        iter = rep(factor(dimnames(obj$stock.a@stock.n)$iter), nrow(indsFd))),
      FUN = mean, na.rm = TRUE, simplify = TRUE)

  # m & harvest rates
  # nStart <- tapply(inds$age != 0, inds$bin.l, sum, na.rm = TRUE) ###
  # nEnd <- tapply(inds$age != 0 & inds$alive, inds$bin.l, sum, na.rm = TRUE) ###
  # nStart <- tapply(inds$n, inds$bin.l, sum, na.rm = TRUE)
  # nEnd <- tapply(inds$alive, inds$bin.l, sum, na.rm = TRUE)

  n.a.End <-
    tapply(inds$alive,
      INDEX = list(length = inds$bin.a,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area = inds$area,
        iter = rep(dimnames(obj$stock.a@stock.n)$iter, nrow(inds))),
      FUN = sum, na.rm = TRUE, simplify = TRUE)

  n.a.Md <-
    tapply(inds$Md,
      INDEX = list(length = inds$bin.a,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area = inds$area,
        iter = rep(dimnames(obj$stock.a@stock.n)$iter, nrow(inds))),
      FUN = sum, na.rm = TRUE, simplify = TRUE)

  n.a.Fd <-
    tapply(inds$Fd,
      INDEX = list(length = inds$bin.a,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area = inds$area,
        iter = rep(dimnames(obj$stock.a@stock.n)$iter, nrow(inds))),
      FUN = sum, na.rm = TRUE, simplify = TRUE)

  # nMd <- tapply(inds$Md, inds$bin.l, sum, na.rm = TRUE)
  # nFd <- tapply(inds$Fd, inds$bin.l, sum, na.rm = TRUE)

  Z <- log( n.a.End / obj$stock.a@stock.n[,year,,season,,] ) * -1
  obj$stock.a@m[,year,,season,,] <- Z * ( n.a.Md / (n.a.Md + n.a.Fd) )
  obj$stock.a@harvest[,year,,season,,] <- Z * (n.a.Fd/(n.a.Md+n.a.Fd))

  # length-at-age
  obj$length.a[,year,,season,,] <-
    tapply(inds$length,
      INDEX = list(length = inds$bin.a,
        year = rep(year, nrow(inds)),
        unit = inds$unit,
        season = rep(season, nrow(inds)),
        area = inds$area,
        iter = rep(dimnames(obj$stock.a@stock.n)$iter, nrow(inds))),
      FUN = mean, na.rm = TRUE, simplify = TRUE)

  return(obj)
}





#' Remove dead individuals
#'
#' @param obj bla
#' @param year bla
#' @param season bla
#'
#' @return FLIBM object
#' @export
#'
remove.inds <- function(
  obj,
  year = NA,
  season = NA
){
  if(is.na(year) | is.na(season)){
    stop("Must specify 'year' and 'season' dimensions")
  }
  inds <- obj$inds

  # dead <- which(inds$alive == 0) #DF
  # if(length(dead)>0){ #DF
  #   inds[dead,] <- NA #DF
  # } #DF
  inds[alive == 0] <- NA #DT

  # occasionally remove empty slots (5% of time steps)
  randnum <- runif(1)
  if(randnum < 0.05){
    inds <- inds[!is.na(alive)]
  }

  obj$inds <- inds
  return(obj)
}





#' Grow individuals
#'
#' @param obj bla
#' @param year bla
#' @param season bla
#'
#' @return FLIBM object
#' @export
#'
grow.inds <- function(
  obj,
  year = NA,
  season = NA
){
  if(is.na(year) | is.na(season)){
    stop("Must specify 'year' and 'season' dimensions")
  }
  inds <- obj$inds

  # dates and times
  yeardec <- as.numeric(year) + (as.numeric(season)-1)/dim(obj$stock.l@stock.n)[4]
  date <- FLIBM::yeardec2date(yeardec)
  if(season == dim(obj$stock.l@stock.n)[4]){ # if last season of year
    yeardec2 <- as.numeric(year)+1
    date2 <- FLIBM::yeardec2date(yeardec2)
  } else {
    yeardec2 <- as.numeric(year) + (as.numeric(season))/dim(obj$stock.l@stock.n)[4]
    date2 <- FLIBM::yeardec2date(yeardec2)
  }
  tincr <- yeardec2 - yeardec

	### grow
  ARGS <- list()
  args.incl <- names(inds)[names(inds) %in% names(formals(obj$growth$model))]
  # if(length(args.incl)>0){ARGS <- c(ARGS, inds[args.incl])} #DF
  if(length(args.incl)>0){ARGS <- c(ARGS, inds[, args.incl, with = FALSE])} #DT
  args.incl <- names(obj$growth$params)[names(obj$growth$params) %in% names(formals(obj$growth$model))]
  # if(length(args.incl)>0){ARGS <- c(ARGS, inds[args.incl])} #DF
  if(length(args.incl)>0){ARGS <- c(ARGS, obj$growth$params[args.incl])}

  ARGS$t1 <- yeardec
  ARGS$t2 <- yeardec2
  L2 <- do.call(obj$growth$model, ARGS)
  # update length and weight
	# inds$length <- L2 # DF
	inds[, length := L2] # DT
	# inds$wt <- obj$growth$params$LWa*inds$length^obj$growth$params$LWb #DF
	new.wt <- obj$growth$params$LWa*inds$length^obj$growth$params$LWb
	inds[, wt := new.wt] #DT


	### age
	# inds$age <- inds$age + tincr #DF
	inds[, age := age + tincr] #DT

	obj$inds <- inds
	return(obj)
}
