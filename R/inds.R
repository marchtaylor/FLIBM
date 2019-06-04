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
  unit = NA,
  season = NA,
  area = NA,
  iter = NA
){
  if(is.na(year) | is.na(unit) | is.na(season) | is.na(area) | is.na(iter)){
    stop("Must specify 'year', 'unit', 'season', 'area', and 'iter' dimensions")
  }
  inds <- obj$inds[[unit]][[area]][[iter]]

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

  obj$inds[[unit]][[area]][[iter]] <- inds
	return(obj)
}



#' Reproduce individuals
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
reproduce.inds <- function(
  obj,
  year = NA,
  unit = NA,
  season = NA,
  area = NA,
  iter = NA
){
  if(is.na(year) | is.na(unit) | is.na(season) | is.na(area) | is.na(iter)){
    stop("Must specify 'year', 'unit', 'season', 'area', and 'iter' dimensions")
  }
  inds <- obj$inds[[unit]][[area]][[iter]]

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
    year=year, unit=unit, season=season, area=area, iter=iter,
    ssbfec=ssbfec)
  ARGS <- c(ARGS, obj$rec$params)
  args.incl <- which(names(ARGS) %in% names(formals(obj$rec$model)))
  ARGS <- ARGS[args.incl]
  n.recruits <- ceiling(
    c(do.call(obj$rec$model, ARGS) *
    obj$rec$covar[,year,unit,season,area,iter]))

  tseries <- expand.grid(
    year = as.numeric(dimnames(obj$rec$rec)[[2]]),
    season = (seq(dim(obj$rec$rec)[4])-1) / dim(obj$rec$rec)[4]
  )
  tseries <- tseries$year + tseries$season

  tmatch <- which.min((tseries - (yeardec+obj$rec$lag))^2)
  obj$rec$rec[1,,unit,,area,iter][[tmatch]] <- n.recruits

  n.recruits.now <- c(obj$rec$rec[,year,unit,season,area,iter])
  if(n.recruits.now > 0){
    NAs <- which(is.na(inds$age)) # empty rows
    if(length(NAs) < n.recruits.now){ # if empty rows needed for new recruits, add
      addl <- n.recruits.now - length(NAs)
      # tmp <- as.data.frame(matrix(NA, nrow = addl*1.25, ncol=ncol(inds))) #DF
      # names(tmp) <- names(inds) #DF
      # inds <- rbind(inds, tmp) #DF

      tmp <- inds[rep(1, addl*1.25)]*NA #DT
      inds <- rbindlist(list(inds, tmp)) #DT
      NAs <- which(is.na(inds$age))
    }
    NAs_fill <- NAs[seq(n.recruits.now)] # empty rows to fill

    inds[NAs_fill,] <- obj$make.inds(n = n.recruits.now, age = obj$rec$lag, obj = obj)
  }

  obj$inds[[unit]][[area]][[iter]] <- inds
	return(obj)
}





#' Die individuals
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
die.inds <- function(
  obj,
  year = NA,
  unit = NA,
  season = NA,
  area = NA,
  iter = NA
){
  if(is.na(year) | is.na(unit) | is.na(season) | is.na(area) | is.na(iter)){
    stop("Must specify 'year', 'unit', 'season', 'area', and 'iter' dimensions")
  }
  inds <- obj$inds[[unit]][[area]][[iter]]

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
  obj$inds[[unit]][[area]][[iter]] <- inds
	return(obj)
}





#' Record state of population
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
record.inds <- function(
  obj,
  year = NA,
  unit = NA,
  season = NA,
  area = NA,
  iter = NA
){
  if(is.na(year) | is.na(unit) | is.na(season) | is.na(area) | is.na(iter)){
    stop("Must specify 'year', 'unit', 'season', 'area', and 'iter' dimensions")
  }
  inds <- obj$inds[[unit]][[area]][[iter]]

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

  inds$bday <- yeardec - inds$age
  inds$cohort <- floor(yeardec) - floor(inds$bday)

  # length-based bins
  inds$bin.l <- cut(
    inds$length,
    breaks = c(as.numeric(c(dimnames(obj$stock.l@stock.n)[[1]])), Inf),
    right = FALSE, include.lowest = TRUE
  )

  # age-based bins
  inds$bin.a <- cut(
    inds$cohort,
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
  obj$stock.l@stock.n[,year,unit,season,area,iter] <-
    # tapply(indsStart$n, indsStart$bin.l, sum, na.rm = TRUE) ###
    tapply(inds$n, inds$bin.l, sum, na.rm = TRUE)
  # wt.l
  obj$stock.l@stock.wt[,year,unit,season,area,iter] <-
    # tapply(indsStart$wt, indsStart$bin.l, mean, na.rm = TRUE) ###
    tapply(inds$wt, inds$bin.l, mean, na.rm = TRUE)
  # mat.l
  obj$stock.l@mat[,year,unit,season,area,iter] <-
    # tapply(indsStart$mat, indsStart$bin.l, mean, na.rm = TRUE) ###
    tapply(inds$mat, inds$bin.l, mean, na.rm = TRUE)

  # catch.l
  obj$stock.l@catch.n[,year,unit,season,area,iter] <-
    tapply(indsFd$Fd, indsFd$bin.l, sum, na.rm = TRUE)
  obj$stock.l@catch.wt[,year,unit,season,area,iter] <-
    tapply(indsFd$wt, indsFd$bin.l, mean, na.rm = TRUE)

  # m & harvest rates
  # nStart <- tapply(inds$age != 0, inds$bin.l, sum, na.rm = TRUE) ###
  # nEnd <- tapply(inds$age != 0 & inds$alive, inds$bin.l, sum, na.rm = TRUE) ###
  nStart <- tapply(inds$n, inds$bin.l, sum, na.rm = TRUE)
  nEnd <- tapply(inds$alive, inds$bin.l, sum, na.rm = TRUE)

  nMd <- tapply(inds$Md, inds$bin.l, sum, na.rm = TRUE)
  nFd <- tapply(inds$Fd, inds$bin.l, sum, na.rm = TRUE)
  Z <- log(nEnd/nStart) * -1
  obj$stock.l@m[,year,unit,season,area,iter] <- Z * (nMd/(nMd+nFd))
  obj$stock.l@harvest[,year,unit,season,area,iter] <- Z * (nFd/(nMd+nFd))

  # age-at-length
  obj$age.l[,year,unit,season,area,iter] <-
    # tapply(indsStart$age, indsStart$bin.l, mean, na.rm = TRUE) ###
    tapply(inds$age, inds$bin.l, mean, na.rm = TRUE)

  ### age ###
  # n.a
  obj$stock.a@stock.n[,year,unit,season,area,iter] <-
    # tapply(indsStart$n, indsStart$bin.a, sum, na.rm = TRUE) ###
    tapply(inds$n, inds$bin.a, sum, na.rm = TRUE)
  # wt.a
  obj$stock.a@stock.wt[,year,unit,season,area,iter] <-
    # tapply(indsStart$wt, indsStart$bin.a, mean, na.rm = TRUE) ###
    tapply(inds$wt, inds$bin.a, mean, na.rm = TRUE)
  # mat.a
  obj$stock.a@mat[,year,unit,season,area,iter] <-
    # tapply(indsStart$mat, indsStart$bin.a, mean, na.rm = TRUE) ###
    tapply(inds$mat, inds$bin.a, mean, na.rm = TRUE)

  # catch
  obj$stock.a@catch.n[,year,unit,season,area,iter] <-
    tapply(indsFd$Fd, indsFd$bin.a, sum, na.rm = TRUE)
  obj$stock.a@catch.wt[,year,unit,season,area,iter] <-
    tapply(indsFd$wt, indsFd$bin.a, mean, na.rm = TRUE)
  # m & harvest rates
  # nStart <- tapply(inds$age != 0, inds$bin.a, sum, na.rm = TRUE) ###
  # nEnd <- tapply(inds$alive & inds$age != 0, inds$bin.a, sum, na.rm = TRUE) ###
  nStart <- tapply(inds$n, inds$bin.a, sum, na.rm = TRUE)
  nEnd <- tapply(inds$alive, inds$bin.a, sum, na.rm = TRUE)

  nMd <- tapply(inds$Md, inds$bin.a, sum, na.rm = TRUE)
  nFd <- tapply(inds$Fd, inds$bin.a, sum, na.rm = TRUE)
  Z <- log(nEnd/nStart) * -1
  obj$stock.a@m[,year,unit,season,area,iter] <- Z * (nMd/(nMd+nFd))
  obj$stock.a@harvest[,year,unit,season,area,iter] <- Z * (nFd/(nMd+nFd))
  # length-at-age
  obj$length.a[,year,unit,season,area,iter] <-
    # tapply(indsStart$length, indsStart$bin.a, mean, na.rm = TRUE) ###
    tapply(inds$length, inds$bin.a, mean, na.rm = TRUE)

  return(obj)
}





#' Remove dead individuals
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
remove.inds <- function(
  obj,
  year = NA,
  unit = NA,
  season = NA,
  area = NA,
  iter = NA
){
  if(is.na(unit) | is.na(area) | is.na(iter)){
    stop("Must specify 'unit', 'area', and 'iter' dimensions")
  }
  inds <- obj$inds[[unit]][[area]][[iter]]

  # dead <- which(inds$alive == 0) #DF
  # if(length(dead)>0){ #DF
  #   inds[dead,] <- NA #DF
  # } #DF
  inds[alive == 0] <- NA #DT

  # occasionally remove empty slots (2% of time steps)
  randnum <- runif(1)
  if(randnum < 0.05){
    inds <- inds[!is.na(alive)]
  }

  obj$inds[[unit]][[area]][[iter]] <- inds
  return(obj)
}





#' Grow individuals
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
grow.inds <- function(
  obj,
  year = NA,
  unit = NA,
  season = NA,
  area = NA,
  iter = NA
){
  if(is.na(year) | is.na(unit) | is.na(season) | is.na(area) | is.na(iter)){
    stop("Must specify 'year', 'unit', 'season', 'area', and 'iter' dimensions")
  }
  inds <- obj$inds[[unit]][[area]][[iter]]

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

	obj$inds[[unit]][[area]][[iter]] <- inds
	return(obj)
}
