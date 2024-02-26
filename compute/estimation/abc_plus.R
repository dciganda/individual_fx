######################################################################
#
# abc_plus.R
# Contains: abc, return.abc, print.abc, is.abc, summary.abc, hist.abc, plot.abc, normalise, namesWarningFilter
#
######################################################################

abc_plus <- function(target, param, sumstat, tol, method, 
                     hcorr = TRUE,
                     transf = "none",
                     logit.bounds = c(0,0),
                     kernel = "epanechnikov",
                     numnet = 10,
                     sizenet = 5,
                     lambda = c(0.0001,0.001,0.01),
                     trace = FALSE,
                     maxit = 500,
                     ntree_rf = 10, min_age = 10, max_age = 49, ...)
{
  
  normalise <- function(x,y){
    if(mad(y) == 0)
      return (x)
    else
      return (x/mad(y))
  }
  
  namesWarningFilter <- function(x){
    if( any( grepl( "No parameter names are given", x) ) ) invokeRestart( "muffleWarning" )
    if( any( grepl( "No summary statistics names are given", x) ) ) invokeRestart( "muffleWarning" )
  }
  
  call <- match.call()
  
  if(method == "rejection") rejmethod <- TRUE
  else rejmethod <- FALSE
  
  if(is.data.frame(param)) param <- as.matrix(param)
  if(is.data.frame(sumstat)) sumstat <- as.matrix(sumstat)
  if(is.list(target)) target <- unlist(target)
  if(is.vector(sumstat)) sumstat <- matrix(sumstat, ncol=1)
  
  ## stop if zero var in sumstat
  ## #########################
  nss <- length(sumstat[1,])
  cond1 <- !any(as.logical(apply(sumstat, 2, function(x) length(unique(x))-1)))
  if(cond1) stop("Zero variance in the summary statistics.")
  
  
  ## transformations
  ## ################
  ltransf <- length(transf)
  if(is.vector(param)){
    numparam <- 1
    param <- matrix(param, ncol=1)
  }
  else numparam <- dim(param)[2]
  for (i in 1:ltransf){
    if(sum(transf[i] == c("none","log","logit")) == 0){
      stop("Transformations must be none, log, or logit.")
    }
    if(transf[i]=="logit"){
      if(logit.bounds[i,1] >= logit.bounds[i,2]){
        stop("Logit bounds are incorrect.")       
      }
    }
  }
  
  
  ## no transformation should be applied when rejmethod is true
  if(rejmethod){
    if(!all(transf == "none")){
      warning("No transformation is applied when the simple rejection is used.", call.=F)
    }
    transf[1:numparam] <- "none"
  }
  else{
    if(numparam != ltransf){
      if(length(transf) == 1){
        transf <- rep(transf[1], numparam)
        warning("All parameters are \"", transf[1], "\" transformed.", sep="", call.=F)
      }
      else stop("Number of parameters is not the same as number of transformations.", sep="", call.=F)
    }
  }
  
  ## Weighted vector
  ## #######################################################
  gwt <- rep(TRUE,length(sumstat[,1]))
  gwt[attributes(na.omit(sumstat))$na.action] <- FALSE
  
  ## extract names of parameters and statistics if given
  ## ###################################################
  if(!length(colnames(param))){
    warning("No parameter names are given, using P1, P2, ...")
    paramnames <- paste("P", 1:numparam, sep="")
  }
  else paramnames <- colnames(param)
  
  if(!length(colnames(sumstat))){
    warning("No summary statistics names are given, using S1, S2, ...")
    statnames <- paste("S", 1:nss, sep="")
  }
  else statnames <- colnames(sumstat)
  
  ## scale everything
  ## #################
  
  scaled.sumstat <- sumstat
  for(j in 1:nss){
    scaled.sumstat[,j] <- normalise(sumstat[,j],sumstat[,j][gwt])
  }
  
  for(j in 1:nss){
    target[j] <- normalise(target[j],sumstat[,j][gwt])
  }
  
  ## calculate euclidean distance
  ## ############################
  sum1 <- 0
  for(j in 1:nss){
    sum1 <- sum1 + (scaled.sumstat[,j]-target[j])^2
  }
  dist <- sqrt(sum1)
  
  ## includes the effect of gwt in the tolerance
  dist[!gwt] <- floor(max(dist[gwt])+10)
  
  ## wt1 defines the distance
  
  ceiling(length(dist)*tol)->nacc
  sort(dist)[nacc]->ds
  wt1 <- (dist <= ds)
  aux<-cumsum(wt1)
  wt1 <- wt1 & (aux<=nacc)
  
  ## transform parameters
  ## ######################
  for (i in 1:numparam){
    if(transf[i] == "log"){
      if(min(param[,i]) <= 0){
        cat("log transform: values out of bounds - correcting...")
        x.tmp <- ifelse(param[,i] <= 0,max(param[,i]),param[,i])
        x.tmp.min <- min(x.tmp)
        param[,i] <- ifelse(param[,i] <= 0, x.tmp.min,param[,i])
      }
      param[,i] <- log(param[,i])
    }
    else if(transf[i] == "logit"){
      if(min(param[,i]) <= logit.bounds[i,1]){
        x.tmp <- ifelse(param[,i] <= logit.bounds[i,1],max(param[,i]),param[,i])
        x.tmp.min <- min(x.tmp)
        param[,i] <- ifelse(param[,i] <= logit.bounds[i,1], x.tmp.min,param[,i])
      }
      if(max(param[,i]) >= logit.bounds[i,2]){
        x.tmp <- ifelse(param[,i] >= logit.bounds[i,2],min(param[,i]),param[,i])
        x.tmp.max <- max(x.tmp)
        param[,i] <- ifelse(param[,i] >= logit.bounds[i,2], x.tmp.max,param[,i])
      }
      param[,i] <- (param[,i]-logit.bounds[i,1])/(logit.bounds[i,2]-logit.bounds[i,1])
      param[,i] <- log(param[,i]/(1-param[,i]))
    }
  } # end of parameter transformations
  
  ## select summary statistics by distance
  ## ###################################
  ss <- sumstat[wt1,]
  unadj.values <- param[wt1,]
  
  ## if simple rejection or in the selected region there is no var in sumstat
  ## ########################################################################
  if(rejmethod){
    weights <- rep(1,length=sum(wt1))
    adj.values <- NULL
    residuals <- NULL
    lambda <- NULL
  }else{
    
    ## weights
    if(kernel == "epanechnikov") weights <- 1 - (dist[wt1]/ds)^2
    
    ## regression correction
    ## ######################
    
    ## gaussian process
    if(method == "gp"){
      
      scaled.target <- target
      param_ls <- list()
      for (i in 1:numparam) {
        param_ls[[i]] <- param[wt1,i]
      }
      
      fitgp <- lapply(param_ls, function(x) laGP::newGP(scaled.sumstat[wt1, ], x, d = 10^6, g =  1e-6, dK = F))
      
      pred_obs <- lapply(fitgp, function(x) laGP::predGP(x, t(as.matrix(scaled.target)), lite = T))
      
      pred_sim <- lapply(fitgp, function(x) laGP::predGP(x, scaled.sumstat[wt1,], lite = T))
      
      fitted_val <- do.call("cbind", lapply(pred_sim, function(x) x$mean)) 
      
      residuals <- param[wt1, ] - fitted_val
      
      pred_obs_df <- do.call("cbind",lapply(pred_obs, function(x) x[1]$mean))
      pred_obs_df <- do.call("rbind", replicate(nrow(residuals), pred_obs_df, simplify = FALSE))
      
      adj.values <- pred_obs_df + residuals
    } # end of gp
    
    ## random forest
    ## ##########################
    if(method == "rf"){
      
      target.scaled <- target 
      adj.values <- matrix(nrow=nrow(param[wt1,]), ncol=numparam)
      residuals <- matrix(nrow=nrow(param[wt1,]), ncol=numparam)
      for(i in 1:numparam){
        data <- cbind(param[wt1,i], scaled.sumstat[wt1,])
        colnames(data)[-1] <- paste0( "age", colnames(data)[-1])
        colnames(data)[1] <- colnames(param)[i]
        rf_model <- randomForest::randomForest(data[,1] ~., data=data[,-1], ntree=ntree_rf)
        aux_target <- as.data.frame(t(target.scaled))
        colnames(aux_target) <- paste0( "age", min_age:max_age)
        pred <- predict(rf_model, aux_target)
        residuals[,i] <- param[wt1,i] - predict(rf_model, data[,-1])
        adj.values[,i] <-  pred + residuals[,i]
      }
    } # end of rf
    
    ## neural network regression
    if(method == "neuralnet"){
      linout <- TRUE
      
      ## normalise parameters
      param.mad <- c()
      for(i in 1:numparam){
        param.mad[i] <- mad(param[,i][gwt]) # save for renormalisation
        param[,i] <- normalise(param[,i],param[,i][gwt])
      }
      
      lambda <- sample(lambda, numnet, replace=T)
      fv <- array(dim=c(sum(wt1), numparam, numnet))
      pred <- matrix(nrow=numparam, ncol=numnet)
      for (i in 1:numnet){
        fit1 <- nnet::nnet(scaled.sumstat[wt1,], param[wt1,], weights = weights, decay = lambda[i],
                           size = sizenet, trace = trace, linout = linout, maxit = maxit, ...)
        cat(i)
        fv[,,i] <- fit1$fitted.values
        pred[,i] <- predict(fit1, data.frame(rbind(target)))
      }
      cat("\n")
      pred.med <- apply(pred, 1, median)
      pred.med <- matrix(pred.med, nrow=sum(wt1), ncol=numparam, byrow=T)
      fitted.values <- apply(fv, c(1,2), median)
      residuals <- param[wt1,] - fitted.values # median of fitted values par nnets for each accepted point and parameter
      
      if(hcorr == TRUE){
        pred2 <- matrix(nrow=numparam, ncol=numnet)
        fv2 <- array(dim=c(sum(wt1), numparam, numnet))
        for (i in 1:numnet){
          fit2 <- nnet::nnet(scaled.sumstat[wt1,], log(residuals^2), weights = weights, decay = lambda[i], size = sizenet, trace = trace, linout = linout, ...)
          cat(i)
          fv2[,,i] <- fit2$fitted.values
          pred2[,i] <- predict(fit2, data.frame(rbind(target)))
        }
        cat("\n")
        pred.sd <- sqrt(exp(apply(pred2, 1, median)))
        pred.sd <- matrix(pred.sd, nrow=sum(wt1), ncol=numparam, byrow=T)
        fv.sd <- sqrt(exp(apply(fv2, c(1,2), median)))
        adj.values <- pred.med + (pred.sd*residuals)/fv.sd # correction heteroscedasticity
        residuals<-(pred.sd*residuals)/fv.sd
      }
      else{
        adj.values <- pred.med + residuals
      }
      colnames(adj.values) <- colnames(unadj.values)
      
      ## renormalise
      for(i in 1:numparam){
        ##       residuals[,i] <- residuals[,i]*param.mad[i] not much sense...
        adj.values[,i] <- adj.values[,i]*param.mad[i]
      }
      
    } # end of neuralnet
    
  } # end of else than rejmethod
  
  ## back transform parameter values
  ## ################################
  if(numparam == 1){
    unadj.values <- matrix(unadj.values, ncol=1)
    if(method!="rejection")
    {adj.values <- matrix(adj.values, ncol=1)
    residuals <- matrix(residuals, ncol=1)}
  }
  
  for (i in 1:numparam){
    if(transf[i] == "log"){
      unadj.values[,i] <- exp(unadj.values[,i])
      adj.values[,i] <- exp(adj.values[,i])
    }
    else if(transf[i] == "logit"){
      unadj.values[,i] <- exp(unadj.values[,i])/(1+exp(unadj.values[,i]))
      unadj.values[,i] <- unadj.values[,i]*(logit.bounds[i,2]-logit.bounds[i,1])+logit.bounds[i,1]
      adj.values[,i] <- exp(adj.values[,i])/(1+exp(adj.values[,i]))
      adj.values[,i] <- adj.values[,i]*(logit.bounds[i,2]-logit.bounds[i,1])+logit.bounds[i,1]
    }
  }
  
  abc.return(transf, logit.bounds, method, call, numparam, nss, paramnames, statnames,
             unadj.values, adj.values, ss, weights, residuals, dist, wt1, gwt, lambda, hcorr,aic,bic)
  
}


abc.return <- function(transf, logit.bounds, method, call, numparam, nss, paramnames, statnames,
                       unadj.values, adj.values, ss, weights, residuals, dist, wt1, gwt, lambda, hcorr,aic,bic){
  
  if(method == "rejection"){
    out <- list(unadj.values=unadj.values, ss=ss, dist=dist,
                call=call, na.action=gwt, region=wt1, transf=transf, logit.bounds = logit.bounds,
                method="rejection", numparam=numparam, numstat=nss, names=list(parameter.names=paramnames, statistics.names=statnames))
  }
  else if(method == "neuralnet"){
    out <- list(adj.values=adj.values, unadj.values=unadj.values,
                ss=ss, weights=weights, residuals=residuals, dist=dist,
                call=call, na.action=gwt, region=wt1, transf=transf, logit.bounds = logit.bounds,
                method="neuralnet", hcorr = hcorr, lambda=lambda, numparam=numparam, numstat=nss,
                names=list(parameter.names=paramnames, statistics.names=statnames))
  }
  else if(method == "rf"){
    out <- list(adj.values=adj.values, unadj.values=unadj.values,
                ss=ss, weights=weights, residuals=residuals, dist=dist,
                call=call, na.action=gwt, region=wt1, transf=transf, logit.bounds = logit.bounds,
                method="rf", hcorr = hcorr, numparam=numparam, numstat=nss,
                names=list(parameter.names=paramnames, statistics.names=statnames))
  }
  else if(method == "gp"){
    out <- list(adj.values=adj.values, unadj.values=unadj.values,
                ss=ss, weights=weights, residuals=residuals, dist=dist,
                call=call, na.action=gwt, region=wt1, transf=transf, logit.bounds = logit.bounds,
                method="gp", hcorr = hcorr, numparam=numparam, numstat=nss,
                names=list(parameter.names=paramnames, statistics.names=statnames))
  }
  class(out) <- "abc"
  out
  
}

is.abc <- function(x){
  if (inherits(x, "abc")) TRUE
  else FALSE    
}

print.abc <- function(x, ...){
  if (!inherits(x, "abc")) 
    stop("Use only with objects of class \"abc\".", call.=F)
  abc.out <- x
  cl <- abc.out$call
  cat("Call:\n")
  dput(cl, control=NULL)
  
  cat("Method:\n")
  ## rejection
  if (abc.out$method == "rejection")
    cat("Rejection\n\n")
  ## loclinear
  else if (abc.out$method == "loclinear"){
    if(abc.out$hcorr == T){
      cat("Local linear regression\n")
      cat("with correction for heteroscedasticity\n\n")
    }
    else cat("Local linear regression\n\n")
  }
  ## ridge
  else if (abc.out$method == "ridge"){
    if(abc.out$hcorr == T){
      cat("Ridge regression\n")
      cat("with correction for heteroscedasticity\n\n")
    }
    else cat("Ridge regression\n\n")
  }
  ## nnet
  else if (abc.out$method == "neuralnet"){
    if(abc.out$hcorr == T){
      cat("Non-linear regression via neural networks\n")
      cat("with correction for heteroscedasticity\n\n")
    }
    else cat("Non-linear regression via neural networks\n\n")
  }
  cat("Parameters:\n")
  cat(abc.out$names$parameter.names, sep=", ")
  cat("\n\n")
  cat("Statistics:\n")
  cat(abc.out$names$statistics.names, sep=", ")
  cat("\n\n")        
  cat("Total number of simulations", length(abc.out$na.action), "\n\n")
  cat("Number of accepted simulations: ", dim(abc.out$unadj.values)[1], "\n\n")
  invisible(abc.out)
}

summary.abc <- function(object, unadj = FALSE, intvl = .95, print = TRUE, digits = max(3, getOption("digits")-3), ...){
  
  if (!inherits(object, "abc")) 
    stop("Use only with objects of class \"abc\".", call.=F)
  abc.out <- object
  np <- abc.out$numparam
  cl <- abc.out$call
  parnames <- abc.out$names[[1]]
  npri <- dim(abc.out$unadj.values)[1]
  npost <- dim(abc.out$unadj.values)[1]
  myprobs <- c((1-intvl)/2, 0.5, 1-(1-intvl)/2)
  
  if(print){
    cat("Call: \n")
    dput(cl, control=NULL)
  }
  
  if(abc.out$method == "rejection" || unadj){
    if(print) cat(paste("Data:\n abc.out$unadj.values (",npost," posterior samples)\n\n", sep=""))
    res <- matrix(abc.out$unadj.values, ncol=np)
    mymin <- apply(res, 2, min)
    mymax <- apply(res, 2, max)
    mymean <- apply(res, 2, mean)
    mymode <- apply(res, 2, getmode, ...)
    quants <- apply(res, 2, quantile, probs = myprobs)
    sums <- rbind(mymin, quants[1,], quants[2,], mymean, mymode, quants[3,], mymax)
    dimnames(sums) <- list(c("Min.:", paste(c(myprobs[1])*100, "% Perc.:", sep=""),
                             "Median:", "Mean:", "Mode:",
                             paste(c(myprobs[3])*100, "% Perc.:", sep=""), "Max.:"),
                           parnames)
  }
  else{
    if(print){
      cat(paste("Data:\n abc.out$adj.values (",npost," posterior samples)\n", sep=""))
      cat(paste("Weights:\n abc.out$weights\n\n", sep=""))
    }
    res <- matrix(abc.out$adj.values, ncol=np)
    wt <- abc.out$weights
    mymin <- apply(res, 2, min)
    mymax <- apply(res, 2, max)
    mymean <- apply(res, 2, weighted.mean, w = wt)
    mymode <- apply(res, 2, getmode, wt/sum(wt), ...)
    quants <- apply(res, 2, function(x) quantreg::rq(x~1, tau = myprobs, weights = wt)$coef)
    sums <- rbind(mymin, quants[1,], quants[2,], mymean, mymode, quants[3,], mymax)
    dimnames(sums) <- list(c("Min.:", paste("Weighted ", c(myprobs[1])*100, " % Perc.:", sep=""),
                             "Weighted Median:", "Weighted Mean:", "Weighted Mode:",
                             paste("Weighted ", c(myprobs[3])*100, " % Perc.:", sep=""), "Max.:"),
                           parnames)
  }
  class(sums) <- "table"
  if(print){
    if(length(digits) == 1) 
      print(round(sums, digits = digits), quote=FALSE)
    else
      print(apply(rbind(digits, sums), 2, function(a) round(a[-1], digits = a[1])), quote=FALSE)
    invisible(sums)
  }
  else sums
}

getmode <- function(x, weights = NULL, ...){
  
  ##  if(missing(bw) | missing(kernel) | missing(window) | missing(n)) warning("density.default() was used to calculate the posterior mode")
  d <- density(x, weights = weights)
  d$x[which(d$y == max(d$y))][1] 
}


hist.abc <- function(x, unadj = FALSE, true = NULL,
                     file = NULL, postscript = FALSE, onefile = TRUE, ask = !is.null(deviceIsInteractive()),
                     col.hist = "grey", col.true = "red", caption = NULL, ...){
  
  
  if (!inherits(x, "abc")) 
    stop("Use only with objects of class \"abc\".", call.=F)
  abc.out <- x
  np <- abc.out$numparam
  parnames <- abc.out$names[[1]]
  
  if(is.null(caption)) mycaption <- as.graphicsAnnot(parnames)
  else mycaption <- caption
  
  ## checks if true is given
  if(!is.null(true)){
    if(!is.vector(true)){
      stop("Supply true parameter value(s) as a vector.", call.=F)
    }
    if (length(true) != np){
      stop("Number of true values has to be the same as number of parameters in 'x'.", call.=F)
    }
    cond <- isTRUE(c(match(names(true), parnames), match(names(true), parnames)))
    if(cond) stop("Names do not match in 'x' and 'true'.", call.=F)
  }
  
  if(abc.out$method == "rejection") res <- abc.out$unadj.values
  else if (unadj){
    res <- abc.out$unadj.values
  }
  else res <- abc.out$adj.values
  
  ## Devices
  ## ##########
  save.devAskNewPage <- devAskNewPage()
  if(!is.null(file)){
    file <- substitute(file)
    if(!postscript) pdf(file = paste(file, "pdf", sep="."), onefile=onefile)
    if(postscript) postscript(file = paste(file, "ps", sep="."), onefile=onefile)
  }
  else{
    if (ask && prod(par("mfcol")) < np) {
      devAskNewPage(TRUE)
    }
  }
  
  myhist <- list()
  for(i in 1:np){
    myxlim <- range(res[,i], true[i])
    myhist[[i]] <- hist(res[,i], xlab = parnames[i], main = paste("Posterior histogram of ", mycaption[i], sep=""),
                        col = col.hist, xlim = myxlim, ...)
    myhist[[i]]$xname <- parnames[i]
    if(!is.null(true)){
      abline(v = true[i], lwd = 2, col = col.true)
    }
  }
  
  if(!is.null(file)){
    dev.off()
  }
  else devAskNewPage(save.devAskNewPage)
  
  names(myhist) <- parnames
  invisible(myhist)
  
} # end of hist.abc


##########################################################################################

## plots: for each parameter
## 1. prior density
## 2. posterior + prior density
## 3. distances vs parameters
## 4. histogram of the residuals

##########################################################################################
plot.abc_plus <- function(x, param, subsample = 1000, true = NULL,
                          file = NULL, postscript = FALSE, onefile = TRUE, ask = !is.null(deviceIsInteractive()), ...){
  
  if (!inherits(x, "abc")) 
    stop("Use only with objects of class \"abc\".", call.=F)
  
  abc.out <- x
  mymethod <- abc.out$method
  
  if(mymethod == "rejection")
    stop("Diagnostic plots can be displayed only when method is \"loclinear\", \"neuralnet\" or \"ridge\".", cal.=F)
  
  if(!is.matrix(param) && !is.data.frame(param) && !is.vector(param)) stop("'param' has to be a matrix, data.frame or vector.", call.=F)
  if(is.null(dim(param))) param <- matrix(param, ncol=1)
  if(is.data.frame(param)) param <- as.matrix(param)
  
  np <- abc.out$numparam
  numsim <- length(param)/np
  alldist <- log(abc.out$dist)
  myregion <- abc.out$region
  residuals <- abc.out$residuals
  parnames <- abc.out$names$parameter.names
  transf <- abc.out$transf
  logit.bounds <- abc.out$logit.bounds
  
  ##check if param is compatible with x
  cond <- isTRUE(c(match(colnames(param), parnames), match(parnames, colnames(param))))
  if(cond) stop("'abc.out' and 'param' are not compatible; paramater names are different.", call.=F)
  
  ## checks if true is given
  if(!is.null(true)){
    if(!is.vector(true)){
      stop("Supply true parameter value(s) as a vector.", call.=F)
    }
    if (length(true) != np){
      stop("Number of true values has to be the same as number of parameters in 'x'.", call.=F)
    }
    cond <- isTRUE(c(match(names(true), parnames), match(names(true), parnames)))
    if(cond) stop("Names do not match in 'x' and 'true'.", call.=F)
  }
  
  rej <- abc.out$unadj.values
  res <- abc.out$adj.values
  if(np == 1) rej <- matrix(rej, ncol=1)
  
  if(is.vector(param)){
    np.orig <- 1
    nsim <- length(param)
  }
  else if(is.matrix(param)){
    np.orig <- dim(param)[2]
    nsim <- dim(param)[1]
    
    myorder <- match(parnames, colnames(param))
    if(isTRUE(myorder-1:np)){
      param <- param[, myorder]
      warning("'param' is being re-ordered according to 'abc.out'...", call.=F, immediate.=T)
    }
  }
  
  ## check if param has the right dimensions
  if(np.orig != np){
    stop("The number parameters supplied in \"param\" is different from that in \"x\".", call.=F)
  }
  
  ## devices
  ## ########
  save.devAskNewPage <- devAskNewPage()
  if(!is.null(file)){
    file <- substitute(file)
    if(!postscript) pdf(file = paste(file, "pdf", sep="."), onefile=onefile)
    if(postscript) postscript(file = paste(file, "ps", sep="."), onefile=onefile)
  }
  else{
    if (ask && 1 < np) {
      devAskNewPage(TRUE)
    }
  }
  
  ## if param is too large, draw a random sample from it the size of
  ## which can be set by the user
  mysample <- sample(1:nsim, subsample)
  
  ## plots
  ## #####
  
  # prior, reg, rej, true
  ltys <- c(3, 1, 1, 3)
  lwds <- c(1, 2, 1, 2)
  cols <- c("black", "red", "black", "black")
  
  unadj <- TRUE
  for(i in 1:np){
    
    par(mfcol=c(2,2))    
    prior.d <- density(param[mysample,i])
    post.d <- density(res[,i])
    rej.d <- density(rej[,i])
    myxlim <- range(c(post.d$x, rej.d$x, true))
    myylim <- range(c(post.d$y, rej.d$y, prior.d$y))
    par(cex = 1, cex.main = 1.2, cex.lab = 1.1, lwd = 2)
    
    ##if(transf[i] == "none")
    myxlab <- parnames[i]
    ##else if(transf[i] == "log") myxlab <- paste("log(", parnames[i], ")", sep="")
    ##else if(transf[i] == "logit") myxlab <- paste("log(", parnames[i], "/(1-",parnames[i],"))", sep="")
    
    ## 1. prior
    ## ########
    plot(prior.d, main = "", col=cols[1], lty = ltys[1], lwd=lwds[1], xlab=myxlab, ...)
    title("Prior", sub = paste("N =", prior.d$n, "  Bandwidth =", formatC(prior.d$bw)))
    
    ## 2. posterior
    ## ############
    plot(post.d, col=cols[2], lty = ltys[2], lwd=lwds[2], xlim = myxlim, ylim = myylim, main = "", xlab=myxlab, ...)
    lines(prior.d, col=cols[1], lty = ltys[1], lwd=lwds[1], ...)
    lines(rej.d, col = cols[3], lty = ltys[3], lwd=lwds[3], ...)
    if(!is.null(true)){
      segments(x0 = true[i], x1 = true[i], y0 = myylim[1], y1 = myylim[2], col=cols[4], lty = ltys[4], lwd=lwds[4])
      mtext(text="True value", side = 1, line=2, at=true[i])
    }
    
    title(paste("Posterior with \"", mymethod, "\"", sep=""), sub=paste("N =", post.d$n, "  Bandwidth =", formatC(post.d$bw)))
    mtext("\"rejection\" and prior as reference", side=3, line = 0.5)
    
    ## 3. distances
    ## ############
    
    mypch <- 19
    myylim <- range(alldist[mysample])#*c(1,1.2)
    
    plot(param[mysample,i], alldist[mysample], col=cols[1],
         xlab=myxlab, ylab="log Euclidean distance", ylim=myylim, pch=mypch, ...)
    title("Euclidean distances")
    mtext(paste("N(All / plotted) = ", numsim, "/", subsample, sep=" "), side = 3, line = .5)
    points(param[myregion,i], alldist[myregion], col=cols[2], pch=mypch, ...)
    mypar <- par()
    if(!is.null(true)){
      segments(x0 = true[i], x1 = true[i], y0 = myylim[1], y1 = myylim[2], col=cols[4], lty = ltys[4], lwd=lwds[4])
      mtext(text="True value", side = 1, line=2, at=true[i])
    }
    
    ## 4. residuals
    ## ############
    
    if(mymethod=="gp") mymain <- "Residuals from gp()"
    if(mymethod=="rf") mymain <- "Residuals from rf()"
    if(mymethod=="neuralnet") mymain <- "Residuals from nnet()"
    qqnorm(residuals, pch=mypch, main=mymain, sub="Normal Q-Q plot", xlab="Theoretical quantiles", ylab="Residuals",...)
    qqline(residuals)
    
  } # np
  
  par(mfcol=c(1,1))
  
  if(!is.null(file)){
    dev.off()
  }
  else devAskNewPage(save.devAskNewPage)
  
  invisible()
  
} # end of plot.abc