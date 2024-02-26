cv4abc_parallel <- function (param, sumstat, abc.out = NULL, nval, tol, statistic = "median", 
                             prior.range = NULL, method, hcorr = TRUE, transf = "none", 
                             logit.bounds = c(0, 0), kernel = "epanechnikov", 
                             numnet = 10, sizenet = 5, lambda = c(1e-04, 0.001, 0.01), 
                             trace = FALSE, maxit = 500, ...) {
  

  # Set number of parameters | statistics | simulations
  np <- dim(param)[2]
  numstat <- dim(sumstat)[2]
  # numsim <- dim(sumstat)[1] 
  numsim <- as.vector(which(rowSums(sumstat) != 0))   # FLAG :: REMOVE AFTER ANALYZING ABC.OBJECT
  
  # Names of parameters | statitics (taken from original object)
  paramnames <- colnames(param)
  statnames <- colnames(sumstat)

  # Weighted vector for statistics
  # gwt <- rep(TRUE, length(sumstat[,1]))
  gwt <- rep(TRUE, length(numsim))   # FLAG :: REMOVE AFTER ANALYZING ABC.OBJECT
  gwt[attributes(na.omit(sumstat))$na.action] <- FALSE
  
  # Indices of the sample used for cross validation
  cvsamp <- sample(numsim, nval, prob = gwt/sum(gwt))
  
  # Start a cluster
  cl <- makeCluster(nval)
  on.exit(stopCluster(cl))  # Ensure that the cluster is stopped when the function exits
  
  # Define a function that will be applied in parallel
  # This uses cvsamp as "observed" values
  
  get_estim_parallel <- function(i) {
    mysamp <- cvsamp[i]
    mytrue <- param[mysamp, ]
    mytarget <- sumstat[mysamp, ]
    myparam <- param[-mysamp, ]
    mysumstat <- sumstat[-mysamp, ]
    subres <- abc_plus(target = mytarget, 
                       param = myparam, sumstat = mysumstat, tol = tol,
                       method = method, transf = transf, 
                       logit.bounds = logit.bounds, kernel = kernel, 
                       hcorr = hcorr)
    
    # Statistic calculated (median | mean | mode) from the results
    if (statistic == "median") 
      estim <- invisible(summary(subres, print = FALSE)[3, ])
    if (statistic == "mean") 
      estim <- invisible(summary(subres, print = FALSE)[4, ])
    if (statistic == "mode") 
      estim <- invisible(summary(subres, print = FALSE)[5, ])
    
    return(estim)
  }
  
  # # Export necessary variables to the cluster
  clusterExport(cl, c("statistic", "cvsamp", "param", "sumstat", "tol",
                      "method", "transf", "logit.bounds", "kernel", "hcorr",
                      "get_estim_parallel"), envir=environment())
  clusterEvalQ(cl, source(file.path("functions","abc_plus.R")))
  
  # Use parLapply to apply the function in parallel
  r <- parLapply(cl, 1:nval, get_estim_parallel)
  res <- do.call(rbind, r)
  
  if (np == 1) 
    res <- c(res)
  else colnames(res) <- paramnames
  
  if (np == 1) {
    true <- as.data.frame(param[cvsamp, ])
    names(true) <- paramnames
  } else true <- param[cvsamp, ]
  
  # Create a cv4abc.out object to store results
  cv4abc.out <- list(cvsamples = cvsamp, tols = 1, 
                     true = true, estim = res, names = list(parameter.names = paramnames, 
                                                            statistics.names = statnames))

  class(cv4abc.out) <- "cv4abc"
  invisible(cv4abc.out)
  
}
