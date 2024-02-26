# Approximate Bayesian Computation on a model of the 
# reproductive process in natural fertility populations. 

# This program computes the model on a sample of locations in the parameter space
# and saves the results in a table the contains the parameter values for each
# simulation and the resulting summary statistics. 
# The summary statistics used to estimate the model are the set of age-specific
# fertility rates -- ages 10 to 49. 

# Hyperparameters ----
ini_c <- 5000   # size of initial cohort 
n <- 100        # nr of parallel runs 
iter <- 30000   # number of iterations

# Priors ----
beta1 <- runif(n*iter, 0.3, 0.9)      # spline parameter - peak fecundability
beta2 <- runif(n*iter, -0.1, 0) # spline parameter - fecundability decline
delta <-  runif(n*iter, 6, 18)            # mean length non suceptibility period 
mu_m <- runif(n*iter, 19, 27)           # mean log normal model age at union
sigma_m <- runif(n*iter, 4.5, 4.9)     # sd log normal model age at union

#sigma_m <- runif(n*iter, 2, 7)     # sd log normal model age at union


# parameters ----
param <- as.data.frame(cbind(beta1, beta2, delta, mu_m, sigma_m))
param_ls <- split(param, rep(1:iter, each = n))

# compute model ----
source(file.path("..","..","model","comfert_natural.R"))

# start cluster 
library(parallel)

s <- Sys.time()
cl <- makeCluster(n, type = "PSOCK")

# export objects
clusterExport(cl, c("ini_c", "param_ls", "comfert_natural"))

sim_ls <- lapply(param_ls,
                 function(x) parLapply(cl, 1:nrow(x),
                                       function(j) comfert_natural(ini_c = ini_c,
                                                                   beta1 = x[j,"beta1"],
                                                                   beta2 = x[j, "beta2"],
                                                                   delta = x[j,"delta"],
                                                                   mu_m = x[j,"mu_m"],
                                                                   sigma_m = x[j,"sigma_m"])))

stopCluster(cl)
e <- Sys.time()
print(e-s)


# add fx to param table
sim_ul <- unlist(sim_ls, recursive = F)
cnames <- sim_ul[[1]][,1]
sim_fx <- lapply(sim_ul, function(x) x[,2])
sim <- as.data.frame(do.call("rbind", sim_fx))
colnames(sim) <- as.character(cnames)
param <- cbind(param, sim)

# Save
saveRDS(param, file.path("results", paste0("ref_table_",ini_c,"_",n*iter,".rds")))

