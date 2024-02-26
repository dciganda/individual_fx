# ------- LOAD REF TABLES -------- 
# Load reference tables computed with the "compute_ref_tables.R" program (in /estimation) 
rf_161 <- readRDS(file.path("estimation","results", FILE))
rf_3235 <- readRDS(file.path("estimation", "results", FILE))
rf_5000 <- readRDS(file.path("estimation", "results", FILE))

rf <- list(rf_161, rf_3235, rf_5000)

# ------- CROSS VALIDATION -------- 
source(file.path("cross_validation","cv4abc_parallel.R"))
source(file.path("cross_validation","plot_cval_list.R"))

cval <- lapply(rf, function(x) cv4abc_parallel(param = x[,1:5],
                                               sumstat = x[,6:45],
                                               nval=100, 
                                               tol=0.00001, 
                                               method="rf"))

cval_ls <- list(cval_161, cval_3235, cval_5000)

plot_cval_list(cval_object_list = cval_ls, n = c(161,3235,5000), colour = F)

# ------- LOAD OBSERVED DATA -------- 
pops <- c("HT", "HY", "BC")
obs <- lapply(pops,
              function(x) read.csv(file.path("data", x,"fx.csv"), header = T)) 

# ------- ESTIMATION -------- 
source(file.path("estimation","abc_plus.R"))

# Compute posterior / regression adjustment
post <- lapply(1:length(rf), function(x) abc_plus(target = obs[[x]][,2], 
                                                  param = rf[[x]][,1:5],
                                                  sumstat = rf[[x]][,6:45],
                                                  tol = 0.00005, 
                                                  method = "rf",
                                                  ntree_rf = 500
))

# ------- POSTERIOR PREDICTIVE CHECKS --------
param <- lapply(post, function(x) as.data.frame(x$adj.values))
for(i in 1:length(obs)){
  names(param[[i]]) <- names(rf[[i]][1:5])
}
n <- 100        # nr of parallel runs 
iter <- ceiling(nrow(param[[1]])/n)    # number of iterations

param_ls <- lapply(param, function(x) split(x, rep(1:iter, each = n)))

# compute model 
# start cluster 
library(parallel)
cl <- makeCluster(n, type = "PSOCK")

# export objects
source(file.path("model","comfert_natural.R"))
ini_c <- c(161, 3235, 5000)    # size of initial cohort 


predpost <- list()
for(i in 1:length(param_ls)){
  
  inic <- ini_c[[i]]
  clusterExport(cl, c("inic", "param_ls", "comfert_natural"))
  predpost_ls <- lapply(param_ls[[i]],
                        function(x) parLapply(cl, 1:nrow(x),
                                              function(j) comfert_natural(ini_c = inic,
                                                                          beta1 = x[j,"beta1"],
                                                                          beta2 = x[j, "beta2"],
                                                                          delta = x[j,"delta"],
                                                                          mu_m = x[j,"mu_m"],
                                                                          sigma_m = x[j,"sigma_m"])))
  
  
  # add fx to param table
  predpost_ul <- unlist(predpost_ls, recursive = F)
  cnames <- predpost_ul[[1]][,1]
  predpost_fx <- lapply(predpost_ul, function(x) x[,2])
  predpost[[i]] <- as.data.frame(do.call("rbind", predpost_fx))
  
}

stopCluster(cl)

# ------- PLOT FIT -------- 
source(file.path("analysis", "plot_fit.R"))

plot_fit(pred_post = predpost[[1]], obs = obs[[1]], pop = "HT",
         x_coord = 42, y_coord = 0.55,margin_y = 0.1)
plot_fit(predpost[[2]], obs[[2]], pop = "HY",
         x_coord = 42, y_coord = 0.32, margin_y = 0.15)
plot_fit(predpost[[3]], obs[[3]], pop = "BC",
         x_coord = 42, y_coord = 0.42, margin_y = 0.15)


# ------- RESULTS: MECHANISMS --------  
source(file.path("analysis", "plot_estimated_phi.R"))
plot_estimated_phi(param = param, each_y = 0.01, margin_y = 0.01)

source(file.path("analysis", "plot_amenorrhea.R"))
input_vals <- list('Hutterites' = param[[1]][3]$delta,
                   'French Canadian' = param[[3]][3]$delta,
                   'Historic France' = param[[2]][3]$delta)

plot_amenhorrhea(pop = input_vals)

source(file.path("analysis", "plot_agem.R"))

plot_agem(obs_data = agem_ht, 
          estimated_mu = mean(param[[1]]$mu_m),
          estimated_sd = mean(param[[1]]$sigma_m),
          xlim = c(10,45),
          xlimit = 680,
          n_breaks = 20,
          pop = "HT",
          legend_x = 27,
          legend_y = 0.2)

plot_agem(obs_data = agem_hy, 
          estimated_mu = mean(param[[2]]$mu_m),
          estimated_sd = mean(param[[2]]$sigma_m),
          xlim = c(10,45),
          xlimit = 680,
          n_breaks = 50,
          pop = "HY",
          legend_x = 31,
          legend_y = 0.07)

plot_agem(obs_data = agem_bc, 
          estimated_mu = mean(param[[3]]$mu_m),
          estimated_sd = mean(param[[3]]$sigma_m),
          xlim = c(10,45),
          xlimit = 680,
          n_breaks = 50,
          pop = "BC",
          legend_x = 27,
          legend_y = 0.1)


# ------- RESULTS: PLOT POSTERIOR -------- 
source(file.path("analysis", "plot_posterior.R"))
lapply(1:3, function(x) lapply(1:5, function(j) plot_posterior(param[[x]][,j],
                                                               name = names(param[[x]][j]),
                                                               pop = pops[x])))

# ------- RESULTS: HETEROGENEITY ---------
# Load reference tables for the heterogeneity model 
rf_0.2 <- readRDS(file.path("estimation", "results", FILE))
rf_0.4 <- readRDS(file.path("estimation", "results", FILE))
rf_0.6 <- readRDS(file.path("estimation", "results", FILE))

rfh <- list(rf_3235, rf_0.2, rf_0.4, rf_0.6)

# load observed data
pops <- rep("HY",length(rfh))
obs <- lapply(pops,
              function(x) read.csv(file.path("data", x,"fx.csv"), header = T)) 

# estimation 
source(file.path("estimation","abc_plus.R"))

# Compute posterior / regression adjustment
post_h <- lapply(1:length(rfh), function(x) abc_plus(target = obs[[x]][,2], 
                                                  param = rfh[[x]][,1:5],
                                                  sumstat = rfh[[x]][,6:45],
                                                  tol = 0.00005, 
                                                  method = "rf",
                                                  ntree_rf = 500
))


param_h <- lapply(post_h, function(x) as.data.frame(x$adj.values))

source(file.path("analysis", "plot_estimated_phi_heterogeneity.R"))
plot_estimated_phi_heterogeneity(param_h)
