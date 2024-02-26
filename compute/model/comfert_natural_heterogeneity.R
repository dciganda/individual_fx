comfert_natural_heterogeneity <- function(ini_c = 5000,
                            beta1 = 0.325 ,
                            beta2 = -0.027,
                            delta = 15,
                            mu_m = 25.7,
                            sigma_m = 5.4,
                            mh = 1,
                            sh = 0.2
){
  
  id <- vector()
  wt_c <- vector()
  
  # fecundability
  # scaling reproductive ages 10:50 in months
  t <- seq(0,1,1/480)
  # compute
  phi <- c(rep(0, 10*12), beta1 * (3*t^3-6*t^2+3*t) + beta2 * (-3*t^3+3*t^2))
  
  # wt marriage
  wt_m <- round(rlnorm(ini_c, log(mu_m^2/ sqrt(mu_m^2+sigma_m^2)), sqrt(log(1 + sigma_m^2/mu_m^2))),7) * 12 
  
  # fecundability after marriage
  phi_i <- lapply(1:ini_c, function(x) phi[wt_m[x]:length(phi)])
  
  # hereogeneity
  h <- rgamma(ini_c, shape = (mh^2) / (sh^2), rate = 1/(sh^2 / mh))
  
  phi_ih <- lapply(1:length(phi_i), function(x) phi_i[[x]]*h[x])
  
  # 
  maxt <- max(sapply(phi_ih, length))
  
  for(t in 1:maxt){ 
    
    ids <- which(runif(ini_c) < sapply(phi_ih, `[`, t)) 
    
    if(length(ids)!=0){ 
      
      wts <- sapply(ids, function(x) (wt_m[x]-1) + t) # wt to conception
      
      id <- c(id, ids) 
      wt_c <- c(wt_c, wts)
      
      #  non susceptibility period
      phi_ih[ids] <- lapply(phi_ih[ids], function(x){x[t:(t+9+delta)] <- NA; return(x)}) 
      
    }
    
  }
  
  age_birth <- (wt_c + 9)/12
  data <- as.data.frame(cbind(id, age_birth))[order(age_birth),]
  data$cum_nac <- cumsum(!is.na(data$id))
  data$cum_fec <- data$cum_nac/ini_c
  cum_rates <- sapply((by(data$cum_fec, floor(data$age_birth), max)),I)
  all_cum_rates <- setNames(rep(0, 51), as.character(0:50))
  all_cum_rates[names(cum_rates)] <- unlist(cum_rates)
  fx <- diff(all_cum_rates)
  fx[fx<0] <- 0
  fx <- as.data.frame(cbind(age = 10:49, fx[10:49]))
  
  return(fx)
  
}


