comfert_natural <- function(ini_c = 5000,
                            beta1 = 0.4221214,
                            beta2 = -0.06294189,
                            delta = 8.621253,
                            mu_m = 20.32025,
                            sigma_m = 1.183069
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
  
  # 
  maxt <- max(sapply(phi_i, length))
  
  for(t in 1:maxt){ 
    
    ids <- which(runif(ini_c) < sapply(phi_i, `[`, t)) 
    
    if(length(ids)!=0){ 
      
      wts <- sapply(ids, function(x) (wt_m[x]-1) + t) # wt to conception
      
      id <- c(id, ids) 
      wt_c <- c(wt_c, wts)
      
      #  non susceptibility period
      phi_i[ids] <- lapply(phi_i[ids], function(x){x[t:(t+9+delta)] <- NA; return(x)}) 
      
    }
    
  }
  
  age_birth <- (wt_c + 9)/12
  data <- as.data.frame(cbind(id, age_birth))[order(age_birth),]
  data$age_birth <- as.numeric(data$age_birth)
  births_by_age <- table(floor(data$age_birth))
  all_births <- rep(0, 51)
  names(all_births) <- 0:50
  all_births[names(births_by_age)] <- births_by_age
  fx <- all_births / ini_c
  fx <- as.data.frame(cbind(age = 10:49, fx[11:50]))
  
  return(fx)
  
}




