data_path <- file.path("..", "data", "final", pop,"in")
age_union_dist <- read.csv(file.path(data_path,"age_union_dist.csv"))
nsp <- floor(read.csv(file.path(data_path, "mean_nsp.csv"))[1,1]) # FLAG: Change to integere in original computation

mu_m <- 21.3
sigma_m <- 1.5

sim_am <- rlnorm(10^4, meanlog = base::log(mu_m)-base::log(sigma_m)^2/2,
                 sdlog = base::log(sigma_m)) 

hist(sim_am)


sim_am <- as.data.frame(table(floor(sim_am)))


plot(10+as.numeric(sim_am$Var1), sim_am$Freq/sum(sim_am$Freq), type = "l")
points(age_union_dist$age,age_union_dist$prop, col = "red")
