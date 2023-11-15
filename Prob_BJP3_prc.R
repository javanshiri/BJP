path <- "D:\\Project-proposal\\projects\\Lake Urmia\\output\\prc\\"
setwd (path)
file.names <- list.files()
M <- length (file.names)
for ( m in 1:M) {
  pathoutput.mcmc <- paste0(path, file.names[m], "\\mcmc\\")
  setwd(pathoutput.mcmc)
  mcmc.sample <- dir(pathoutput.mcmc, pattern=".csv")
  length.sample <- length (mcmc.sample)
  
  for (s in 1:length.sample)
  { 
    P <- read.csv(mcmc.sample[s]) [,-1]
    data_test <- read.csv (paste0(path, file.names[m],"\\data_test\\", mcmc.sample[s]))[,-1]
    landa <- read.csv (paste0(path, file.names[m],"\\landa\\", mcmc.sample[s]))[,-1]
    z1_model <- data_test[,4]
    mu_perim <- lapply(z1_model, F= function (x) {P[,"mu.2."] + P[,"rho"] * P[,"sigma.2."] / P[,"sigma.1."] * (x - P[,"mu.1."])}) 
    sigma_perim <- P[,"sigma.2."] ^ 2 * (1 - P[,"rho"] ^ 2)
    P_perim <- lapply (mu_perim, function(x)  {cbind(x, sigma_perim)})
    z2_dist <- lapply (P_perim, function(x) {apply (x, 1, F= function(y) {rnorm(1, y[1] , y[2])}) } )
    y2_dist <-  lapply (z2_dist, function(x) {1/landa[2] * asinh (exp(x * landa[2]))})
    
    Mean <- lapply(y2_dist, FUN = mean)
    compare <- cbind(Mean, data_test[,5], data_test[,4]*30 )
    
    #f_y2 <- lapply(y2_dist, function(x) approxfun(density(x)))
    F_y2 <- lapply(y2_dist, function(x) ecdf(x))
    lt <- length (z1_model)
    
    PROB <- c()
    for (t in 1: lt) {
      prob <- 1 - F_y2[[t]] (c(0.1, 2.5, 5, 10, 15, 25))
      PROB <- rbind(PROB, prob)
    }
    prob_obs <- cbind(PROB, data_test[,5])
    colnames (prob_obs) <- c("0.1", "2.5", "5", "10", "15", "25", "obs")
    write.csv(prob_obs, paste0( path, file.names[m], "\\prob\\", s, ".csv"))
  }
}
#####################################################  


