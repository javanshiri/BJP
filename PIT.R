path <- "D:\\Project-proposal\\projects\\Lake Urmia\\csv\\models_prec\\monthly\\"
file.names <- dir (path, pattern= ".csv")
M <- length (file.names)

for ( m in 1:M) { 
  print(m)
  for (l in 1:4) {
    for (i in 1:12) {
      pathout <- paste0("D:\\Project-proposal\\projects\\Lake Urmia\\output\\prc\\", "Lead", l, "\\Initial", i, "\\BJP\\", gsub (".csv", "", file.names[m]), "_L",l, "_In", i )

# calculate f(y|x)
#####################
  pathoutput.mcmc <- paste0(pathout, "\\mcmc\\")
  setwd(pathoutput.mcmc)
  mcmc.sample <- dir(pathoutput.mcmc, pattern=".csv")
  length.sample <- length (mcmc.sample)
  
  PR <- c()
  V <- seq (0, 33, by=3)
    for ( v in V) {
    print(v)
    name <- paste0 (v , ".csv")
    P <- read.csv (name) [,-1]
    data_test <- read.csv (paste0(pathout, "\\data_test\\", name))[,-1]
    data_train <- read.csv (paste0(pathout, "\\data_train\\", name))[,-1]
    
    param <- read.csv (paste0(pathout,"\\param\\", name))[,-1]
    landa <- param[1:2]
    alpha <- param[3:4]
    y1_model <- data_test[,4]
    y2_obs <- data_test[,5]
    z1_model <- 1/landa[1] * log (sinh ( alpha[1] + landa[1] * y1_model))
    z2_obs <-  1/landa[1] * log (sinh ( alpha[1] + landa[1] * y2_obs))
    mu_perim <- lapply(z1_model, F= function (x) {P[,"mu.2."] + P[,"rho"] * P[,"sigma.2."] / P[,"sigma.1."] * (x - P[,"mu.1."])}) 
    sigma_perim_2 <- P[,"sigma.2."] ^ 2 * (1 - P[,"rho"] ^ 2)
    sigma_perim <- sqrt (sigma_perim_2)
    P_perim <- lapply (mu_perim, function(x)  {cbind(x, sigma_perim)})
    z2_dist <- lapply (P_perim, function(x) {apply (x, 1, F= function(y) {rnorm(1, y[1] ,y[2])} )} )
    
    F_z2 <- lapply(z2_dist, function(x) ecdf(x))
    
    #y2_dist <-  lapply (z2_dist, function(x) { (asinh (exp(x * landa[2]))- alpha[2])/landa[2]})
    #y2_dist_nonnegative <- lapply(y2_dist, function(x) { x[x < 0] <- 0;  return (x) })
    
    #F_y2 <- lapply(y2_dist_nonnegative, function(x) ecdf(x))
    lt <- length (z1_model)
    PROB <- c()
    for (t in 1: lt) {
      obs <- z2_obs[t]
      prob <-  F_z2[[t]] (obs)
      PROB <- c(PROB, prob)
    }
    PR <- c(PR, PROB)
    }
  hist ( PR, freq= FALSE, xlim= c(0,1), breaks=seq(0, 1, length.out=10), main= paste(), xlab="PIT value", ylab="Relative frequency", col="aquamarine2")
  write.csv(PR, paste0( pathout, "\\PIT.csv"))
  }
}}