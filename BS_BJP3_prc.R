library(verification)
###############################
BS <- function(x) {1/nrow(x)*sum((x[,1]-x[,2])^2)}
####################################################
path <- "D:\\Project-proposal\\projects\\Lake Urmia\\output\\prc\\"
path.eval <- "D:\\Project-proposal\\projects\\Lake Urmia\\evaluation\\"
setwd (path)
file.names <- list.files()
M <- length (file.names)
tr <- c(0.1, 2.5, 5, 10, 15, 25)
BS_all <- c()
for ( m in 1:M) {
  pathoutput.prob <- paste0(path, file.names[m], "\\prob\\")
  setwd(pathoutput.prob)
  Num.sample <- list.files() 
  S <- length (Num.sample)
  PROB <- c()
  for ( s in 1:S) {
    prob <- read.csv(Num.sample[s])
    PROB <- rbind (PROB, prob)
  }
    obs <- PROB[,8]
    BS_all_ther <- c()
    for (i in 1: 6) {
    t <- as.numeric(tr[i])
    obs_bin <- +(obs>t)
    pred <- PROB[,i+1]
    pred_obsbin <- data.frame (pred, obs_bin)
    N <- nrow (pred_obsbin)
    bs <- BS (pred_obsbin)  
    BS_all_ther <- c(BS_all_ther, bs)
    }
    BS_all <- rbind(BS_all, BS_all_ther)
}
colnames( BS_all) <- tr
row.names(BS_all) <- file.names
write.csv(BS_all, paste0 (path.eval, "BS_BJP_prc.csv") )


  