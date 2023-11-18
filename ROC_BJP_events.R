library(verification)
library(ROCR)
###########################
In <- function (m, l) {if (m-l+1 > 0 ) {i=m-l+1} else {i=m-l+13}
  return(i)}
###############################################################
p=1
event <- c("LN", "N", "MN")

Model <- c("BJP-CanCM4i", "BJP-CCSM4", "BJP-NEMO", "BJP-NASA")
for ( mo in 1:12) {
  png (paste0("D:\\Project-proposal\\projects\\Lake Urmia\\plot\\ROC_events\\", event[p], "\\roc_", event[p] , "_", mo, ".png"), res=70)
  par(mar=c(4,4,2,2),oma=c(0.2, 0.2, 0.2, 0.2), mfrow=c(2,2))
  P <- "D:\\Project-proposal\\projects\\Lake Urmia\\output\\prc\\"
  for ( l in 1:4) {
    ini <- In (mo, l)
    path <-  paste0(P, "lead", l, "\\Initial", ini, "\\BJP\\")
    setwd (path)
    names <- dir (path)
    AUC <- list()
    PERF_b <- list()
     for (m in 1:4) {
     setwd (paste0 (path, names[m], "\\merge_prob_trecile_1991-2020_global"))
     data_b <- read.csv (paste0("L", l, "_In", ini, "bjp.csv"))
     p1 <- prediction(data_b[,p+2], data_b[,p+5])
         PERF_b [[m]] <- performance(p1, "tpr", "fpr")
         AUC[[m]] <- performance(p1, "auc")@y.values[[1]]
       
       }
         #########################################
     AUC_R <- sapply (AUC, FUN= function(x) {round(x, 2)})
     plot( PERF_b[[1]], col = "red", lwd=2, main = paste0("lead ", l))
     plot( PERF_b[[2]], add = TRUE, col = "forestgreen",  lwd=2 )
     plot( PERF_b[[3]], add = TRUE, col = "blue", lwd=2)
     plot( PERF_b[[4]], add = TRUE, col = "brown", lwd=2)
     
      abline(0, 1)
     legend("bottomright", legend = c(paste0("AUC=", AUC_R[[1]]), paste0("AUC=",AUC_R[[2]]), paste0("AUC=",AUC_R[[3]]), paste0("AUC=",AUC_R[[4]])), col = c("red", "forestgreen", "blue", "brown"),
            lwd = 2, cex =1, lty = c(1,1,1,1))
     }

dev.off()     
}     

 


