library(verification)
###############################################################
p=2
event <- c("LN", "N", "MN")

Model <- c("BJP-CanCM4i", "BJP-CCSM4", "BJP-NEMO", "BJP-NASA")
COL <- c("red", "forestgreen", "blue", "brown")
LTY <- c(1,2,3, 4)
for ( mo in 1:12) {
   P <- "D:\\Project-proposal\\projects\\Lake Urmia\\output\\prc\\"
  for ( l in 1:4) {
  ini <- In (mo, l)
  path <-  paste0(P, "lead", l, "\\Initial", ini, "\\BJP\\")
  setwd (path)
  names <- dir (path)
  png (paste0("D:\\Project-proposal\\projects\\Lake Urmia\\plot\\reliability_BJP_events\\", event[p],"\\rel_bjp_lead", l,  "_", mo, ".png"), res=100)
  plot(c(0,1), c(0,1), xlim=c(0,1), ylim=c(0,1), type="l", xlab = "Forecast Probability", ylab = c("Observed Relative Frequency"), main = paste0("Lead", l))
    for (m in 1:4) {
    setwd (paste0 (path, names[m], "\\merge_prob_trecile_1991-2020_global"))
    data_b <- read.csv (paste0("L", l, "_In", ini, "bjp.csv"))
    ver <- verify (data_b[,p+5], data_b[,p+2], frcst.type = "prob", obs.type = "binary")
    lines.attrib(ver, col = COL[m], lwd = 3, type = "b", lty=LTY[m])
    }
  #legend(0, 1, legend = Model, lwd=3, col = COL, lty = LTY, cex=1)
       dev.off()
  }
}
 
  
    #########################################
