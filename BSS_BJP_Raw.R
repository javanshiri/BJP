path.plot <- "E:\\Papers-Journals\\papers\\BJP\\plot\\BSS\\"
############
In <- function (m, l) {if (m-l+1 > 0 ) {i=m-l+1} else {i=m-l+13}
  return(i)}
#############
BSS <- read.csv("D:\\Project-proposal\\projects\\Lake Urmia\\output\\prc\\BSS_ALL_CORRECTED_1951-2021_global.csv")
for ( m in 1:12) {
  jpeg (paste0 (path.plot, m, "BSS2.jpg"))
  #layout(matrix(c(1, 2, 3, 4, 5, 5), ncol=2, byrow=TRUE), heights=c(7, 7, 1))
  layout(matrix(c(1, 2, 3, 4), ncol=2, byrow=TRUE), heights=c(7, 7))
      for (l in 1:4) {
    ini <- In (m, l)
    BSS_l_i <- BSS[BSS$Lead == l & BSS$Initial == ini, ] 
    mat <- as.matrix (t(BSS_l_i[,c(7,10,13,16)]))
    colnames (mat) <- c("LN", "N", "MN")
    mycols = c("cyan", "pink",  "khaki", "darkseagreen" )
    barplot(mat, beside=T, ylim=c(0, 1), legend=F, 
            main = paste("Lead", l, "- Initial", ini),
            col=c("cyan", "pink",  "khaki", "darkseagreen" ),
            cex.axis = 1.5, cex.lab=1.5, cex.names=1.5, font = 2)
            }
 # par(mai=c(0,0,0,0))
 # plot.new()
  #legend(x="center", ncol=4, 
        # legend =c( "CanCM4i", "CCSM4", "NEMO", "NASA"),
       #  border=c("black"), density=c(1000), 
        # fill=mycols, cex = 1.1)
dev.off()
}