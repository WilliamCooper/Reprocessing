### plot 3: plot all temperatures, one plot
RPlot3 <- function (data) { 
  ## needs ATH1, ATH2, AT_A
  par(oma=c(1.1,0,0,0))
  ylb <- expression (paste ("temperature  ATy  [", degree, "C]"))
  plotWAC (data[, c("Time", "ATHL1", "ATHL2", "ATHR1", "ATHR2", "AT_A")],
           ylab=ylb, lty=c(1,1,1,2), lwd=c(2,1.5,1,2,1),
           legend.position='bottomleft')
  #        lwd=c(2,1.5,1,2), legend.position=NA) # pulling legend out of plotWAC to increase font size
  # legend('bottomright',c("ATH2", "ATH1", "AT_A"),col=c("blue","darkgreen","red"),text.col=c("blue","darkgreen","red"),lty=c(1,1,2),lwd=c(2,1.5,1))
  title(sprintf("Means HL2-HL1: %.2f; HR1-HL1: %.2f; _A-HL1: %.2f", 
                mean (data$ATHL2-data$ATHL1, na.rm=TRUE), 
                mean (data$ATHR1-data$ATHL1, na.rm=TRUE),
                mean (data$AT_A-data$ATHL1, na.rm=TRUE)), cex.main=0.8)
  AddFooter ()
}
