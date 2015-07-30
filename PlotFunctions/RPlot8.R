### plot 8: total pressure (static + dynamic)
RPlot8 <- function (data) { 
  ## needs PSF, QCF, PS_A, QC_A
  op <- par (mar=c(5,4,1,1)+0.1, oma=c(1.1,0,0,0))
  layout(matrix(1:1, ncol = 1), widths = 1, heights = 5)
  DF <- data[, c("Time", "PSF", "PS_A")]
  DF$PSF <- data$PSF + data$QCF
  DF$PS_A <- data$PS_A + data$QC_A
  DF$Diff <- 20*(DF$PSF-DF$PS_A)+500
  colnames(DF) <- c("Time", "PtotF", "PtotAvionics", "Diff*20+500")
  plotWAC (DF, col=c('blue', 'darkgreen', 'red'), ylab='Ptot [hPa]',
           legend.position='topright')
  hline(520, 'red'); hline(480, 'red')
  title (sprintf ("mean difference: %.1f", 
                  mean (DF$PtotF-DF$PtotAvionics, na.rm=TRUE)), cex.main=0.75)
  hline (0.2); hline (-0.2)
  AddFooter ()
}
