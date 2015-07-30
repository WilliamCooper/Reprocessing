### plot 6: ambient pressures
RPlot6 <- function (data) { 
  ## needs PSF, PSFC, PS_A
  op <- par (mar=c(5,4,1,2)+0.1, oma=c(1.1,0,0,0))
  layout(matrix(1:1, ncol = 1), widths = 1, heights = 5)
  plotWAC (DF <- data[, c("Time", "PSFC", "PS_A", "PSF")], 
           col=c('blue', 'skyblue', 'darkgreen'), ylab='pressure  PSy [hPa]')
  points (data$Time, (data$PSFC-data$PS_A)*50+600, type='l', col='red')
  axis (4, at=c(500,600,700), labels=c("-2", "0", "2"), col='red', col.axis='red')
  hline (500, 'red'); hline (700, 'red')
  legend ("bottomleft", 
          legend=c("(PSFC-PS_A)*50+600", "+/-2 hPa"),
          lty=c(1,2), cex=0.75,
          col=c('red', 'red'))
  title (sprintf ("mean difference PSFC-PS_A: %.1f; PSFC-PSF: %.1f", 
                  mean (data$PSFC-data$PS_A, na.rm=TRUE),
                  mean (data$PSFC-data$PSF, na.rm=TRUE)), cex.main=0.75)
  AddFooter ()
}
