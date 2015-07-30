
### plot 7: dynamic pressure; also TAS and MACH
RPlot7 <- function (data) { 
  ## needs QCF, QCR, QCFC, QCRC, QC_A, TASF, TASR, TAS_A,
  ## MACHF, MACHR, MACH_A
  op <- par (mar=c(2,4,1,2)+0.1, oma=c(1.1,0,0,0))
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  plotWAC (data[, c("Time", "QCF", "QCR")], 
           col=c('blue', 'darkgreen'), ylab='QCy [hPa]', 
           legend.position='top')
  points (data$Time, (data$QCF-data$QCR)*10+120, type='l', col='red')
  axis (4, at=c(100,120,140), labels=c("-2", "0", "2"), col='red', col.axis='red')
  hline (100, col='red'); hline (140, col='red')
  legend("bottomleft", legend=c("red: (QCF-QCR)*10+120", 
                                "dashed red: +/- 2 hPa [diff]"), cex=0.75)
  op <- par (mar=c(5,4,1,2)+0.1)
  plotWAC (data[, c("Time", "QCFC", "QCRC", "QC_A")], 
           col=c('blue', 'darkgreen', 'cyan'), ylab=' corrected QCyC [hPa]',
           legend.position='top')
  points (data$Time, (data$QCFC-data$QC_A)*10+120, type='l', col='red')
  axis (4, at=c(100,120,140), labels=c("-2", "0", "2"), col='red', col.axis='red')
  hline (140, col='red'); hline (100, col='red')
  legend("bottomleft", c("red: (QCFC-QC_A)*10+120", 
                         "dashed red: +/- 2 hPa [diff]"), cex=0.75)
  title (sprintf ("mean difference QCFC-QC_A=%.1f",
                  mean (data$QCFC-data$QC_A, na.rm=TRUE)), cex.main=0.75)
  AddFooter ()
  # add TAS and MACH plots:
  op <- par (mar=c(2,4,1,1)+0.1)
  plotWAC (data[, c("Time", "TASF", "TASR", "TAS_A")], 
           col=c('blue', 'darkorange', 'darkgreen', 'red'), ylab='TASy [m/s]', 
           legend.position='top')
  title (sprintf ("diff vs TASF: %.1f (TASR), %.1f (TAS_A)",
                  mean (data$TASR-data$TASF, na.rm=TRUE), 
                  mean (data$TAS_A-data$TASF, na.rm=TRUE)), cex.main=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "MACHF", "MACHR", "MACH_A")], 
           col=c('blue', 'darkorange', 'darkgreen'), ylab='MACHy', 
           legend.position='top')
  AddFooter ()
}
