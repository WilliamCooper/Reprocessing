### plot 5: humidity
RPlot5 <- function (data) { 
  ## needs DP_DPL, DP<DPR, DP_VXL, EW_DPR, EW_VXL, MR, ATX, 
  ## PSFC, CAVP_DPL, CAVP_DPR
  ## if EW_VXL or CAVP missing, fill in:
  if (!exists("data$EW_VXL")) {data$EW_VXL <- MurphyKoop (data$DP_VXL)}
  if (!exists("data$CAVP_DPL")) {
    data$CAVP_DPL <- data$PSFC*(1.065+0.001575*data$QCFC
                     - 1.2498*MachNumber (data$PSFC, data$QCFC))
  }
  if (!exists("data$CAVP_DPR")) {
    data$CAVP_DPR <- data$PSFC*(1.0162 +0.003024*data$QCFC
                     - 1.34521*MachNumber (data$PSFC, data$QCFC))
  }
  op <- par (mfrow=c(1,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
  plotWAC (DF <- data[, c("Time", "DP_DPL", "DP_DPR", "DP_VXL", "ATX")], 
           ylab=expression (paste ("dew point  DPy  [", degree, "C]")), 
           lty=c(1,1,2,1), lwd=c(2,1.5,1,3), legend.position='bottom', 
           col=c('blue', 'red', 'darkgreen', 'black'))
  # pulling legend out of plotWAC to increase font size
  # legend('bottomright',c("DP_DPL", "DP_DPR", "DP_VXL", "ATX"),col=c("blue","red","darkgreen","black"),text.col=c("blue","red","darkgreen","black"),lty=c(1,1,2,1),lwd=c(2,1.5,1,3))
  title(sprintf("Means DPL-DPR: %.2f; DPL-VXL: %.2f", 
                mean (data$DP_DPL-data$DP_DPR, na.rm=TRUE), 
                mean (data$DP_DPL-data$DP_VXL, na.rm=TRUE)), cex.main=0.8)
  AddFooter ()
# BBS 2/2/15 edited to match sensor colors and make VXL the dependent variable
# also, high transparency was making DPR look better than it actually was
#  plot(DF <- data[, c("DP_DPR", "DP_VXL")], pch=20, col='blue', 
  plot(DF <- data[, c("DP_VXL", "DP_DPL")], pch=20, col='blue', xlab=expression (paste ("DP_VXL  [", degree, "C]")),
       ylab=expression (paste ("dew point  DPy  [", degree, "C]")))
  lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
  lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
  DF$DP_DPL <- data$DP_DPR
#  # set high transparency (30) to avoid obscuring DPR by VXL
#  tgreen <- rgb(0,100,0,30,maxColorValue=255)
#  points (DF, pch=20, col=tgreen, cex=0.5)
#  legend('bottomright', legend=c("y=DP_DPR", "y=DP_VXL"), 
#         pch=20, col=c('blue', tgreen))
  points (DF, pch=20, col='red', cex=0.5)
  legend('bottomright', legend=c("y=DP_DPL", "y=DP_DPR"), 
         pch=20, col=c('blue', 'red'),text.col=c('blue','red'),pt.cex=c(1.0,0.5))
  title("dashed orange lines: +/-1C error bands", cex.main=0.8)
  AddFooter ()
  # DP cavity pressures and VCSEL laser intensity:
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,5))
  op <- par (mar=c(2,4,1,2.5)+0.1)
  plotWAC (data[, c("Time", "CAVP_DPR", "CAVP_DPL", "PSFC")], 
           lwd=c(2,1,1), lty=c(1,2,1), ylab='CAVP [hPa]',legend.position='bottom')
  # pulling legend out of plotWAC to increase font size
  # legend('bottomright',c("CAVP_DPR", "CAVP_DPL", "PSFC"),col=c("blue","darkgreen","red"),text.col=c("blue","darkgreen","red"),lty=c(1,2,1),lwd=c(2,1,1),cex=0.75)
  title (sprintf ("mean above PSFC: %.1f (DPL) and %.1f (DPR)", 
                  mean (data$CAVP_DPL - data$PSFC, na.rm=TRUE),
                  mean (data$CAVP_DPR - data$PSFC, na.rm=TRUE)), cex.main=0.75)
  op <- par (mar=c(5,4,1,2.5)+0.1)
  plotWAC (data[, c("Time", "LSRINT_VXL")], ylim=c(0,4000),ylab="LSRINT_VXL [count]")
  hline (1000, 'red'); hline (2700, 'red')
  # vapor pressure and mixing ratio
  AddFooter ()
  op <- par (mar=c(2,5,1,1)+0.1)
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  plotWAC (data[, c("Time", "EW_DPL", "EW_DPR", "EW_VXL")], ylab="EWy [hPa]", 
           logxy='y', ylim=c(1e-2, 100),legend.position='bottom',cex.lab=1.5,cex.axis=1.5)
  lineWAC (data$Time, MurphyKoop (data$ATX, data$PSFC), col='cyan', lty=2)
  # pulling legend out of plotWAC to increase font size
  # legend('bottomright',c("EW@ATX","EW_DPL", "EW_DPR", "EW_VXL"),col=c("cyan","blue","darkgreen","red"),text.col=c("cyan","blue","darkgreen","red"),lty=c(2,1,1,1),lwd=c(2,1,1,1))
#  title ("cyan line: equilibrium vapor pressure at ATX")
  data$MRDPL <- 0.622 * data$EW_DPL / (data$PSFC-data$EW_DPL) * 1000
  data$MRDPR <- 0.622 * data$EW_DPR / (data$PSFC-data$EW_DPR) * 1000
  plotWAC (data[, c("Time", "MR", "MRDPL", "MRDPR")], ylab="mixing ratio [g/kg]",
           logxy='y', ylim=c(0.01, 100),cex.lab=1.5,cex.axis=1.5)
  op <- par (mar=c(5,5,1,1)+0.1)
  data$RHVXL <- 100 * data$EW_VXL / MurphyKoop (data$ATX, data$PSFC)
  data$RHDPL <- 100 * data$EW_DPL / MurphyKoop (data$ATX, data$PSFC)
  data$RHDPR <- 100 * data$EW_DPR / MurphyKoop (data$ATX, data$PSFC)
  plotWAC (data[, c("Time", "RHDPL", "RHDPR", "RHVXL")], lty=c(1,1,2), lwd=1, ylab="relative humidity [%]",cex.lab=1.5,cex.axis=1.5, legend.position='topright', ylim=c(0,150))
  # pulling legend out of plotWAC to increase font size
  # legend('topright',c("RHDPL", "RHDPR", "RHVXL"),col=c("blue","darkgreen","red"),text.col=c("blue","darkgreen","red"),lty=c(1,1,2),lwd=1)
  hline (100)
  AddFooter ()
}

