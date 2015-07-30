### plot 16: DBAR (mean diameters) and PLWC (liquid water content)
# do 1-min smoothing; otherwise, too noisy
RPlot16 <- function (data) {
  ## needs DBARD_LWOI, DBARP_RWOOP, DBAR1DC_LWOO, DBARU_RWOOU
  ## PLWCD_LWOI, PLWC, PLWCC, PLWC1DC_LWOO
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  nm1 <- names(data)[grepl("DBARD_", names(data))]
  DBARD <- data[, nm1]
  nm1 <- names(data)[grepl("DBARD_", names(data))]
  DBARD <- data[, nm1]
  nm2 <- names(data)[grepl("DBARU_", names(data))]
  DBARU <- data[, nm2]
  nm3 <- names(data)[grepl("DBAR1DC_", names(data))]
  DBAR1DC <- data[, nm3]
  nm4 <- names(data)[grepl("PLWCD_", names(data))]
  PLWCD <- data[, nm4]
  nm5 <- names(data)[grepl("PLWC1DC_", names(data))]
  PLWC1DC <- data[, nm5]
  nm6 <- names(data)[grepl("TCNTD_", names(data))]
  TCNTD <- data[, nm6]
  nm7 <- names(data)[grepl("REJDOF_", names(data))]
  REJDOF <- data[, nm7]
  nm8 <- names(data)[grepl("AVGTRNS_", names(data))]
  AVGTRNS <- data[, nm8]
  nm9 <- names(data)[grepl("CDPLSRP_", names(data))]
  CDPLSRP <- data[, nm9]

  # DBAR:
  op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
  DBARD <- SmoothInterp(DBARD)
  plotWAC (data.frame (data$Time, DBARD), ylim=c(0,30), ylab="DBAR", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  #DF <- data[, c("Time", "DBAR3_RPO", "DBARP_RWOOP")]
  # DF <- data[, c("Time", "DBARP_RWOOP", "DBARU_RWOOU")]
  #DF$DBAR3_RPO <- SmoothInterp(data$DBAR3_RPO)
  # DF$DBARP_RWOOP <- SmoothInterp(data$DBARP_RWOOP)
  DBARU <- SmoothInterp(DBARU)
  plotWAC (data.frame (data$Time, DBARU), ylim=c(0,2), ylab="DBARP and DBARU", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  DBAR1DC <- SmoothInterp(DBAR1DC)
  plotWAC (data.frame (data$Time, DBAR1DC))
  title ("1-min filter", cex.main=0.75)
  AddFooter ()
  # PLWC:
  op <- par (mar=c(2,4,1,1)+0.1)
  PLWCD <- SmoothInterp(PLWCD)
  PLWCC <- SmoothInterp(data$PLWCC)
  plotWAC (data.frame(data$Time, PLWCD, PLWCC), ylim=c(0,1), ylab="PLWCy", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  plotWAC (data[, c("Time", "PLWC", "RICE")], ylim=c(0,25), ylab="PLWC (Watts)")
  hline (10); hline (15)
  op <- par (mar=c(5,4,1,1)+0.1)
  PLWC1DC <- SmoothInterp(PLWC1DC)
  plotWAC (data.frame(data$Time, PLWC1DC), ylim=c(0,1))
  title ("1-min filter", cex.main=0.75)
  AddFooter ()
  
  # CDP housekeeping
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  DOFACC <- TCNTD / (TCNTD + REJDOF)
  DOFACC <- SmoothInterp (DOFACC)
  plotWAC (data.frame (data$Time, DOFACC), ylab="DOF acceptance fraction")
  hline (0.2, 'red')
  AVT <- SmoothInterp (AVGTRNS)
  plotWAC (data.frame (data$Time, AVT), ylim=c(0, 2))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data.frame(data$Time, CDPLSRP), ylab="CDP laser power", ylim=c(0,4))
  AddFooter ()
}

