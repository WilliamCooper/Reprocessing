### plot 22: UHSAS size distributions
RPlot22 <- function (data, ...) {
  ## needs CUHSAS_RWOOU, CPCASP_RWOOP; references fname from calling environment
  kount = 0
  plotTest <- 50
  netCDFfile = nc_open(fname)
  namesCDF <- names (netCDFfile$var)
  V <- VRPlot$PV22[1]
  if (substr(V, nchar(V), nchar(V)) == '_') {
    nm1 <- namesCDF[grepl(VRPlot$PV22[1], namesCDF)]
  } else {
    nm1 <- V
  }
  AddPCASP <- FALSE
  if (length(VRPlot$PV22) > 2) {
    AddPCASP <- TRUE
    V <- VRPlot$PV22[2]
    if (substr(V, nchar(V), nchar(V)) == '_') {
      nm2 <- namesCDF[grepl(VRPlot$PV22[2], namesCDF)]
    } else {
      nm2 <- V
    }
  }
  if (length (nm1) > 1) {nm1 <- nm1[1]}  ## multiple (e.g., for CVI): choose 1st
  Time <- ncvar_get (netCDFfile, "Time")
  TASX <- ncvar_get (netCDFfile, "TASX")
  CUHSAS <- ncvar_get (netCDFfile, nm1)
  if (AddPCASP) {CPCASP <- ncvar_get (netCDFfile, nm2)}
  time_units <- ncatt_get (netCDFfile, "Time", "units")
  tref <- sub ('seconds since ', '', time_units$value)
  Time <- as.POSIXct(as.POSIXct(tref, tz='UTC')+Time, tz='UTC')
  CellSizes <- ncatt_get (netCDFfile, nm1, "CellSizes")
  CellLimitsU <- CellSizes$value
  if (AddPCASP) {
    CellSizes <- ncatt_get (netCDFfile, nm2, "CellSizes")
    CellLimitsP <- CellSizes$value
  }
  layout(matrix(1:6, ncol = 2), widths = c(5,5), heights = c(5,5,6))
  op <- par (mar=c(2,2,1,1)+0.1,oma=c(1.1,0,0,0))
  ## yes, I know, bad-practice-reference to calling environment for StartTime
  ifelse (StartTime > 0, jstart <- getIndex(Time, StartTime), jstart <- 1)
  # print (sprintf ("start time in RPlot20 is %d and jstart is %d\n",
  #                 StartTime, jstart))

  for (j in jstart:length(Time)) {
    if (is.na(Time[j])) {next}
    if (!is.na(TASX[j]) && (TASX[j] < 90)) {next}
    if (kount >= 24) {break}
    UHSAS <- CUHSAS[, j]
    if (AddPCASP) {PCASP <- CPCASP[, j]}
    ## convert distributions to number per cm per um
    for (m in 2:length(UHSAS)) {
      UHSAS[m] <- UHSAS[m] / (CellLimitsU[m] - CellLimitsU[m-1])
    }
    if (AddPCASP) {
      for (m in 2:length(PCASP)) {
        PCASP[m] <- PCASP[m] / (CellLimitsP[m] - CellLimitsP[m-1])
      }
    }

    UHSAS[UHSAS <= 0] <- 1e-4
    if (AddPCASP) {PCASP[PCASP <= 0] <- 1e-4}
    if ((any(UHSAS > plotTest, na.rm=TRUE)) || (AddPCASP && any(PCASP > plotTest, na.rm=TRUE)) ) {
      kount <- kount + 1
      ifelse ((kount %% 3), op <- par (mar=c(2,2,1,1)+0.1),
              op <- par (mar=c(5.2,2,1,1)+0.1))
      plot (CellLimitsU, UHSAS, type='s', ylim=c(1,1.e6), 
            xlab="Diameter [um]", log="y", col='blue', lwd=2)
      if (AddPCASP) {
        points (CellLimitsP, PCASP, type='s', col='darkgreen')
      }
      title(sprintf("size distribution, Time=%s", strftime (Time[j], format="%H:%M:%S", tz='UTC')), 
           cex.main=.75)
      legend ("topright", legend=c("UHSAS", "PCASP"), col=c('blue', 'darkgreen'), 
              lwd=c(2,1), cex=0.75) 
      if (kount%%6==0)   AddFooter ()
    }
  }
  Z <- nc_close (netCDFfile)
}

