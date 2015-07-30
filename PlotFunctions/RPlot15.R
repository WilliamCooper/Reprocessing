### plot 15: CN, FSSP, CDP, F300, CONCP, CONC1DC_LWOO
RPlot15 <- function(data) {
  ## needs CONCN, CONCD_LWOI, CONCP_RWOOP, CONC1DC_LWOO,
  ##       CONCU, CONCU100, CONCU500, USMPFLW, USHFLW, FCNC, XICNC, PFLWC
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
  # get right locations for names
  nm1 <- names(data)[grepl("CONCU_", names(data))]
  CONCU <- data[, nm1]
  nm2 <- names(data)[grepl("CONCU100_", names(data))]
  CONCU100 <- data[, nm2]
  nm3 <- names(data)[grepl("CONCU500_", names(data))]
  CONCU500<- data[, nm3]
  nm4 <- names(data)[grepl("CONCD_", names(data))]
  CONCD <- data[, nm4]
  nm5 <- names(data)[grepl("CONC1DC_", names(data))]
  CONC1DC <- data[, nm5]
  nm6 <- names(data)[grepl("USHFLW_", names(data))]
  USHFLW <- data[, nm6]
  nm7 <- names(data)[grepl("USMPFLW_", names(data))]
  USMPFLW <- data[, nm7]
  nm8 <- names(data)[grepl("UREF_", names(data))]
  UREF <- data[, nm8]
  nm9 <- names(data)[grepl("USCAT_", names(data))]
  USCAT <- data[, nm9]
  # remove zeroes for log plot:
  # data$CONCN[!is.na(data$CONCN) & (data$CONCN <= 0)] <- NA
  CONCU[!is.na(CONCU) & (CONCU <= 0)] <- NA
  CONCU100[!is.na(CONCU100) & (CONCU100<= 0)] <- NA
  CONCU500[!is.na(CONCU500) & (CONCU500<= 0)] <- NA
  CONCD[!is.na(CONCD) & (CONCD<= 0)] <- NA
  #  data$CONCP_RWOOP[!is.na(data$CONCP_RWOOP) & (data$CONCP_RWOOP <= 0)] <- NA
  CONC1DC[!is.na(CONC1DC) & (CONC1DC <= 0)] <- NA
  plotWAC (data.frame(data$Time, CONCU, CONCU100, CONCU500), 
           logxy='y', ylim=c(1,1.e5), 
           ylab=expression (paste ("CONCy [cm"^"-3"*"]")))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data.frame(data$Time, CONCD, CONC1DC),
           logxy='y', ylim=c(0.001,1e4), ylab=expression(paste("CONCy [cm"^"-3"*"]")))
  AddFooter ()
  op <- par (mar=c(2,4,1,1)+0.1)
  USHF <- USHFLW/10
  plotWAC (data.frame(data$Time, USMPFLW, USHF),
           ylab="flows", legend.position='topright',
           ylim=c(0,2.5))
  hline (0.82, 'blue'); hline (1, 'darkgreen'); hline(0.5, 'red'); hline (1.5, 'red')
  legend ("topleft", legend=c("dashed red: limits for FCNC, XICNC, PFLWC", 
          "dashed blue-green: expected values for corresponding flows"), text.col=c('red', 'blue'), cex=0.55)
  title ("USHF is USHFLW_RWOOU/10", cex.main=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data.frame (data$Time, UREF, USCAT), 
           ylab="laser V", legend.position='topright', ylim=c(0,10))
  hline (2.05, 'blue'); hline (1.95, 'darkgreen'); hline(6, 'red'); hline (9.95, 'red')
  title ("dashed-blue: lower limit for UREF; dashed-green: upper limit for USCAT", cex.main=0.65)
  AddFooter ()
}

