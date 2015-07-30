### plot 30: chemistry (CO, O3)


RPlot30 <- function (data) { 
  ## needs COFLOW_AL, CORAW_AL, INLETP_AL, FO3_ACD
  op <- par (mfrow=c(2,1), mar=c(5,5,2,2)+0.1,oma=c(1.1,0,0,0))
  ## beware of all-missing case:
  if (!any(!is.na(data$CORAW_AL))) {return ()}

  # plot CORAW
  plotWAC (data[, c("Time", "CORAW_AL")],
         ylab="ppbv",
         lty=c(1,1), lwd=c(2), legend.position='bottomright',
         col='red')
  title("CORAW", cex.main=0.8)
  plotWAC (data[, c("Time", "FO3_ACD")], ylab="FO3 [ppbv]")
  AddFooter ()
  # plot COFLOW and INLETP
  if (any(!is.na(data$COFLOW_AL))) {
    plotWAC(data[, c("Time", "COFLOW_AL")])
  }
  plotWAC(data[, c("Time","INLETP_AL")])
  #legend('bottomright', legend=c("COFLOW", "INLETP"), pch=20, col=c('red', 'blue'))
  AddFooter ()
}

