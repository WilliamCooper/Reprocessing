### plot 13: IRU continued, ACINS, VSPD
RPlot13 <- function (data) {
  ## needs ACINS, ACINS_IRS2, FSPD, FSPD_A, GGVSPD, GGALT, ALT_A, ALT_A2
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
  DF <- data[, c("Time", "ACINS", "ACINS_IRS2")]
  plotWAC (DF, ylab="ACINS")
  title (sprintf ("mean vertical acceleration: %.3f", mean (data$ACINS, na.rm=TRUE)))
  plotWAC (data[, c("Time", "VSPD", "VSPD_G")], legend.position='topright')
  title (sprintf ("mean vertical velocity: %.3f (IRS) and %.3f (GPS)",
                  mean (data$VSPD, na.rm=TRUE), mean (data$VSPD_G, na.rm=TRUE)))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "GGALT", "ALT_G")],
           legend.position = "top")
  AddFooter ()
}

