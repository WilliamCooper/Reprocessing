### plot 4: plot differences, individual pairs of temperatures
RPlot4 <- function (data) { 
  ## needs ATHL1, ATHL2, ATHR1, AT_A
  op <- par (mar=c(5,5,2,4)+0.1,oma=c(1.1,0,0,0))
  labelled=F
  layout(matrix(1:4, nrow=2, ncol = 2), widths = c(5,5), heights = c(5,6))
  # ATHL1,2 section
  # protection against all-bad:
  if (any(!is.na(data$ATHL1) & !is.na(data$ATHL2))) {
    ylb <- expression (paste ("ATHL2  [", degree, "C]"))
    plot (DF <- data[, c("ATHL1", "ATHL2")], pch=20,ylab=ylb)
    lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
    lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
    par(new=T)
    plot (data$ATHL1, data$ATHL2 - data$ATHL1, type='l', col='red',axes=F,xlab='',ylab='',ylim=c(-2,2))
    axis (4,col='red',col.axis='red')
    abline(h=c(-1,1),col='red',lty=2)
    mtext(expression (paste ("ATHL2 - ATHL1  [", degree, "C]")),4,3,col='red',cex=0.8)
#    DF$ATH2 <- (data$ATH2 - data$ATH1)*5
#    points (DF, col='red', type='l', lwd=2)
#    lines (c(-70,30), c(5,5), col='red', lty=2)
#    lines (c(-70,30), c(-5,-5), col='red', lty=2)
# this info somewhat redundant with legend now, also cex=0.5 was too small to read in pngs but 1.0 too big for plot
#    legend ("bottomright", legend=c("red: y=(ATH2-ATH1)*5", 
#                                    "dashed lines: +/-1C error bands"), 
#            box.col='red', text.col='red', cex=0.5)
    fm <- lm(ATHL2~ATHL1, data=data)
    coef <- coefficients (fm)
    if (coef[1] < 0.) {
      t <- sprintf ("ATHL2=%.3f(ATHL1)%.3f\nmean diff ATHL2-ATHL1=%.2f +/- %.2f", 
                    coef[2], coef[1], mean (data$ATHL2-data$ATHL1, na.rm=TRUE),
                    sd(data$ATHL2-data$ATHL1, na.rm=TRUE))
    } else {
      t <- sprintf ("ATHL2=%.3f(ATHL1)+%.3f\nmean diff ATHL2-ATHL1=%.2f +/-%.2f", 
                    coef[2], coef[1], mean (data$ATHL2-data$ATHL1, na.rm=TRUE),
                    sd(data$ATHL2-data$ATHL1, na.rm=TRUE))
    }
    title(t, cex.main=0.75)
    AddFooter ()
  }
  
  # ATHR section:
  if (any(!is.na(data$ATHR1) & !is.na(data$ATHL2))) {
    ylb <- expression (paste ("ATHR1  [", degree, "C]"))
    plot (DF <- data[, c("ATHL2", "AT_A")], pch=20,ylab=ylb)
    lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
    lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
    par(new=T)
    plot (data$ATHL2, data$ATHR1 - data$ATHL2, type='l', col='red',axes=F,xlab='',ylab='',ylim=c(-2,2))
    axis (4,col='red',col.axis='red')
    abline(h=c(-1,1),col='red',lty=2)
    mtext(expression (paste ("ATHR1 - ATHL2  [", degree, "C]")),4,3,col='red',cex=0.8)
    #    DF$AT_A <- (data$ATHL2 - data$AT_A)*5
    #    points (DF, col='red', type='l', lwd=2)
    #    lines (c(-70,30), c(5,5), col='red', lty=2)
    ##    lines (c(-70,30), c(-5,-5), col='red', lty=2)
    # this info somewhat redundant with legend now, also cex=0.5 was too small to read in pngs but 1.0 too big for plot
    #    legend ("topleft", legend=c("red: y=(ATHL2-AT_A)*5", 
    #                                "dashed lines: +/-1C error bands"), 
    #            box.col='red', text.col='red', cex=0.5)
    fm <- lm(ATHR1~ATHL2, data=data)
    coef <- coefficients (fm)
    if (coef[1] < 0.) {
      t <- sprintf ("ATHR1=%.3f(ATHL2)%.3f\n mean diff ATHR1-ATHL2=%.2f +/-%.2f", 
                    coef[2], coef[1], mean(data$ATHR1-data$ATHL2, na.rm=TRUE),
                    sd(data$ATHR1-data$ATHL2, na.rm=TRUE))
    } else {
      t <- sprintf ("ATHR1=%.3f(ATHL2)+%.3f\n mean diff ATHR1-ATHL2=%.2f +/-%.2f", 
                    coef[2], coef[1], mean(data$ATHR1-data$ATHL2, na.rm=TRUE), 
                    sd(data$ATHR1-data$ATHL2, na.rm=TRUE))
    }
    title(t, cex.main=0.8)
    if(!labelled) AddFooter (); labelled=T
  }
  
  # AT_A section:
  if (any(!is.na(data$AT_A) & !is.na(data$ATHL2))) {
    ylb <- expression (paste ("AT_A  [", degree, "C]"))
    plot (DF <- data[, c("ATHL2", "AT_A")], pch=20,ylab=ylb)
    lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
    lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
    par(new=T)
    plot (data$ATHL2, data$AT_A - data$ATHL2, type='l', col='red',axes=F,xlab='',ylab='',ylim=c(-2,2))
    axis (4,col='red',col.axis='red')
    abline(h=c(-1,1),col='red',lty=2)
    mtext(expression (paste ("AT_A - ATHL2  [", degree, "C]")),4,3,col='red',cex=0.8)
#    DF$AT_A <- (data$ATHL2 - data$AT_A)*5
#    points (DF, col='red', type='l', lwd=2)
#    lines (c(-70,30), c(5,5), col='red', lty=2)
##    lines (c(-70,30), c(-5,-5), col='red', lty=2)
# this info somewhat redundant with legend now, also cex=0.5 was too small to read in pngs but 1.0 too big for plot
#    legend ("topleft", legend=c("red: y=(ATHL2-AT_A)*5", 
#                                "dashed lines: +/-1C error bands"), 
#            box.col='red', text.col='red', cex=0.5)
    fm <- lm(AT_A~ATHL2, data=data)
    coef <- coefficients (fm)
    if (coef[1] < 0.) {
      t <- sprintf ("AT_A=%.3f(ATHL2)%.3f\n mean diff AT_A-ATHL2=%.2f +/-%.2f", 
                    coef[2], coef[1], mean(data$AT_A-data$ATHL2, na.rm=TRUE),
                    sd(data$AT_A-data$ATHL2, na.rm=TRUE))
    } else {
      t <- sprintf ("AT_A=%.3f(ATHL2)+%.3f\n mean diff AT_A-ATHL2=%.2f +/-%.2f", 
                    coef[2], coef[1], mean(data$AT_A-data$ATHL2, na.rm=TRUE), 
                    sd(data$AT_A-data$ATHL2, na.rm=TRUE))
    }
    title(t, cex.main=0.8)
    if(!labelled) AddFooter (); labelled=T
  }
}

