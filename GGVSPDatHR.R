## ----initialization, include=FALSE---------------------------------------

library(Ranadu)
library(knitr)
library(reshape2)
library(grid)
opts_chunk$set(echo=FALSE, include=FALSE, fig.lp="fig:")
opts_chunk$set(fig.width=6, fig.height=4.7, fig.pos="center", digits=4)
thisFileName <- "GGVSPDatHR"
require(Ranadu, quietly = TRUE, warn.conflicts=FALSE)
VarList <- c('PSXC', 'ATX', 'GGALT', 'LATC', 'GGVSPD', 'ACINS', 'WIC', 'SSLIP')
VarList <- c(VarList, 'PITCH', 'ATTACK', 'GGVEW', 'GGVNS', 'VEW', 'VNS', 'TASX', 'THDG', 'ROLL')


## ----get-data------------------------------------------------------------

Project <- 'WINTER'
Flight <- 10
Rate <- 25
Start <- 0
End <- 0
# source('~/RStudio/Ranadu/R/getNetCDF.R')  ## restore -- not necessary
## HR is produced with sample-rate GGVSPD (10 Hz); H with 25 Hz GGVSPD
Data <- getNetCDF(sprintf('%s%s/%srf%02dHR.nc', DataDirectory (), Project, Project, Flight), VarList)
Data$WIC <- zoo::na.approx (as.vector(Data$WIC), maxgap=1000, na.rm=FALSE)
Data$WICorig <- Data$WIC  ## save to use later
DNEW <- WindProcessor (Data)
Data$WICC <- DNEW$WIN  ## recalculated WIC using GGVSPD as interp. in getNetCDF
D <- getNetCDF(sprintf('%s%s/%srf%02dH.nc', DataDirectory (), Project, Project, Flight), VarList)
D$WIC <- zoo::na.approx (as.vector(D$WIC), maxgap=1000, na.rm=FALSE)
Data$GGVSPD25 <- D$GGVSPD  ## the 25-Hz version from nimbus
Data$WIC25 <- D$WIC        ## " " "
# source('~/RStudio/Ranadu/R/getNetCDFv2.R')  ## uses 15-pt SG filter instead of 21-pt
# D <- getNetCDF(sprintf('%s%s/%srf%02dHR.nc', DataDirectory (), Project, Project, Flight), VarList)
# Data$GGVSPDa <- D$GGVSPD
# Data$WICA <- Data$WIC + Data$GGVSPDa - Data$GGVSPD25
Data$WIC <- Data$WIC + Data$GGVSPD - Data$GGVSPD25
# unlink('/Data/WINTER/WINTERrf10z.nc')
# makeNetCDF(Data, '/Data/WINTER/WINTERrf10z.nc')
# source('~/RStudio/Ranadu/R/getNetCDF.R')  ## restore 


## ----pressure-derivative, include=FALSE, fig.cap='Comparison of GPS-provided rate of climb (GGVSPD) and that calculated from the complementary filter.'----

## update to GGVSPD instead
Data$WPSTAR <- cumsum(Data$ACINS / Rate)
Data$WPSTAR <- zoo::na.approx (as.vector(Data$WPSTAR), maxgap=1000, na.rm=FALSE)
Data$WPSTAR <- ShiftInTime(Data$WPSTAR, .shift=-20)
Data$DIF <- Data$GGVSPD - Data$WPSTAR
Data$DIF <- zoo::na.approx (as.vector(Data$DIF), maxgap=1000, na.rm=FALSE)
tau <- 25  ## 10 s
Data$DIFW <- signal::filter (signal::butter (3, 2/tau), Data$DIF)
Data$ROCG <- Data$WPSTAR + Data$DIFW
tau <- 50
Data$DIFW <- signal::filter (signal::butter (3, 2/tau), Data$DIF)
Data$ROCG50 <- Data$WPSTAR + Data$DIFW
tau <- 10
Data$DIFW <- signal::filter (signal::butter (3, 2/tau), Data$DIF)
Data$ROCG10 <- Data$WPSTAR + Data$DIFW
# with(Data, plotWAC(data.frame (Time, GGVSPD, ROCG), ylab='rate of climb'))
SE <- getStartEnd (Data)
i1 <- 7500; i2 <- nrow(Data) - 7500
R <- i1:i2
meanw <- with (Data[R,], mean (ROCG-GGVSPD, na.rm=TRUE))
sdw   <- with (Data[R,], sd   (ROCG-GGVSPD, na.rm=TRUE))
Data$WIR <- Data$WICorig + Data$ROCG - Data$GGVSPD25
Data$WIR50 <- Data$WICorig + Data$ROCG50 - Data$GGVSPD25
Data$WIR10 <- Data$WICorig + Data$ROCG10 - Data$GGVSPD25
DTemp <- Data
DTemp$GGVSPD <- Data$ROCG
DNEW <- WindProcessor(DTemp)
Data$WIRC <- DNEW$WIN
# unlink('/Data/WINTER/WINTERrf10z.nc')
# makeNetCDF(Data, '/Data/WINTER/WINTERrf10z.nc')


## ----baro-loop, include=TRUE, fig.cap=''---------------------------------

# C <- c(0.15, 0.0075, 0.000125)
zeta <- 1/10  ## approx 10-s time constant, providing rapid feedback
C <- c(3*zeta, 3*zeta^2, zeta^3)
# wp3 <- Data$GGVSPD[1];hi3 <- Data$GGALT[1];hxx <- 0; hx <- 0; wp3last <- wp3
# Data$WPG2 <- cumsum (Data$ACINS) / Rate
# lstep <- function (data) {
#   dwp <- (data$ACINS -C[2] * hx - C[3] * hxx) / Rate
#   hi3 <<- hi3 + (wp3 + dwp - C[1] * hx) / Rate
#   hx <<- hi3 - data$GGALT
#   hxx <<- hxx + hx / Rate
#   wp3 <<- wp3 + dwp
#   return (wp3 + dwp / 2)
# }
# DT <- with(Data, data.frame(ACINS, GGALT))
# for (i in 1:nrow(Data)) {
#   Data$WPG2[i] <- lstep(DT[i,])
# }
wp3 <- Data$GGVSPD[1];hi3 <- Data$GGALT[1];hxx <- 0; hx <- 0; wp3last <- wp3
Data$WPG <- vector('numeric', length=nrow(Data))
for (i in 1:nrow(Data)) {
  wp3 <- wp3 + (Data$ACINS[i] - C[2] * hx - C[3] * hxx) / Rate
  hi3 <- hi3 + (wp3 - C[1] * hx) / Rate
  hx <- hi3 - Data$GGALT[i]
  hxx <- hxx + hx / Rate
  Data$WPG[i] <- (wp3 + wp3last) / 2
  wp3last <- wp3
}
Data$WIL <- Data$WICorig + Data$WPG - Data$GGVSPD25
fnew <- sprintf ('%s%s/WAC/%srf%02dz.nc', DataDirectory(), Project, Project, Flight)
unlink(fnew)
makeNetCDF(Data, fnew)


