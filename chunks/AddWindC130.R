
## variables needed in input data.frame for all additions:
## "ACINS", "PSXC", "ATX", "GGLAT", "GGALT", "PSTF, "QCTF",
## "PSXC", "TASX", "ATTACK", "SSLIP", "GGVEW", "GGVNS", "EWX", "GGVSPD", 
## "VEW", "VNS", "THDG", "ROLL", "PITCH", "ADIFR", "QCF", "PSFRD", "QCFR", "ADIF_GP",
## "BDIF_GP", "QC_GP", "PS_GP", "CROLL_GP", "CPITCH_GP", "CTHDG_GP" 
## Recommended variables in data.frame (to provide comparison variables also):
# VR <- c("ACINS", "ADIF_GP", "ADIFR", "AKRD", "ATTACK", "ATX", "BDIF_GP",
#   "CPITCH_GP", "CROLL_GP", "CTHDG_GP", "CVEW_GP", "CVNS_GP",  
#   "CVSPD_GP", "EWX",  "GGALT",  "GGLAT", "GGVEW", "GGVNS",  "GGVSPD",
#   "PITCH",  "PS_GP",  "PSF",  "PSTF", "PSXC", "QC_GP",  "QCF", "QCFR", "PSFRD",
#   "QCTF", "ROLL", "SSLIP",  "TASX", "THDG", "UXC",  "VEW",  "VNS",
#   "VYC",  "WDC",  "WIC",  "WSC" )
## construct function to add variables to a data.frame:
## the follow default coefficients are for WECAN:
AddWindC130 <- function (.d, .Rate=1, cff=10.351, cfs=c(5.15306, 13.16552, 0.0002517)) {
  .R <- attr(.d, 'Rate')
  if (!is.null(.R)) {.Rate <- .R}
  CutoffPeriod <- 600 * .Rate
  ## add the variable ROC:
  .d$Grav <- Gravity(.d$GGLAT, .d$GGALT)
  PSXC <- zoo::na.approx (as.vector(.d$PSXC), maxgap=1000, na.rm=FALSE)
  PSXC[is.na(PSXC)] <- mean (PSXC, na.rm=TRUE)
  DPDT <- c(0, diff(PSXC)) * .Rate
  g <- .d$Grav
  g[is.na(g)] <- 9.80
  ATX <- zoo::na.approx (as.vector(.d$ATX), maxgap=1000, na.rm=FALSE)
  ATX[is.na(ATX)] <- mean (ATX, na.rm=TRUE)
  WPPRIME <- -StandardConstant('Rd') * (273.15 + ATX) /
    (PSXC * g) * DPDT
  ACINS <- zoo::na.approx (as.vector(.d$ACINS), maxgap=1000, na.rm=FALSE)
  ACINS[is.na(ACINS)] <- 0
  WPSTAR <- cumsum(ACINS)
  DIF <- WPPRIME - WPSTAR
  DIF <- zoo::na.approx (as.vector(DIF), maxgap=1000, na.rm=FALSE)
  DIF[is.na(DIF)] <- 0
  tau <- 300 * .Rate
  DIF <- signal::filtfilt (signal::butter (3, 2/tau), DIF)
  .d$ROC <- WPSTAR + DIF
  # .d$ZROC <- .d$GGALT[1] + cumsum (.d$ROC)
  rm (DPDT, g, WPPRIME, WPSTAR, DIF, PSXC, ATX, ACINS)
  .d$QR <- .d$ADIFR / .d$QCF
  .d$QR[.d$QCF < 20] <- NA
  .d$QR[is.infinite(.d$QR)] <- NA
  .d$QRS <- zoo::na.approx (as.vector(.d$QR), maxgap=1000*.Rate, na.rm = FALSE)
  .d$QRS[is.na(.d$QRS)] <- 0
  .d$QRS <- signal::filtfilt (signal::butter (3, 2/CutoffPeriod), .d$QRS)
  .d$QRF <-  .d$QR - .d$QRS
  .d$QCFS <- zoo::na.approx (as.vector(.d$QCF), maxgap=1000*.Rate, na.rm = FALSE)
  .d$QCFS[is.na(.d$QCFS)] <- 0
  .d$QCFS <- signal::filtfilt (signal::butter (3, 2/CutoffPeriod), .d$QCFS)
  .d$AKY <- cff * .d$QRF + cfs[1] + cfs[2] * .d$QRS + cfs[3] * .d$QCFS
  DW <- .d
  DW$ATTACK <- .d$AKY
  ## for the C130 in WECAN, PSXC is dominated by noise and that noise contaminates ROC at high rate
  DW$GGVSPD <- .d$ROC  ## needed to use HR files with 1-Hz GGVSPD
  DW <- WindProcessor (DW, AC='C130', CompF=TRUE) ## uses ATTACK in preference to AKRD
  .d$WIY <- DW$WIN
  .d$WDY <- DW$WDN
  .d$WSY <- DW$WSN
  ## Use the following because the available 25-Hz files have GGVSPD only at 1 Hz.
  .d$WIY <- with(.d, WIC + TASX * (AKY - AKRD) * pi/180)
  return (.d)
}
AddWind <- function (DF, Rate=1, addAKY=TRUE, addROC=TRUE) {
  if (!is.null(attr(DF, 'Rate'))) {Rate <- attr (DF, 'Rate')}
  requiredVar <- c('PSXC', 'TASX', 'ATTACK', 'SSLIP', 'GGVEW', 'GGVNS', 'EWX',
    'VEW', 'VNS', 'THDG', 'ROLL', 'PITCH')
  if (addAKY) {requiredVar <- c(requiredVar, 'ADIFR', 'QCF', 'PSF')}
  if (addROC) {requiredVar <- c(requiredVar, 'ACINS', 'PSXC', 'ATX', 
    'GGLAT', 'GGALT')}
  requiredVar <- unique(requiredVar)
  NV <- names(DF)
  for (V in requiredVar) {
    if (!(V %in% NV)) {
      print (sprintf ('Variable %s not found; AddWind() returning unchanged data.frame', V))
      return (DF)
    }
  }
  ## make copy for function use, to add fit variables without returning them
  D <- DF
  
  ## define fit variables:
  FV <- vector('character')
  if (addROC) {
    ## ROC.R -- chunk to add ROC variable
    D$Grav <- Gravity(D$GGLAT, D$GGALT)
    DPDT <- c(0, diff(D$PSXC)) * Rate
    g <- D$Grav
    g[is.na(g)] <- 9.80
    WPPRIME <- -StandardConstant('Rd') * (273.15 + D$ATX) /
      (D$PSXC * g) * DPDT
    ACINS <- zoo::na.approx (as.vector(D$ACINS), maxgap=1000, na.rm=FALSE)
    ACINS[is.na(ACINS)] <- 0
    WPSTAR <- cumsum(ACINS)
    DIF <- WPPRIME - WPSTAR
    DIF <- zoo::na.approx (as.vector(DIF), maxgap=1000, na.rm=FALSE)
    DIF[is.na(DIF)] <- 0
    tau <- 300 * Rate
    DIF <- signal::filtfilt (signal::butter (3, 2/tau), DIF)
    DF$ROC <- WPSTAR + DIF
    # DF$ZROC <- D$GGALT[1] + cumsum (DF$ROC)
    rm (DPDT, g, WPPRIME, WPSTAR, DIF)
  }
  return (DF)
}
