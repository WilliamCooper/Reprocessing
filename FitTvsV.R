## HIPPO-5 calibrations

## need function to calculate temperature from resistance, CVD equation:
## pfit must contain rz, alpha, beta, delta
pfit <- data.frame(rz = 50)
pfit$alpha <- 0.003925
pfit$delta <- 1.45

CVDtemperature <- function (pfit, R) {
  Tr <- ((R/pfit$rz-1.)/(pfit$alpha*(1.+pfit$delta/100.)))
  beta <- vector('numeric', length(R))
  t <- (Tr < 0)
  beta[t] <- 0.1
  Tr2 <- Tr
  for (j in 1:8) {
    Tr2 = Tr+(pfit$delta*Tr2**2/1.e4  - beta*Tr2^3/1.e6
              + beta*Tr2^4/1.e8) / (1.+pfit$delta/100.)
    # print (sprintf( "j=%d, Tr2 <- %.7f", j, Tr2))
  }
  return(Tr2)
}

## function to get CVD coefficients, each sensor:
CVDcoef <- function (SN) {
  # now, instead, get coefficients dependent on probe SN
  if (SN == 'A50738A') {
    alphac = 0.003935
    RZC = 49.9960
  }  
  if (SN == 'A50738B') {
    alphac = 0.003936
    RZC = 50.0146
  }
  if (SN == '2984') {
    alphac = 0.003919
    RZC = 50.5493
  }
  if (SN == '3245') {
    alphac = 0.003929
    RZC = 50.0477
  }
  if (SN == '3109') {      # DLR cal, outlier, use with caution
    alphac = 0.003745
    RZC = 49.584
  }
  if (SN == '812452A') {
    alphac = 0.003913
    RZC = 50.0333
  }
  if (SN == '812452B') {
    alphac = 0.003915
    RZC = 50.0227
  }
  if (SN == '630393A') {
    alphac = 0.003914
    RZC = 50.0081
  }
  if (SN == '630393B') {
    alphac = 0.003911
    RZC = 50.0053
  }
  if (SN == '708094A') {
    alphac = 0.003916
    RZC = 49.9873
  }
  if (SN == '708094B') {
    alphac = 0.003916
    RZC = 49.9871
  }
  print (sprintf("coefficients used: %.6f, %.3f for SN %s", alphac, RZC, SN))
  return(c(alphac, RZC))
}

a <- CVDcoef("630393A")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45



HIPPO5a <- read.csv("~/Spyder/TCal/Calibrations/HIPPO5a.csv", header=FALSE, sep=';')
names(HIPPO5a) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
HIPPO5a$T <- CVDtemperature(pfit, HIPPO5a$R)
HIPPO5b <- read.csv("~/Spyder/TCal/Calibrations/HIPPO5b.csv", header=FALSE, sep=';')
names(HIPPO5b) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("630393B")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
HIPPO5b$T <- CVDtemperature(pfit, HIPPO5b$R)
HIPPO5c <- read.csv("~/Spyder/TCal/Calibrations/HIPPO5c.csv", header=FALSE, sep=';')
names(HIPPO5c) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("A50738A")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
HIPPO5c$T <- CVDtemperature(pfit, HIPPO5c$R)
HIPPO5d <- read.csv("~/Spyder/TCal/Calibrations/HIPPO5d.csv", header=FALSE, sep=';')
names(HIPPO5d) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("A50738B")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
HIPPO5d$T <- CVDtemperature(pfit, HIPPO5d$R)
HIPPO5e <- read.csv("~/Spyder/TCal/Calibrations/HIPPO5e.csv", header=FALSE, sep=';')
names(HIPPO5e) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("A50738B")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
HIPPO5e$T <- CVDtemperature(pfit, HIPPO5e$R)
HIPPO5f <- read.csv("~/Spyder/TCal/Calibrations/HIPPO5f.csv", header=FALSE, sep=';')
names(HIPPO5f) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("630393A")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
HIPPO5f$T <- CVDtemperature(pfit, HIPPO5f$R)
HIPPO5g <- read.csv("~/Spyder/TCal/Calibrations/HIPPO5g.csv", header=FALSE, sep=';')
names(HIPPO5g) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("630393A")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
HIPPO5g$T <- CVDtemperature(pfit, HIPPO5g$R)
HIPPO5h <- read.csv("~/Spyder/TCal/Calibrations/HIPPO5h.csv", header=FALSE, sep=';')
names(HIPPO5h) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("630393B")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
HIPPO5h$T <- CVDtemperature(pfit, HIPPO5h$R)
HIPPO5i <- read.csv("~/Spyder/TCal/Calibrations/HIPPO5i.csv", header=FALSE, sep=';')
names(HIPPO5i) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("630393B")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
HIPPO5i$T <- CVDtemperature(pfit, HIPPO5i$R)
HIPPO5j <- read.csv("~/Spyder/TCal/Calibrations/HIPPO5j.csv", header=FALSE, sep=';')
names(HIPPO5j) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("A50738A")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
HIPPO5j$T <- CVDtemperature(pfit, HIPPO5j$R)
ADELEa <- read.csv("~/Spyder/TCal/Calibrations/ADELEa.csv", header=FALSE, sep=';')
names(ADELEa) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("3109")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
ADELEa$T <- CVDtemperature(pfit, ADELEa$R)
ADELEb <- read.csv("~/Spyder/TCal/Calibrations/ADELEb.csv", header=FALSE, sep=';')
names(ADELEb) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("630393A")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
ADELEb$T <- CVDtemperature(pfit, ADELEb$R)
ADELEc <- read.csv("~/Spyder/TCal/Calibrations/ADELEc.csv", header=FALSE, sep=';')
names(ADELEc) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("630393B")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
ADELEc$T <- CVDtemperature(pfit, ADELEc$R)
ADELEd <- read.csv("~/Spyder/TCal/Calibrations/ADELEd.csv", header=FALSE, sep=';')
names(ADELEd) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("3109")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
ADELEd$T <- CVDtemperature(pfit, ADELEd$R)
ADELEe <- read.csv("~/Spyder/TCal/Calibrations/ADELEe.csv", header=FALSE, sep=';')
names(ADELEe) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("630393B")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
ADELEe$T <- CVDtemperature(pfit, ADELEe$R)
ADELEf <- read.csv("~/Spyder/TCal/Calibrations/ADELEf.csv", header=FALSE, sep=';')
names(ADELEf) <- c('T', 'R', 'V', 'V4', 'V5', 'V6', 'V7')
a <- CVDcoef("630393A")
pfit <- data.frame(rz = a[2])
pfit$alpha <- a[1]
pfit$delta <- 1.45
ADELEf$T <- CVDtemperature(pfit, ADELEf$R)

## Rosemount element A:
plot(HIPPO5c$V, HIPPO5c$T, type='b', col='blue', xlim=c(1,5), ylim=c(-70,30), xlab='Voltage', ylab='Temperature')
lines(HIPPO5j$V, HIPPO5j$T, col='red')
points(HIPPO5j$V, HIPPO5j$T, pch=20, col='red')
H <- rbind(HIPPO5c, HIPPO5j)
f <- lm (H$T~H$V+I(H$V^2))
cf <- coef(f)
xp <- seq(1,5,length.out=200)
yp <- cf[1] +cf[2] * xp + cf[3] * xp^2
lines(xp, yp, col='darkgreen')
title(sprintf("HIPPO5 Rosemount A50738A (was TTFH1), coef=%.4f %.4f %.4f", cf[1], cf[2], cf[3]))
## Rosemount element B: (files HIPPO5d.csv and HIPPO5e.csv)
plot(HIPPO5d$V, HIPPO5d$T, type='b', col='blue', xlim=c(1,5), ylim=c(-70,30), xlab='Voltage', ylab='Temperature')
lines(HIPPO5e$V, HIPPO5e$T, col='red')
points(HIPPO5e$V, HIPPO5e$T, pch=20, col='red')
H <- rbind(HIPPO5d, HIPPO5e)
f <- lm (H$T~H$V+I(H$V^2))
cf <- coef(f)
xp <- seq(1,5,length.out=200)
yp <- cf[1] +cf[2] * xp + cf[3] * xp^2
lines(xp, yp, col='darkgreen')
title(sprintf("HIPPO5 Rosemount A50738B (was TTFH2) coef %.4f %.4f %.4f", cf[1], cf[2], cf[3]))
## HARCO sensor 630393A (files HIPPO5a.csv and HIPPO5f.csv, HIPPO5g.csv)
plot(HIPPO5a$V, HIPPO5a$T, type='b', col='blue', xlim=c(1,5), ylim=c(-70,30), xlab='Voltage', ylab='Temperature')
lines(HIPPO5f$V, HIPPO5f$T, col='red')
points(HIPPO5f$V, HIPPO5f$T, pch=20, col='red')
lines(HIPPO5g$V, HIPPO5g$T, col='darkgreen')
points(HIPPO5g$V, HIPPO5g$T, pch=20, col='darkgreen')
H <- rbind(HIPPO5a, HIPPO5f)
H <- rbind(H, HIPPO5g)
f <- lm (H$T~H$V+I(H$V^2))
cf <- coef(f)
xp <- seq(1,5,length.out=200)
yp <- cf[1] +cf[2] * xp + cf[3] * xp^2
lines(xp, yp, col='darkgreen')
title(sprintf("HIPPO5 HARCO 630393A (was TTHR1), coef=%.4f %.4f %.4f", cf[1], cf[2], cf[3]))

## HARCO sensor 630393B (files HIPPO5b.csv and HIPPO5h.csv, HIPPO5i.csv)
plot(HIPPO5b$V, HIPPO5b$T, type='b', col='blue', xlim=c(1,5), ylim=c(-70,30), xlab='Voltage', ylab='Temperature')
lines(HIPPO5h$V, HIPPO5h$T, col='red')
points(HIPPO5h$V, HIPPO5h$T, pch=20, col='red')
lines(HIPPO5i$V, HIPPO5i$T, col='darkgreen')
points(HIPPO5i$V, HIPPO5i$T, pch=20, col='darkgreen')
H <- rbind(HIPPO5b, HIPPO5h)
H <- rbind(H, HIPPO5i)
f <- lm (H$T~H$V+I(H$V^2))
cf <- coef(f)
xp <- seq(1,5,length.out=200)
yp <- cf[1] +cf[2] * xp + cf[3] * xp^2
lines(xp, yp, col='darkgreen')
title(sprintf("HIPPO5 HARCO 630393B (was TTHR2), coef %.4f %.4f %.4f", cf[1], cf[2], cf[3]))

## ADELE sensor 3109 TTFR (ADELEa,csv and ADELEd.csv)
plot(ADELEa$V, ADELEa$T, type='b', col='blue', xlim=c(1,5), ylim=c(-70,30), xlab='Voltage', ylab='Temperature')
lines(ADELEd$V, ADELEd$T, col='red')
points(ADELEd$V, ADELEd$T, pch=20, col='red')
H <- rbind(ADELEa, ADELEd)
f <- lm (H$T~H$V+I(H$V^2))
cf <- coef(f)
xp <- seq(1,5,length.out=200)
yp <- cf[1] +cf[2] * xp + cf[3] * xp^2
lines(xp, yp, col='darkgreen')
title(sprintf("ADELE Rosemount 3109 (was TTFR), coef %.4f %.4f %.4f", cf[1], cf[2], cf[3]))

## ADELE sensor 630393A TTHR1 (ADELEb,csv and ADELEf.csv)
plot(ADELEb$V, ADELEb$T, type='b', col='blue', xlim=c(1,5), ylim=c(-70,30), xlab='Voltage', ylab='Temperature')
lines(ADELEf$V, ADELEf$T, col='red')
points(ADELEf$V, ADELEf$T, pch=20, col='red')
H <- rbind(ADELEb, ADELEf)
f <- lm (H$T~H$V+I(H$V^2))
cf <- coef(f)
xp <- seq(1,5,length.out=200)
yp <- cf[1] +cf[2] * xp + cf[3] * xp^2
lines(xp, yp, col='darkgreen')
title(sprintf("ADELE HARCO 690393A (was TTHR1), coef %.4f %.4f %.4f", cf[1], cf[2], cf[3]))

## ADELE sensor 630393B TTHR2 (ADELEc,csv and ADELEe.csv)
plot(ADELEc$V, ADELEc$T, type='b', col='blue', xlim=c(1,5), ylim=c(-70,30), xlab='Voltage', ylab='Temperature')
lines(ADELEe$V, ADELEe$T, col='red')
points(ADELEe$V, ADELEe$T, pch=20, col='red')
H <- rbind(ADELEc, ADELEe)
f <- lm (H$T~H$V+I(H$V^2))
cf <- coef(f)
xp <- seq(1,5,length.out=200)
yp <- cf[1] +cf[2] * xp + cf[3] * xp^2
lines(xp, yp, col='darkgreen')
title(sprintf("ADELE 630393B (was TTHR2), coef %.4f %.4f %.4f", cf[1], cf[2], cf[3]))
