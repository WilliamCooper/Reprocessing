## combine speed runs to be able to consider a single plot

library(Ranadu)

source('~/RStudio/Ranadu/R/TellAbout.R')
CP <- SpecificHeats ()
Project <- 'DEEPWAVE'
fileName <- sprintf ("%s%s/%srf15hPC.nc", DataDirectory (), Project, Project)

SaveFileName <- './SRrf15hr.Rdata'
ReloadData <- FALSE
if (ReloadData) {
  VarList <- c('TASX', 'PSFC', 'QCFC', 'GGALT', 'THDG')
  NCF <- nc_open (fileName)
  N <- names(NCF$var)
  nc_close(NCF)
  t <- c(which(grepl("RTH", N) & !grepl("C$", N)), which(grepl("RTF", N) & !grepl("C$", N)))
  TTvar <- N[t]
  VarList <- c(VarList, TTvar)
  
  D <- getNetCDF (fileName, VarList, F=15)
  D$X <- D$TASX^2 / (2 * CP[1])
  D$MACH <- MachNumber (D$PSFC, D$QCFC)
  D$Bin <- as.integer (D$MACH*10+0.5)
  D$LMACH <- log10(D$MACH)
  D1 <- D[setRange(D$Time, 32048, 32924), ]
  D1$PBAR <- mean (D1$PSFC, na.rm=TRUE)
  D1$RF <- D1$RF + 100
  D2 <- D[setRange(D$Time, 41449, 42329), ]
  D2$PBAR <- mean (D2$PSFC, na.rm=TRUE)
  D2$RF <- D2$RF + 101
  D3 <- D[setRange(D$Time, 50045,51058), ]
  D3$PBAR <- mean (D3$PSFC, na.rm=TRUE)
  D3$RF <- D3$RF + 102
  Data <- rbind (D1, D2)
  Data <- rbind (Data, D3)
  ## note: D1 <- Data[Data$RF == 115, ] etc
  save(Data, TTvar, VarList, file=SaveFileName)
} else {
  load (SaveFileName)
}
## difference in steady change expected to be tau*(dT/dt)
## lags in climb/descent suggest tau=2.3 s for HARCO time constant
## Try correcting: Tnew[t] <- T[t] + (T[t]-T[t-3])
Nshift <- 58
for (TT in TTvar) {
 TTm3 <- c(rep(Data[1,TT], Nshift), Data[1:(nrow(Data)-Nshift),TT])
 Data[,TT] <- 2*Data[,TT]-TTm3
}

## also adjust for height: Stratification was about 56C per 8000 m
meanZ <- mean(Data[Data$RF == 115, 'GGALT'], na.rm=TRUE)
for (TT in TTvar) {
  t <- Data$RF == 115
  # Data[t, TT] <- Data[t, TT] - (Data$GGALT[t]-meanZ) * 56/8000
}
meanZ <- mean(Data[Data$RF == 116, 'GGALT'], na.rm=TRUE)
for (TT in TTvar) {
  t <- Data$RF == 116
  # Data[t, TT] <- Data[t, TT] - (Data$GGALT[t]-meanZ) * 56/8000
}
meanZ <- mean(Data[Data$RF == 117, 'GGALT'], na.rm=TRUE)
for (TT in TTvar) {
  t <- Data$RF == 117
  # Data[t, TT] <- Data[t, TT] - (Data$GGALT[t]-meanZ) * 56/8000
}

for (TT in TTvar) {
  Data$X <- Data$TASX^2 / (2 * CP[1])
  Data$DT <- Data[, TT]
  plot (Data$X, Data$DT, type='p', pch=20, col='blue')
  title (TT)
}

D1 <- Data[Data$RF == 115, ]
D1 <- D1[setRange(D1$Time, 32300, 32800), ]
plot (D1$X, D1[, TTvar[1]], type='p', pch=20, col='blue')
lines(D1$X, D1$MACH*10-10, col='cyan')

rr <- setRange(D1$Time, 32325,32430)
points (D1$X[rr], D1[rr, TTvar[1]], col='red')
rr2 <- setRange(D1$Time, 32530,32620)
points (D1$X[rr2], D1[rr2, TTvar[1]], col='green')
t <- abs(D1$X[rr]-14) < 0.1
points (D1$X[rr][t], D1[rr, TTvar[1]][t], col='darkorange')
t2 <- abs(D1$X[rr2]-14) < 0.1
points (D1$X[rr2][t2], D1[rr2, TTvar[1]][t2], col='darkgreen')
print (sprintf("ascending:  mean of %s is %.3f", TTvar[1], mean(D1[rr, TTvar[1]][t])))
print (sprintf("descending: mean of %s is %.3f", TTvar[1], mean(D1[rr2, TTvar[1]][t2])))

f <- lm (D1[, TTvar[1]] ~ D1$X)
print (summary(f))
D1$M1 <- D1$X * log10(D1$MACH)
D1$M2 <- D1$M1 * (log10(D1$MACH))
D1$M3 <- D1$M2 * (log10(D1$MACH))
f4 <- lm (D1[, TTvar[1]] ~ D1$X + D1$M1 + D1$M2 + D1$M3)
f3 <- lm (D1[, TTvar[1]] ~ D1$X + D1$M1 + D1$M2)
print (summary(f3))
cf <- coef(f3)
rf <- cf[2]+cf[3]*D1$M1 + cf[4]*D1$M2
lines (D1$X, (1-rf)*(-100), col='black')
abline(h=100*(coef(f)[2]-1), col='black')
# cf <- coef(f)
# rf <- cf[2]+cf[3]*D1$LMACH
# A <- D1$RTHR1-rf*D1$X
# hist(rf)
# hist(A)
lines(D1$X, D1$MACH*10-10, col='cyan', lwd=2)
f <- lm (D1[, TTvar[1]] ~ D1$X+I(D1$X^2)+I(D1$X^3)+I(D1$X^4))
cf <- coef(f)
D1$ALPHA <- (cf[2]+D1$X*(2*cf[3]+D1$X*(3*cf[4]+D1$X*(4*cf[5]))))
lines(D1$X, (1-D1$ALPHA)*(-100), lwd=2, col='magenta')
plot(D1$MACH, D1$ALPHA)


for (bin in 4:6) {
  t <- D1$Bin == bin
  print(sprintf ("bin %d rf %.4f", bin, coef(f <- lm(D1$RTHR1[t] ~ D1$X[t]))[2]))
}

D2 <- Data[Data$RF == 116, ]
D2 <- D2[setRange(D2$Time, 41615, 42235), ]
plot (D2$X, D2[, TTvar[1]], type='p', pch=20, col='blue')
lines(D2$X, D2$MACH*10-10, col='cyan')

# rr <- setRange(D1$Time, 32325,32430)
# points (D1$X[rr], D1[rr, TTvar[1]], col='red')
# rr2 <- setRange(D1$Time, 32530,32620)
# points (D1$X[rr2], D1[rr2, TTvar[1]], col='green')
# t <- abs(D1$X[rr]-14) < 0.1
# points (D1$X[rr][t], D1[rr, TTvar[1]][t], col='darkorange')
# t2 <- abs(D1$X[rr2]-14) < 0.1
# points (D1$X[rr2][t2], D1[rr2, TTvar[1]][t2], col='darkgreen')
# print (sprintf("ascending:  mean of %s is %.3f", TTvar[1], mean(D1[rr, TTvar[1]][t])))
# print (sprintf("descending: mean of %s is %.3f", TTvar[1], mean(D1[rr2, TTvar[1]][t2])))

f <- lm (D2[, TTvar[1]] ~ D2$X)
print (summary(f))
D2$M1 <- D2$X * log10(D2$MACH)
D2$M2 <- D2$M1 * (log10(D2$MACH))
D2$M3 <- D2$M2 * (log10(D2$MACH))
f4 <- lm (D2[, TTvar[1]] ~ D2$X + D2$M1 + D2$M2 + D2$M3)
f3 <- lm (D2[, TTvar[1]] ~ D2$X + D2$M1 + D2$M2)
print (summary(f3))
cf <- coef(f3)
rf <- cf[2]+cf[3]*D2$M1 + cf[4]*D2$M2
lines (D2$X, (1-rf)*(-100), col='black')
abline(h=100*(coef(f)[2]-1), col='black')
# cf <- coef(f)
# rf <- cf[2]+cf[3]*D2$LMACH
# A <- D2$RTHR1-rf*D2$X
# hist(rf)
# hist(A)
lines(D2$X, D2$MACH*10-10, col='cyan', lwd=2)
for (bin in 4:7) {
  t <- D2$Bin == bin
  print(sprintf ("bin %d rf %.4f", bin, coef(f <- lm(D2$RTHR1[t] ~ D2$X[t]))[2]))
}

D3 <- Data[Data$RF == 117, ]
D3 <- D3[setRange(D3$Time, 50110, 51058), ]
plot (D3$X, D3[, TTvar[1]], type='p', pch=20, col='blue')
lines(D3$X, D3$MACH*10-10, col='cyan')

# rr <- setRange(D1$Time, 32325,32430)
# points (D1$X[rr], D1[rr, TTvar[1]], col='red')
# rr2 <- setRange(D1$Time, 32530,32620)
# points (D1$X[rr2], D1[rr2, TTvar[1]], col='green')
# t <- abs(D1$X[rr]-14) < 0.1
# points (D1$X[rr][t], D1[rr, TTvar[1]][t], col='darkorange')
# t2 <- abs(D1$X[rr2]-14) < 0.1
# points (D1$X[rr2][t2], D1[rr2, TTvar[1]][t2], col='darkgreen')
# print (sprintf("ascending:  mean of %s is %.3f", TTvar[1], mean(D1[rr, TTvar[1]][t])))
# print (sprintf("descending: mean of %s is %.3f", TTvar[1], mean(D1[rr2, TTvar[1]][t2])))

f <- lm (D3[, TTvar[1]] ~ D3$X)
print (summary(f))
D3$M1 <- D3$X * log10(D3$MACH)
D3$M2 <- D3$M1 * (log10(D3$MACH))
D3$M3 <- D3$M2 * (log10(D3$MACH))
f4 <- lm (D3[, TTvar[1]] ~ D3$X + D3$M1 + D3$M2 + D3$M3)
f3 <- lm (D3[, TTvar[1]] ~ D3$X + D3$M1 + D3$M2)
print (summary(f3))
cf <- coef(f3)
rf <- cf[2]+cf[3]*D3$M1 + cf[4]*D3$M2
lines (D3$X, (1-rf)*(-100), col='black')
abline(h=100*(coef(f)[2]-1), col='black')
# cf <- coef(f)
# rf <- cf[2]+cf[3]*D3$LMACH
# A <- D3$RTHR1-rf*D3$X
# hist(rf)
# hist(A)
lines(D3$X, D3$MACH*10-10, col='cyan', lwd=2)
for (bin in 5:8) {
  t <- D3$Bin == bin
  print(sprintf ("bin %d rf %.4f", bin, coef(f <- lm(D3$RTHR1[t] ~ D3$X[t]))[2]))
}

## Use rf11 -- but note that this is a 1-Hz file
D11 <- getNetCDF("/Data/DEEPWAVE/DEEPWAVErf11.nc", VarList)
D11$X <- D11$TASX^2/(2*CP[1])
r <- setRange(D11$Time, 103030, 103830)
D11 <- D11[r,]
plot(D11$X, D11$RTHR1, pch=20, col='blue')
f <- lm(D11$RTHR1~D11$X)
print(summary(f))

## now read data for all good speed runs:
load('SpeedRunDF.Rdata')
for (id in 26:(length(D)-1)) {
  print (sprintf ("Speed Run %d", id))
  Data <- D[[id]]
  SE <- getStartEnd(Data$Time)
  r <- setRange (Data$Time, SE[1], SE[2])
  if (id == 6) {
    r <- setRange (Data$Time, 174930, 175500)
    r <- setRange (Data$Time, 175100, 175500)
  }
  if (id == 9) {
    r <- setRange (Data$Time, 32300, 32900)
  }
  if (id == 10) {
    r <- setRange (Data$Time, 41450, 42230)
  }
  if (id == 25) {
    r <- setRange (Data$Time, 215445, 215821)
  }
  if (id == 28) {
    r <- setRange (Data$Time, 163600, 163925)
  }
  if (id == 34) {
    r <- setRange (Data$Time, 90220, 90650)
  }
  N <- names (Data)
  t <- c(which(grepl("RTH", N) & !grepl("C$", N)), which(grepl("RTF", N) & !grepl("C$", N)))
  TTvar <- N[t]
  t <- c(which(grepl("TTH", N) & !grepl("C$", N)), which(grepl("TTF", N) & !grepl("C$", N)))
  TTvar <- c(TTvar, N[t])
  Data$X <- Data$TASX^2 / (2*CP[1])
  Data$MACH <- MachNumber (Data$PSFC, Data$QCFC)
  Data$Bin <- as.integer (Data$MACH*10+0.5)
  sink('/dev/null')
  A <- getAttributes (Data)
  sink()
  Proj <- A[[which (grepl ("ProjectName", A[]))]]
  for (TT in TTvar) {
    if (grepl ("RTH", TT) || grepl ("TTH", TT)) {
      Nshift <- 3
    } else {
      Nshift <- 2
    }
    if (grepl ("ADELE", Proj)) {Nshift <- Nshift - 2}
    if (grepl ("DC3", Proj) && !grepl("TEST", Proj)) {Nshift <- Nshift - 2}
    if (grepl ("HIPPO-3", Proj)) {Nshift <- Nshift - 2}
    if (grepl ("PREDICT", Proj)) {Nshift <- Nshift - 2}
    if (grepl ("SPRITE-II", Proj)) {Nshift <- 5}
    if (grepl ("START08", Proj)) {Nshift <- Nshift - 2}
    sink ('/dev/null')
    AV <- getAttributes (Data[,TT])
    sink()    
    ia <- which (grepl ("TimeLag", AV[]) & !grepl ("Units", AV[]), arr.ind=TRUE)
    if (length(ia) > 0) {Nshift <- Nshift + as.integer (AV[[ia]])/1000}
    if (Nshift < 0) {Nshift <- 0}
    TTm3 <- c(rep(Data[1, TT], Nshift), Data[1:(nrow(Data)-Nshift), TT])
    Data[,TT] <- 2*Data[, TT]-TTm3
    plot (Data$X[r], Data[r, TT], pch=20, ylab=TT)
    title (sprintf ("PRJ %s seq %d time shift %d; Recovery factor %.4f", 
                    Proj, id, Nshift, coef(lm (Data[r, TT] ~ Data$X[r]))[2]))
    print (sprintf ("for probe %s shift %d RF %.4f", TT, Nshift, coef(lm (Data[r, TT] ~ Data$X[r]))[2]))
    for (n in 2:9) {
      DB <- Data[Data$Bin == n, ]
      if (nrow(DB) > 10) {
        fb <- lm (DB[, TT] ~ DB$X)
        print (sprintf ("bin %d rf %.4f points=%d", n, coef(fb)[2], nrow(DB)))
      }
    }
    f <- lm (Data[, TT] ~ Data$X+I(Data$X^2)+I(Data$X^3)+I(Data$X^4))
    cf <- coef(f)
    Data$ALPHA <- (cf[2]+Data$X*(2*cf[3]+Data$X*(3*cf[4]+Data$X*(4*cf[5]))))
    plot(Data$MACH, Data$ALPHA)

  }
#   cat ("Press [enter] to continue, Q to quit")
#   l <- readline()
#   if (grepl ("Q", l)) {
#     break
#   }
}
