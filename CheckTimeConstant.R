## combine speed runs to be able to consider a single plot

CP <- SpecificHeats ()
Project <- 'DEEPWAVE'
fileName <- sprintf ("%s%s/%srf15hPC.nc", DataDirectory (), Project, Project)
fileName <- sprintf ("/scr/raf/Prod_Data/DEEPWAVE/HRT/%srf15h.nc", Project)
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
  save(Data, TTvar, file=SaveFileName)
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
f <- lm (D1[, TTvar[1]] ~ D1$X + D1$M1 + D1$M2 + D1$M3)
f <- lm (D1[, TTvar[1]] ~ D1$X + I(D1$X*log10(D1$MACH)) + I(D1$X*log10(D1$MACH)^2))
      # + I(D1$X*log10(D1$MACH)^3))
print (summary(f))
cf <- coef(f)
rf <- cf[2]+cf[3]*log10(D1$MACH)+cf[4]*log10(D1$MACH)^2
lines (D1$X, (1-rf)*100, col='black')

