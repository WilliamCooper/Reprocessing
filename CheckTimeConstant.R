## combine speed runs to be able to consider a single plot

CP <- SpecificHeats ()
Project <- 'DEEPWAVE'
fileName <- sprintf ("%s%s/%srf15hPC.nc", DataDirectory (), Project, Project)
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
## difference in steady change expected to be tau*(dT/dt)
## lags in climb/descent suggest tau=2 s
## Try correcting: Tnew[t] <- T[t] + (T[t]-T[t-3])
Nshift <- 50
for (TT in TTvar) {
  TTm3 <- c(rep(D[1,TT],Nshift), D[1:(nrow(D)-Nshift),TT])
  D[,TT] <- 2*D[,TT]-TTm3
}
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

for (TT in TTvar) {
  Data$X <- Data$TASX^2 / (2 * CP[1])
  Data$DT <- Data[, TT]
  plot (Data$X, Data$DT, type='p', pch=20, col='blue')
  title (TT)
}
plot (D1$X, D1[, 'RTHR1'], type='p', pch=20, col='blue')
rr <- setRange(D1$Time, 32325,32430)
points (D1$X[rr], D1[rr, 'RTHR1'], col='red')
rr2 <- setRange(D1$Time, 32530,32620)
points (D1$X[rr2], D1[rr2, 'RTHR1'], col='green')
t <- abs(D1$X[rr]-14) < 0.1
points (D1$X[rr][t], D1$RTHR1[rr][t], col='darkorange')
t2 <- abs(D1$X[rr2]-14) < 0.1
points (D1$X[rr2][t2], D1$RTHR1[rr2][t2], col='darkgreen')
mean(D1$RTHR1[rr][t])
mean(D1$RTHR1[rr2][t2])

f <- lm (D1$RTHR2 ~ D1$X)
summary(f)
f <- lm (D1$RTHR2 ~ D1$X + I(D1$X*log10(D1$MACH)))
     # + I(D1$X*log10(D1$MACH)^2) + I(D1$X*log10(D1$MACH)^3))
summary(f)
