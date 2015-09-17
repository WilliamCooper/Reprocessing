# check speed run
require(Ranadu)

Cals <- data.frame(Project='DC3', TotalT='TTHR1', c1=-82.117, c2=22.593, c3=0.3054)
Cals <- rbind(Cals, data.frame(Project='DC3', TotalT='TTHR2', c1=-82.217, c2=22.802, c3=0.2819))
Cals <- rbind(Cals, data.frame(Project='DC3', TotalT='TTRL', c1=-84.004, c2=22.314, c3=0.3027))
Cals <- rbind (Cals, data.frame(Project='TREX', TotalT='TTRL', c1=-83.304, c2=23.222, c3=0.19054))
Cals <- rbind (Cals, data.frame(Project='TREX', TotalT='TTHR1', c1=-83.326, c2=23.120, c3=0.24174))
Cals <- rbind (Cals, data.frame(Project='TREX', TotalT='TTHR2', c1=-82.265, c2=22.636, c3=0.32405))
Cals <- rbind (Cals, data.frame (Project='PACDEX', TotalT='TTFR', c1=-82.130, c2=22.422, c3=0.30632))
Cals <- rbind (Cals, data.frame (Project='PACDEX', TotalT='TTHR1', c1=-82.959, c2=22.892, c3=0.26392))
Cals <- rbind (Cals, data.frame (Project='PACDEX', TotalT='TTHR2', c1=-82.328, c2=22.675, c3=0.29493))
Cals <- rbind (Cals, data.frame (Project='HEFT08', TotalT='TTHR1', c1=-82.314, c2=22.430, c3=0.30844))
Cals <- rbind (Cals, data.frame (Project='HEFT08', TotalT='TTHR2', c1=-82.207, c2=22.494, c3=0.30833))
Cals <- rbind (Cals, data.frame (Project='START08', TotalT='TTFR', c1=-82.108, c2=22.410, c3=0.30155))
Cals <- rbind (Cals, data.frame (Project='START08', TotalT='TTHR1', c1=-83.704, c2=22.428, c3=0.35397))
Cals <- rbind (Cals, data.frame (Project='START08', TotalT='TTHR2', c1=-82.644, c2=21.961, c3=0.43736))
Cals <- rbind (Cals, data.frame (Project='HIPPO-1', TotalT='TTFR', c1=-82.415, c2=22.482, c3=0.29216))
Cals <- rbind (Cals, data.frame (Project='HIPPO-1', TotalT='TTHR1', c1=-82.441, c2=22.550, c3=0.29465))
Cals <- rbind (Cals, data.frame (Project='HIPPO-1', TotalT='TTHR2', c1=-82.066, c2=22.467, c3=0.31373))
Cals <- rbind (Cals, data.frame (Project='HIPPO-2', TotalT='TTHL1', c1=-82.378, c2=22.760, c3=0.30230))
Cals <- rbind (Cals, data.frame (Project='HIPPO-2', TotalT='TTHL2', c1=-82.245, c2=22.689, c3=0.30490))
Cals <- rbind (Cals, data.frame (Project='HIPPO-2', TotalT='TTHR1', c1=-82.441, c2=22.734, c3=0.31780))
Cals <- rbind (Cals, data.frame (Project='HIPPO-2', TotalT='TTHR2', c1=-82.826, c2=22.999, c3=0.28923))
Cals <- rbind (Cals, data.frame (Project='HIPPO-3', TotalT='TTHL1', c1=-82.301, c2=22.731, c3=0.30567))
Cals <- rbind (Cals, data.frame (Project='HIPPO-3', TotalT='TTHL2', c1=-81.619, c2=22.321, c3=0.36363))
Cals <- rbind (Cals, data.frame (Project='HIPPO-3', TotalT='TTHR1', c1=-82.068, c2=22.389, c3=0.37123))
Cals <- rbind (Cals, data.frame (Project='HIPPO-3', TotalT='TTHR2', c1=-82.282, c2=22.761, c3=0.31150))
Cals <- rbind (Cals, data.frame (Project='PREDICT', TotalT='TTHL1', c1=-83.803, c2=25.523, c3=-0.30633))
Cals <- rbind (Cals, data.frame (Project='PREDICT', TotalT='TTHL2', c1=-82.009, c2=22.501, c3=0.30480))
Cals <- rbind (Cals, data.frame (Project='PREDICT', TotalT='TTHR1', c1=-82.154, c2=22.501, c3=0.33251))
Cals <- rbind (Cals, data.frame (Project='PREDICT', TotalT='TTHR2', c1=-82.255, c2=22.704, c3=0.30903))
Cals <- rbind (Cals, data.frame (Project='HEFT10', TotalT='TTHL1', c1=-83.803, c2=25.523, c3=-0.30633))
Cals <- rbind (Cals, data.frame (Project='HEFT10', TotalT='TTHL2', c1=-82.009, c2=22.501, c3=0.30480))
Cals <- rbind (Cals, data.frame (Project='HEFT10', TotalT='TTHR1', c1=-82.154, c2=22.501, c3=0.33251))
Cals <- rbind (Cals, data.frame (Project='HEFT10', TotalT='TTHR2', c1=-82.255, c2=22.704, c3=0.30903))
Cals <- rbind (Cals, data.frame (Project='HIPPO-4', TotalT='TTHL1', c1=-85.735, c2=25.126, c3=-0.28625))
Cals <- rbind (Cals, data.frame (Project='HIPPO-4', TotalT='TTHR1', c1=-75.948, c2=20.355, c3=0.33556))
Cals <- rbind (Cals, data.frame (Project='HIPPO-4', TotalT='TTHR2', c1=-76.781, c2=20.490, c3=0.34571))
Cals <- rbind (Cals, data.frame (Project='HIPPO-5', TotalT='TTFH1', c1=-84.9434, c2=23.6729, c3=0.20332))
Cals <- rbind (Cals, data.frame (Project='HIPPO-5', TotalT='TTFH2', c1=-83.7574, c2=23.5047, c3=0.1857))
Cals <- rbind (Cals, data.frame (Project='HIPPO-5', TotalT='TTHR1', c1=-83.1736, c2=22.9696, c3=0.2381))
Cals <- rbind (Cals, data.frame (Project='HIPPO-5', TotalT='TTHR2', c1=-83.2696, c2=22.9263, c3=0.2656))
Cals <- rbind (Cals, data.frame (Project='ADELE', TotalT='TTFR', c1=-73.4062, c2=21.3463, c3=0.2944))
Cals <- rbind (Cals, data.frame (Project='ADELE', TotalT='TTHR1', c1=-81.0865, c2=22.3040, c3=0.2971))
Cals <- rbind (Cals, data.frame (Project='ADELE', TotalT='TTHR2', c1=-82.0327, c2=22.6917, c3=0.2614))

getCalCoef <- function (P, T, Cals) {
  if (!(P %in% Cals$Project)) {return (c(NA, NA, NA))}
  C <- Cals[Cals$Project == P & Cals$TotalT == T,]
  return (c(C$c1, C$c2, C$c3))
}

plotfile = "~/RStudio/Reprocessing/SpeedRunPlots.pdf"
cairo_pdf (filename = plotfile, onefile=TRUE)
## enable the next to get individual png files instead of one large pdf
#### png (file = sprintf ("./Figures/WINTER%s-%%02d.png", Flight))
print (sprintf ("saving plots to file %s", plotfile))
UseNewCal <- TRUE
SRs <- read.csv ("~/RStudio/Reprocessing/SRs.csv", header=FALSE, sep=';', stringsAsFactors=FALSE)
for (i in 1:97) {
  fileName <- SRs[i,1]
  Project <- sub ('.*/', '', fileName)
  Project <- sub ('.f.*nc', '', Project)
  print (fileName)
  CP <- SpecificHeats()
  ## find available total temperatures
  NCF <- nc_open (fileName)
  N <- names(NCF$var)
  nc_close(NCF)
  if (grepl ("raf_data", fileName)) {
    t <- c(which(grepl("RTH", N) & !grepl("C$", N)), which(grepl("RTF", N) & !grepl("C$", N)))
  } else {
    t <- c(which(grepl("TTH", N) & !grepl("C$", N)), which(grepl("TTF", N) & !grepl("C$", N)))
  }
  TTvar <- N[t]
  VarList <- c("TASX", "GGALT", "THDG")
  VarList <- c(VarList, TTvar)
  Data <- getNetCDF(fileName, VarList)
  r <- setRange(Data$Time, SRs[i,2], SRs[i,3])
  plotWAC(Data[r, c("Time", "TASX")])
  title(sprintf("%s %d--%d", fileName, SRs[i,2], SRs[i,3]))
  for (TT in TTvar) {
    ## get previous calibration
    X <- system(sprintf("ncdump -h %s | grep %s |grep Cal", fileName, TT), intern=TRUE)
    X <- sub (".*= ", '', X)
    X <- sub (' ;', '', X)
    X <- gsub('f', '', X)
    X <- strsplit(X, ',')
    oldCal <- c(as.numeric(X[[1]][1]), as.numeric(X[[1]][2]), as.numeric(X[[1]][3]))
    ## look up new calibration:
    newCal <- getCalCoef (Project, TT, Cals)
    if (!is.na(newCal[1])) {
      ## revise temperatures
      rad <- (oldCal[2]**2-4.*(oldCal[1]-Data[,TT])*oldCal[3])**0.5
      Volts <- (-oldCal[2] + rad) / (2.*oldCal[3])
      Data[, TT] <- newCal[1]+newCal[2]*Volts+newCal[3]*Volts**2
    }
    f <- lm (Data[r,TT] ~ I(Data[r, "TASX"]^2))
    plot (Data$TASX[r]^2, Data[r,TT], type='p', col='blue', pch=20, ylab=TT)
    lines (c(0., 70000), c(coef(f)[1], coef(f)[1]+70000*coef(f)[2]), col='darkorange', lwd=2, lty=2)
    title (sprintf ("%s %s rf is %.3f", fileName, TT, coef(f)[2] * 2 * CP[1]))
  }
}
