## search for speed runs in data files below /Data 

require(Ranadu)

Files <- list.files (DataDirectory (), pattern="*f...nc$", full.names=TRUE, recursive=TRUE)
source ("./PlotFunctions/SpeedRunSearch.R")
for (File in Files) {
  print (sprintf ("searching %s", File))
  if (grepl ("zip", File)) {next}
  Data <- getNetCDF (File, c("TASX", "GGALT"))
  Data[!is.na(Data$TASX) & (Data$TASX < 110), ] <- NA
  SpeedRunSearch (Data)
}
