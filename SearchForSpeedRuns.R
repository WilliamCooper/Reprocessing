## search for speed runs in data files below /Data 

require(Ranadu)

Projects <- c("ADELE", "CONTRAST", "DC3", "DC3-TEST", "DEEPWAVE", "HEFT08", "HEFT10", "HIPPO-1", "HIPPO-2",
              "HIPPO-3", "HIPPO-4", "HIPPO-5", 
              "IDEAS-4-GV", "MPEX", "PACDEX", "PodCertGV", "PREDICT", 
              "ProgSci", "SAANGRIA-TEST", "SPRITE-II",
              "START08", "TORERO", "TREX")
source ("./PlotFunctions/SpeedRunSearch.R")
sink("SRs", append=TRUE)
for (Project in Projects) {
  Directory <- sprintf ("/scr/raf/Prod_Data/%s", Project)
  Files <- list.files (Directory, pattern="*f...nc$", full.names=TRUE, recursive=TRUE)
  DontStart <- FALSE
  for (File in Files) {
    #if (grepl ('HIPPO_oldish', File)) {DontStart <- FALSE}
    #if (grepl ('IDEAS4_mf01', File)) {DontStart <- FALSE}
    #if (grepl ('nomadss_hyb', File)) {DontStart <- FALSE}
    #print (sprintf ("File %s DontStart %d", File, DontStart))
    if (DontStart) {next}
    if (grepl ('HIPPO_oldish', File)) {next}
    if (grepl ('IDEAS4_mf01', File)) {next}
    #print (sprintf ("searching %s", File))
    if (grepl ("zip", File)) {next}
    if (grepl ("_hyb", File)) {next}
    Data <- getNetCDF (File, c("TASX", "GGALT"))
    if (!is.data.frame(Data)) {next}
    Data[!is.na(Data$TASX) & (Data$TASX < 110), ] <- NA
    SpeedRunSearch (Data, File)
  }
}
sink()
