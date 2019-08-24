

require(Ranadu, quietly = TRUE, warn.conflicts=FALSE)

# needed packages
library(zoo)
require(signal)

## Get file to process:

## Specify the Project and Flight:
Project <- 'SOCRATES'
Flight <- 'rf15h'  # should be high-rate
getNext <- function(ProjectDir, Project) {
  Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectDir),
    sprintf ("%srf..hT.nc", Project)), decreasing = TRUE)[1]
  if (is.na (Fl)) {
    Flight <- 'rf01h'
  } else {
    Flight <- sub (".*rf", '',  sub ("hT.nc", '', Fl))
    Flight <- as.numeric(Flight)+1
    Flight <- sprintf ('rf%02dh', Flight)
  }
  return (Flight)
}
if (!interactive()) {  ## can run interactively or via Rscript
  run.args <- commandArgs (TRUE)
  if (length (run.args) > 0) {
    if (nchar(run.args[1]) > 1) {
      Project <- run.args[1]
      ProjectDir <- Project
    }
  } else {
    print ("Usage: Rscript FilterDynamicHeating.R Project Flight")
    print ("Example: Rscript FilterDynamicHeating.R SOCRATES rf15h")
    stop("exiting...")
  }
  ## Flight
  if (length (run.args) > 1) {
    ALL <- FALSE
    if (run.args[2] == 'ALL') {
      ALL <- TRUE
    } else if (run.args[2] != 'NEXT') {
      Flight <- run.args[2]
    } else {
      ## Find max already-processed rf..h in data directory,
      ## Use as default if none supplied via command line:
      Flight <- getNext(ProjectDir, Project)
    }
  }
} else {
  x <- readline (sprintf ("Project is %s; CR to accept or enter new project name: ", Project))
  if (nchar(x) > 1) {
    Project <- x
    if (grepl ('HIPPO', Project)) {
      ProjectDir <- 'HIPPO'
    } else {
      ProjectDir <- Project
    }
  }
  x <- readline (sprintf ("Flight is %s; CR to accept, number 'ALL' or 'NEXT' for new flight name: ", 
                          Flight))
  ALL <- FALSE
  if (x == 'ALL') {
    ALL <- TRUE
  } else if (x == 'NEXT') {
    Flight <- getNext(ProjectDir, Project)
  } else if (nchar(x) > 0 && !is.na(as.numeric(x))) {
    Flight <- sprintf('rf%02dh', as.numeric(x))
  }
}
## A function to transfer attributes:
copy_attributes <- function (atv, v, nfile) {
  for (i in 1:length(atv)) {
    aname <- names(atv[i])
    if (grepl ('name', aname)) {next}  # skips long and standard names
    if (grepl ('units', aname)) {next}
    if (grepl ('Dependencies', aname)) {next}
    if (grepl ('actual_range', aname)) {next}
    if (is.numeric (atv[[i]])) {
      ncatt_put (nfile, v, attname=aname, attval=as.numeric(atv[[i]]))
    } else {
      ncatt_put (nfile, v, attname=aname, attval=as.character (atv[[i]]))
    }
  }
}

processFile <- function(ProjectDir, Project, Flight) {
  ## Find the available air_temperature variables:
  fname <- file.path(DataDirectory(), sprintf('%s/%s%s.nc', 
    ProjectDir, Project, Flight))
  FI <- DataFileInfo(fname, LLrange = FALSE)
  TVARS <- FI$Measurands$air_temperature
  TVARS <- TVARS[-which ('ATX' == TVARS)]  # don't include ATX
  
  ## get the old netCDF variables needed to calculate the modified variables
  VarList <- standardVariables(TVARS)
  D <- getNetCDF (fname, VarList)  
  Rate <- attr(D, 'Rate')
  
  ## Calculate the new variables:
  D$Cp <- SpecificHeats (D$EWX / D$PSXC)[, 1]
  D$DH <- D$TASX^2 / (2 * D$Cp)
  # filter DH:
  CutoffPeriod <- rep(Rate * 1.0, length(TVARS)) # Standard is 1 s
  probe <- rep('HARCO', 3)  # used to determine the recovery factor
  # check for ATF
  ic <- which(grepl('ATF', TVARS))
  if (length(ic) > 0) {
    CutoffPeriod[ic] <- Rate * 0.5  # 0.5 s for ATFx
    probe[ic] <- 'UNHEATED'
  }
  ic <- which(grepl('ATH2', TVARS))
  if (length(ic) > 0) {
    probe[ic] <- 'HARCOB'
  }
  if (Rate == 1) {  # Protection against script failure for a LRT file
    CutoffPeriod[CutoffPeriod == 1] <- 2.2
    CutoffPeriod[CutoffPeriod == 0.5] <- 2.0
  }
  
  DHM <- rep(D$DH, length(TVARS))
  dim(DHM) <- c(length(D$DH), length(TVARS))
  newTVARS <- paste0(TVARS, 'Y')
  for (i in 1:length(TVARS)) {
    DHM[, i] <- zoo::na.approx (as.vector(D$DH), maxgap=1000*Rate, 
      na.rm = FALSE, rule = 2)
    DHM[is.na(DHM[, i]), i] <- 0
    DHM[, i] <- signal::filtfilt (signal::butter (3, 
      2/CutoffPeriod[i]), DHM[, i])
    D[, newTVARS[i]] <- D[, TVARS[i]] + (D$DH - DHM[, i]) * RecoveryFactor(D$MACHX, probe=probe[i])
  }
  
  ## create and open a copy of the old file for writing:
  fnew <- sub ('.nc', 'T.nc', fname)
  ## beware: overwrites without warning!!
  Z <- file.copy (fname, fnew, overwrite=TRUE)  
  netCDFfile <- nc_open (fnew, write=TRUE) 
  ## retrieve dimension info from the old file
  Dimensions <- attr (D, "Dimensions")
  Dim <- Dimensions[["Time"]]
  if ("sps25" %in% names (Dimensions)) {
    Rate <- 25
    Dim <- list(Dimensions[["sps25"]], Dimensions[["Time"]])
  }
  DATT <- D
  
  ## variables to add to the netCDF file:
  VarNew <- newTVARS   
  VarOld <- TVARS
  VarUnits <- rep("deg_C", length(TVARS))
  VarLongName <- rep("Ambient Temperature, filtered", length(TVARS))
  VarStdName <- rep("air_temperature", length(TVARS))
  Dependencies <- rep(sprintf('2 %s TASX', TVARS[1]), length(TVARS))
  for (i in 1:length(TVARS)) {
    Dependencies[i] <- sprintf('2 %s TASX', TVARS[i])
  }
  
  ## create the new variables
  varCDF <- list ()
  for (i in 1:length(VarNew)) {  
    ## create the new variable and add it to the netCDF file
    varCDF[[i]] <- ncvar_def (VarNew[i],  
      units=VarUnits[i], 
      dim=Dim, 
      missval=as.single(-32767.), prec="float", 
      longname=VarLongName[i])
    if (i == 1) {
      newfile <- ncvar_add (netCDFfile, varCDF[[i]])
    } else {
      newfile <- ncvar_add (newfile, varCDF[[i]])
    }
    ## transfer attributes from the old variable and add new ones
    ATV <- ncatt_get (netCDFfile, VarOld[i])
    copy_attributes (ATV, VarNew[i], newfile)
    ncatt_put (newfile, VarNew[i], attname="standard_name", 
      attval=VarStdName[i])
    ncatt_put (newfile, VarNew[i], attname="Dependencies", 
      attval=Dependencies[i])
    ## add the measurements for the new variable
    if (Rate == 1) {
      ncvar_put (newfile, varCDF[[i]], D[, VarNew[i]])
    } else if (Rate == 25) {
      ncvar_put (newfile, varCDF[[i]], D[, VarNew[i]], 
        count=c(25, nrow(D)/25))
    }
  }
  ## then close to write the new file
  nc_close (newfile)
}

if (ALL) {
  Fl <- sort (list.files (sprintf ("%s%s/", DataDirectory (), ProjectDir),
    sprintf ("%srf..h.nc", Project)), decreasing = TRUE)
  for (flt in Fl) {
    fcheck <- file.path(DataDirectory(), ProjectDir, '/', flt, fsep = '')
    fcheck <- sub('.nc', 'T.nc', fcheck)
    if (file.exists(fcheck)) {
      print (sprintf('processed file %s exists; skipping', flt))
    } else {
      print (sprintf('processing file %s', flt))
      processFile (ProjectDir, Project, sub('.*rf', 'rf', sub('.nc', '', flt)))
    }
  }
} else {
  processFile (ProjectDir, Project, Flight)
}
