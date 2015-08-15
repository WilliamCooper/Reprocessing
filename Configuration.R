## defines plot variables to use in RPlot functions
## this script serves two functions:
## 1. The variables listed here are included in VarList, used
##    to construct the data.frame used for these plots
## 2. For individual plots, the variables are used to
##    construct each plot as specified (with some exceptions,
##    like the track plot in RPlot1, where the variables are
##    required to be as specified here in plotTrack() ).
if (Project == "HIPPO-2") {
      ## track plot: don't change any exc. GGALT
      ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
      ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
      ## RPlot3: T vs time, specify any number of temperatures 
  VRPlot$PV3 <- c("ATHL1", "ATHL2", "ATHR1", "ATHR2", "AT_A")
      ## RPlot4: compare temperatures in pairs; specify up to five.
      ## first is reference for comparisons
  VRPlot$PV4 <- c("ATHL1", "ATHL2", "ATHR1", "ATHR2", "AT_A")
      ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX")
## don't use if CAVP not available:
  # VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR". "PSFC", "LSRINT_VXL") 
## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSFC", "QCFC", "LSRINT_VXL")           
      ## list of vapor pressures to plot (there is no EW_VXL in HIPPO-2)
      ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "MR", "H2OMR_GMD")
      ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSFC", "PS_A", "PSF")
      ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
      ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
      ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TAS_A")    #plot 7b-top
      ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
      ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
      ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "WI")
      ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
      ## compare calculated AOA/SS vs measured to check sensitivity coefficients
      ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_G", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS") 
      ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
      ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_G", "GGALT", "ALT_G")
      ## at present there is no RPlot14; UHSAS is handled later
  VRPlot$PV14 <- c(NA)
      ## plot concentrations:
  VRPlot$PV15 <- c("CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
      ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
      ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
      ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSFC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSFC", "ATX", "DPXC", "GGALT")
      ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSFC")
      ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
      ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
      ## radiometers not present in HIPPO-2
  VRPlot$PV21 <- c(NA)
  # VRPlot$PV21 <- c("RSTB", "IRBC", "IRTC"))
      ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV22 <- c("CUHSAS_", "TASX")
      ## plot sample of 2DC size distributions 
  VRPlot$PV23 <- c("C1DC_", "TASX")
  # VRPlot$PV30 <- c("CORAW_AL", "FO3_ACD", "COFLOW_AL", "INLETP_AL")
}
if (Project == "HIPPO-3") {
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures 
  VRPlot$PV3 <- c("ATHL1", "ATHL2", "ATHR1", "ATHR2", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- c("ATHL1", "ATHL2", "ATHR1", "ATHR2", "AT_A")
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX")
  ## don't use if CAVP not available:
  # VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR". "PSFC", "LSRINT_VXL") 
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSFC", "QCFC", "LSRINT_VXL")           
  ## list of vapor pressures to plot (there is no EW_VXL in HIPPO-2)
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "MR")  # H2OMR_GMD not in HIPPO-3 files?
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSFC", "PS_A", "PSF")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "WI", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_G", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS") 
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "ROLL", "ROLL_IRS2", "THDG", "THDG_IRS2")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_G", "GGALT", "ALT_G")
  ## at present there is no RPlot14; UHSAS is handled later
  VRPlot$PV14 <- c(NA)
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSFC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSFC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSFC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
  ## radiometers not present in HIPPO-2
  VRPlot$PV21 <- c(NA)
  # VRPlot$PV21 <- c("RSTB", "IRBC", "IRTC"))
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV22 <- c("CUHSAS_", "TASX")
  ## plot sample of 2DC size distributions 
  VRPlot$PV23 <- c("C1DC_", "TASX")
  # VRPlot$PV30 <- c("CORAW_AL", "FO3_ACD", "COFLOW_AL", "INLETP_AL")
}
if (Project == "HIPPO-5") {
  ## track plot: don't change any exc. GGALT
  ## (PALT and PSXC are included to check the pressure altitude calculation)
  VRPlot <- list(PV1=c("LATC", "LONC", "WDC", "WSC", "GGALT", "PALT", "PSXC"))
  ## RPlot2: uses same variables as RPlot1
  VRPlot[[2]] <- VRPlot[[1]]
  ## RPlot3: T vs time, specify any number of temperatures 
  VRPlot$PV3 <- c("ATHR1", "ATHR2", "ATFH1", "ATFH2", "AT_A")
  ## RPlot4: compare temperatures in pairs; specify up to five.
  ## first is reference for comparisons
  VRPlot$PV4 <- VRPlot$PV3
  ## the next line should end with ATX and list dewpoints
  VRPlot$PV5 <- c("DP_VXL", "DP_DPL", "DP_DPR", "ATX")
  ## don't use if CAVP not available:
  # VRPlot$PV5 <- c(VRPlot$PV5, "CAVP_DPL", "CAVP_DPR". "PSFC", "LSRINT_VXL") 
  ## use only if CAVP not available: will plot surrogate CAVP
  VRPlot$PV5 <- c(VRPlot$PV5, "PSFC", "QCFC", "LSRINT_VXL")           
  ## list of vapor pressures to plot (there is no EW_VXL in HIPPO-2)
  ## then list "MR" (will calculate mixing ratios corresponding to EW)
  VRPlot$PV5 <- c(VRPlot$PV5, "EW_DPL", "EW_DPR", "MR")  # H2OMR_GMD not in HIPPO-3 files?
  ## pressure measurements, first is reference
  VRPlot$PV6 <- c("PSFC", "PS_A", "PSF")
  ## dynamic pressure measurements, uncorrected, first is reference
  VRPlot$PV7 <- c("QCF", "QCR")                       #plot 7a-top
  ## dynamic pressure measurements, corrected, first is reference
  VRPlot$PV7 <- c(VRPlot$PV7, "QCFC", "QCRC", "QC_A") #plot 7a-bottom
  ## list TAS measurements to plot vs time
  VRPlot$PV7 <- c(VRPlot$PV7, "TASF", "TASR", "TAS_A")    #plot 7b-top
  ## and Mach numbers
  VRPlot$PV7 <- c(VRPlot$PV7, "MACHF", "MACHR", "MACH_A") #plot 7b-bottom
  ## plot 8 is total pressure, sum of 1+2 and 3+4; expect agreement
  VRPlot$PV8 <- c("PSF", "QCF", "PS_A", "QC_A")
  ## wind direction, speed, vertical wind: keep these unchanged
  VRPlot$PV9 <- c("WDC", "IWD", "WSC", "IWS", "WIC", "WI", "ADIFR")  # need ADIFR for WIX
  ## IRU velocity errors from differences (Schuler oscillation); don't change
  VRPlot$PV10 <- c("GGVEW", "VEW", "GGVNS", "VNS", "GGQUAL")
  ## for plotting effect of complementary filter
  VRPlot$PV10 <- c(VRPlot$PV10, "VEWC", "VNSC")
  ## compare calculated AOA/SS vs measured to check sensitivity coefficients
  ## 3rd variable is aircraft vertical speed, preferable is GGVSPD, not in HIPPO-2
  VRPlot$PV11 <- c("AKRD", "PITCH", "VSPD_G", "TASX", "SSRD", "WDC", "WSC", "GGVEW", "GGVNS") 
  ## compare IRU attitude angles, IRS1 and IRS2
  VRPlot$PV12 <- c("PITCH", "PITCH_IRS2", "PITCH_IRS3", "ROLL", "ROLL_IRS2", "ROLL_IRS3", "THDG", "THDG_IRS2", "THDG_IRS3")
  ## compare IRU measurements of acceleration, vertical speed, altitude
  VRPlot$PV13 <- c("ACINS", "ACINS_IRS2", "VSPD", "VSPD_G", "GGALT", "ALT_G")
  ## at present there is no RPlot14; UHSAS is handled later
  VRPlot$PV14 <- c(NA)
  ## plot concentrations:
  VRPlot$PV15 <- c("CONCU_", "CONCU100_", "CONCU500_")  # top panel
  VRPlot$PV15 <- c(VRPlot$PV15, "CONCD_", "CONC1DC_")   # 2nd panel
  VRPlot$PV15 <- c(VRPlot$PV15, "USHFLW_", "USMPFLW_", "UREF_", "USCAT_")  # 3rd panel
  ## list mean diameters, liquid water, RICE
  VRPlot$PV16 <- c("DBARD_", "DBARU_", "DBAR1DC_", "PLWCD_", "PLWCC", "PLWC", "PLWC1DC_", "RICE")
  ## if CDP present, include TCNTD_ and REJDOF_ to get acceptance ratio
  VRPlot$PV16 <- c(VRPlot$PV16, "TCNTD_", "REJDOF_", "AVGTRNS_", "CDPLSRP_")
  ## plot variable for skew-T sounding (don't normally change)
  VRPlot$PV17 <- c("PSFC", "ATX", "DPXC")
  VRPlot$PV18 <- c("PSFC", "ATX", "DPXC", "GGALT")
  ## time history and vertical profile of potential temperatures:
  VRPlot$PV19 <- c("THETA", "THETAV", "THETAE", "THETAP", "THETAQ", "PSFC")
  ## additions for checking calculations:
  VRPlot$PV19 <- c(VRPlot$PV19, "ATX", "PSXC", "EWX")
  ## plot sample of CDP size distributions
  VRPlot$PV20 <- c("CCDP_", "TASX")
  ## radiometers not present in HIPPO-2
  VRPlot$PV21 <- c(NA)
  # VRPlot$PV21 <- c("RSTB", "IRBC", "IRTC"))
  ## plot sample of UHSAS size distributions; include PCASP if present (not HIPPO-2)
  VRPlot$PV22 <- c("CUHSAS_", "TASX")
  ## plot sample of 2DC size distributions 
  VRPlot$PV23 <- c("C1DC_", "TASX")
  # VRPlot$PV30 <- c("CORAW_AL", "FO3_ACD", "COFLOW_AL", "INLETP_AL")
}

