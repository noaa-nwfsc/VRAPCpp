#' @title Write Rav File from VRAP 2.0 inputs list
#' @description Takes the input list and write a .rav file. 
#' @details Takes the inputs list and write a .rav file that can be input into the VRAP 1.0 function \link[VRAP]{Main}.
#' @param inputs A list of the necessary input values for \link{RunSims2R} and \link{RunSims2C}.
#' @param FileName Name of the .rav file that data will be written to.
#' @return Nothing. The data is written to ravFileName.

WriteRavFile <- function(inputs, FileName="tmp.rav"){
  # Error checking
  if(inputs$errorType != "GAMMA"){
    stop("ERROR: WriteRavFile requires errorType = GAMMA if writing a rav file which is for VRAP 1.0.")
  }
  inputs$errorType = "YES" # GAMMA
  #make sure all the input values for VRAP 1.0 are in the list
  inputs = InputsBackwardCompat(inputs)

  if(inputs$depen != "NO") stop("ERROR: WriteRavFile requires depen = NO.")
  if(inputs$EscChoice != "YES") stop("ERROR: WriteRavFile requires EscChoice = YES.")
  if(!(toupper(inputs$SRType) %in% c("RIC2", "HOC2", "BEV2"))) stop("ERROR: WriteRavFile requires SRType be RIC2, HOC2 or BEV2.")
  
  fileText <- paste(inputs$Title, ", Title
", inputs$RanSeed, ", Random seed; 0 gives random seed; numbers give fixed seed
", inputs$NRuns, ", Number of runs
", inputs$NYears, ", Number of years
2, 5, Minimum and maximum age (fixed; do not change)
", inputs$ConvergeCrit, ", Convergence criterion (% error) for target ER
NO, 0, -Inf, VRAP 2.0 has no covariates; Center covariate flag and log MS and log Flow mean
", inputs$SRType, ", Spawner Recruit function. Must be Ric2, Hoc2, Bev2
", inputs$prod,",",inputs$cap,", S/R a; b parameters; VRAP 2.0 does not have covariates
Mean and CV for c
Variability type for c
Variability params for c
Mean and CV for d
Variability type for d
Variability params for d
", inputs$depen, ", Depensation? (Must be NO for VRAP 2.0)
0, 0, 1, 1) Esc. level for depensation to start 2) QET quasi-extinction threshold, 3)% predicted return at QET (or for r/s=1 third parameter = 1)
", inputs$EscChoice, ", Determine recruits from adult spawners (Must be YES for VRAP 2.0)
", inputs$errorType, ", Stock-recruit variation type
", inputs$SRErrorA,",",inputs$SRErrorB,", 0, if gamma, shape and scale.  if lognormal, log sd and lag-1 autocor.  3rd param is unused but req for VRAP 1.0.
NO, VRAP 2.0 does not use; Smolt to adult survival w/variation
beta params for smolt to adult survival
0, Number of breakpoints
1, Level to use as base regime
", inputs$TargetU, ", Arbitrary. Must be between 0 and 1 and used by VRAP 1.0 to set step sizes
", ifelse(inputs$MgmtError,"YES","NO"), ", Include error (YES or NO) in ER management
", inputs$GammaMgmtA,",", inputs$GammaMgmtB, ", Gamma parameters for management error
", inputs$ECrit, ", Lower escapement threshold
", inputs$ERecovery, ",", inputs$EndAv, ", Upper escapement threshold (MSY);  # yrs to ave.
", inputs$StepFunc, ", Step ER (ER) or  Pop Capacity (Pop)?
", inputs$BufferStep, ", Step size as a percent of TargetU or Pop capacity
", inputs$BufferStart, ", ", inputs$BufferEnd, ", Min & max ER or Pop for sims as a percent of TargetU or Pop capacity
", inputs$CohortStart[1],", Initial population size at Age  1 
", inputs$CohortStart[2],", Initial population size at Age  2 
", inputs$CohortStart[3],", Initial population size at Age  3 
", inputs$CohortStart[4],", Initial population size at Age  4 
", inputs$CohortStart[5],", Initial population size at Age  5 
", inputs$NatMort[1], ", Age 1 natural mortality
", inputs$NatMort[2], ", Age 2 natural mortality
", inputs$NatMort[3], ", Age 3 natural mortality
", inputs$NatMort[4], ", Age 4 natural mortality
", inputs$NatMort[5], ", Age 5 natural mortality
", inputs$MatRate[2], ", Age 2 average maturation rate
", inputs$MatRate[3], ", Age 3 average maturation rate
", inputs$MatRate[4], ", Age 4 average maturation rate
", inputs$MatRate[5], ", Age 5 average maturation rate
", inputs$PTU[2], ",", inputs$MatU[2],", Age 2 average mixed-maturity and mature fishery fishing rates
", inputs$PTU[3],",", inputs$MatU[3],", Age 3 average mixed-maturity and mature fishery fishing rates
", inputs$PTU[4],",", inputs$MatU[4],", Age 4 average mixed-maturity and mature fishery fishing rates
", inputs$PTU[5],",", inputs$MatU[5],", Age 5 average mixed-maturity and mature fishery fishing rates
endofinput, end of input indicator
",sep="")
  
  cat(fileText, file=FileName)
}