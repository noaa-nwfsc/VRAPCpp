#' @title Run VRAP 2.0 Simulations in C++
#' @description Run the VRAP2 simulations in C++ over a specified range of escapement rates (ERs). 
#' @details Calls the C++ function `simFish` to run the VRAP simulations
#'  and returns a 3D array of the total escapement
#'  at each exploitation rate (ER) for NRuns over NYears.
#' @param input A list of the necessary input values (can be taken from .rav file).
#' @param silent Whether print progress as the current ER value.
#' @return A list with the input and the 3D array of total escapment values.

RunSims2C <- function(inputs, silent=TRUE){
  # initialize output array
  totEsc <- array(NA,dim=c(inputs$StepNum,inputs$NRuns,inputs$NYears))
  HRscale <- 1 # multiply this times the harvest rates.
  # calcualte the target exploitation rates based on start, step size, and steps
  targetER <- inputs$StepStart + inputs$StepSize * (0:(inputs$StepNum-1))
  # calculate AEQ and recruitsFromAgeOneFish
  AEQ <- c(0,0,0,0,inputs$MatRate[5]) 
  for(age in 4:2){
    AEQ[age] <- inputs$MatRate[age] + (1-inputs$NatMort[age+1]) * (1 - inputs$MatRate[age]) * AEQ[age+1]
  }
  recruitsFromAgeOneFish <- (1-inputs$NatMort[1])*(1-inputs$NatMort[2])*AEQ[2]
  for(ERind in 1:inputs$StepNum){
    if(!silent) print(paste("============= target ER =",targetER[ERind]))
    totEsc[ERind,,] <- simFish(
      NRuns=inputs$NRuns,
      NYears=inputs$NYears,
      targetER=targetER[ERind],
      MgmtError=inputs$MgmtError, 
      GammaMgmtA=inputs$GammaMgmtA, GammaMgmtB=inputs$GammaMgmtB,
      errorType=inputs$errorType,
      SRErrorA=inputs$SRErrorA, SRErrorB=inputs$SRErrorB,
      CohortStart=inputs$CohortStart, prod=inputs$prod, cap=inputs$cap,
      MatRate = inputs$MatRate, NatMort=inputs$NatMort, PTU=inputs$PTU, MatU=inputs$MatU,
      AEQ=AEQ)
  }
  list(inputs=inputs, totEsc=totEsc)
}