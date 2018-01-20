#' @title Run simulations sensu VRAP 1.0
#' @description RunSims.VRAP1 takes the input list, runs the VRAP simulations, and returns the summary statistics used by VRAP 1.0
#' @param inputs Inputs from .rav file
#' @param version R or C++
#' @details This function is to produce VRAP 1.0 output stats using VRAP 1.0 functions.
#' @return  list with inputs, SummaryStats, staticvars, comp.time.
RunSims.VRAP1 = function(inputs, version="R"){

  # RUN SIMULATIONS TO GET TOTESC AND TOTAEQMORT
  t1 <- Sys.time()
  if(version=="R") SimResults = RunSims2R(inputs)
  if(version=="C") SimResults = RunSims2C(inputs)
  t2 <- Sys.time()
  
  # Set up list that will hold static computed variables.  From VRAP AEQcalc.R
  staticvars=list()
  AEQ = rep(0, inputs$MaxAge);
  TmpA = 0; TmpS = 0
  for(Age in inputs$MaxAge:inputs$MinAge){
    AEQ[Age] = inputs$MatRate[Age] + TmpS * (1 - inputs$MatRate[Age]) * TmpA
    TmpA = AEQ[Age]
    TmpS = 1 - inputs$NatMort[Age]
  }
  staticvars$AEQ = AEQ
  
  # COMPUTE FACTOR TO TRANSLATE AEQ RECRUITMENT TO AGE 1 from VRAP Recruits.R
  Tmp = 0
  X9 = 1 - inputs$NatMort[1] #survival age 1
  for(Age in inputs$MinAge:inputs$MaxAge){
    X9 = X9 * (1 - inputs$NatMort[Age])
    Tmp = Tmp + X9 * inputs$MatRate[Age]
    X9 = X9 * (1 - inputs$MatRate[Age])
  }
  staticvars$RecruitsAtAge1 = Tmp
  
  # COMPUTE THE SUMMARYSTATS LIST USED IN VRAP 1.0
  
  SummaryStats = CompStats(inputs, SimResults)
  
  
  return(list(inputs=inputs, SummaryStats=SummaryStats, staticvars=staticvars, time=t2-t1))
}