#' @title CompStats
#' @description Returns the statistics (calculated values) needed to produce the summary output file
#' @details This function similar but not identical to the original VB function in VRAP
#' @param BufNum Which simulation (TargetER or Pop) is this for.
#' @param inputs Inputs from .rav file
#' @param BufSRb Capacity.  Changes if StepFunc=Pop.  Otherwise stays the same.
#' @param YearStats list of computed variables for each year: AEQMort, Escpmnt[Year,] = Escpmnt, TotAdultEscpmnt, TotAEQMort, TotEscpmnt,TempCohort.
#' @param SummaryStats list with the summary statistics to be updated
#' @return Updated SummaryStats list
CompStatsEEH = function(BufNum, inputs, BufSRb, YearStats, SummaryStats){ 
  #BufNum is BufMax, an integer
  
  NYears = inputs$NYears
  NRuns = inputs$NRuns
  SRErrorA = inputs$SRErrorA
  SRErrorA = inputs$SRErrorB
  SRa = inputs$BSRa
  if(inputs$StepFunc=="POP"){
    SRb = BufSRb
  }else{
    SRB = inputs$BSRb
  }
  SRc = inputs$BSRc
  SRd = inputs$BSRd
  SurvScale = inputs$SurvScale
  
  #In VB code Escapmnt (over ages) is redefined to be TotEscpmnt (sum over ages)
  #kept terminology closer to what is being done
  TotEscpmnt = YearStats$TotEscpmnt #a matrix of Escpmnt for each year
  
  # COMPUTE STATISTICS
  # Sum up over all reps and divide by reps
  # COMPUTE AVERAGE MORTALITY OVER REPETITIONS
  #divide by NRuns because we want the average over all runs
  #Note in VB code, AEQMort is initially by age and then in SaveYearData, it is reassigned to be TotAEQMort.
  #Here I keep AEQMort and TotAEQMort separate
  #Thus the VB code refers to YearStats.AEQMort while VRAPR uses YearStats$TotAEQMort
  SummaryStats$AvgAEQMort[BufNum] = SummaryStats$AvgAEQMort[BufNum]+mean(YearStats$TotAEQMort)/inputs$NRuns
  
  # COMPUTE AVERAGE CALENDAR YEAR HARVEST RATE
  #CalendarHR = YearStats$CalendarHR
  #divide by NRuns because we want the average over all runs
  SummaryStats$AvgCaHR[BufNum] = SummaryStats$AvgCaHR[BufNum] + mean(YearStats$CalendarHR)/inputs$NRuns
  
  # COMPUTE PROPORTION OF TIMES ESCAPEMENT WAS LESS THAN LOWER LEVEL
  # add 1/NRuns because we want the fraction out of all runs
  SummaryStats$AvgECrit[BufNum] = SummaryStats$AvgECrit[BufNum] + sum(TotEscpmnt < inputs$ECrit)/(inputs$NYears*inputs$NRuns)
  
  # FIND MINIMUM, MAXIMUM, AND AVERAGE ESCAPEMENT        
  SummaryStats$MinEscpmnt[BufNum,] = pmin(SummaryStats$MinEscpmnt[BufNum,],TotEscpmnt,na.rm=TRUE)
  SummaryStats$MaxEscpmnt[BufNum,] = pmax(SummaryStats$MaxEscpmnt[BufNum,],TotEscpmnt,na.rm=TRUE)
  #divide these by NRuns because we want the average over all runs
  SummaryStats$AvgEscpmnt[BufNum, ] = SummaryStats$AvgEscpmnt[BufNum, ] + TotEscpmnt/inputs$NRuns
  
  
  # COMPUTE PROPORTION OF TIMES ENDING ESCAPEMENT WENT BELOW QET (DL2) (depensation)
  #uses last EndAv years
  assessmentYears = (inputs$NYears - (inputs$EndAv - 1)):inputs$NYears
  # divide by NRuns at end to get frac of reps below
  if(any(TotEscpmnt[assessmentYears] < (inputs$DL2 + 1))){
    # add 1/NRuns because we want the fraction out of all runs
    SummaryStats$PropExt[BufNum] = SummaryStats$PropExt[BufNum] + 1/inputs$NRuns
  }
  
  # COMPUTE PROPORTION OF TIMES ENDING ESCAPEMENT EXCEEDED UPPER LEVEL
  EscpmntPositive = TotEscpmnt
  EscpmntPositive[EscpmntPositive<1]=1
  #assessmentYears defined above; uses geometric mean over last EndAv years
  GeomeanEscpmnt = exp(mean(log(EscpmntPositive)[assessmentYears]))
  if(GeomeanEscpmnt > inputs$ERecovery){
    # add 1/NRuns because we want the fraction out of all runs
    SummaryStats$PropRec[BufNum] = SummaryStats$PropRec[BufNum] + 1/inputs$NRuns
  }
  
  # COMPUTE BROOD YEAR HARVEST RATES
  BYrAEQMort = rep(0, inputs$NYears - (inputs$MaxAge-inputs$MinAge)) # ReDim BYrAEQMort(-1 To NYears% - 5)
  BYrEscpmnt = rep(0, inputs$NYears - (inputs$MaxAge-inputs$MinAge)) #ReDim BYrEscpmnt(-1 To NYears% - 5)
  # AGE 2
  Age = 2
  Years = 1:(inputs$NYears - (inputs$MaxAge-inputs$MinAge)) + (Age-inputs$MinAge)
  Byrs = Years - Age
  BYrAEQMort[Byrs+2] = BYrAEQMort[Byrs+2] + YearStats$AEQMort[Years,2]
  BYrEscpmnt[Byrs+2] = BYrEscpmnt[Byrs+2] + YearStats$Escpmnt[Years,2]
  
  # AGE 3
  Age= 3
  Years = 1:(inputs$NYears - (inputs$MaxAge-inputs$MinAge)) + (Age-inputs$MinAge)
  Byrs = Years - Age
  BYrAEQMort[Byrs+2] = BYrAEQMort[Byrs+2] + YearStats$AEQMort[Years,3]
  BYrEscpmnt[Byrs+2] = BYrEscpmnt[Byrs+2] + YearStats$Escpmnt[Years,3]
  
  # AGE 4
  Age = 4
  Years = 1:(inputs$NYears - (inputs$MaxAge-inputs$MinAge)) + (Age-inputs$MinAge)
  Byrs = Years - Age
  BYrAEQMort[Byrs+2] = BYrAEQMort[Byrs+2] + YearStats$AEQMort[Years,4]
  BYrEscpmnt[Byrs+2] = BYrEscpmnt[Byrs+2] + YearStats$Escpmnt[Years,4]
  
  # AGE 5
  Age = 5
  Years = 1:(inputs$NYears - (inputs$MaxAge-inputs$MinAge)) + (Age-inputs$MinAge)
  Byrs = Years - Age
  BYrAEQMort[Byrs+2] = BYrAEQMort[Byrs+2] + YearStats$AEQMort[Years,5]
  BYrEscpmnt[Byrs+2] = BYrEscpmnt[Byrs+2] + YearStats$Escpmnt[Years,5]
  
  TempBYrHR = BYrAEQMort / (BYrAEQMort + BYrEscpmnt)
  TempBYrHR[BYrAEQMort < 1E-16]=0
  #divide these by NRuns because we want the average over all runs
  
  SummaryStats$BufAvgBYrHR[BufNum] = SummaryStats$BufAvgBYrHR[BufNum] + mean(TempBYrHR)/inputs$NRuns
  SummaryStats$AvgBYrHR[BufNum, ] = SummaryStats$AvgBYrHR[BufNum, ] + TempBYrHR/inputs$NRuns
  #don't divide these by NRuns because Max/Min
  SummaryStats$MinBYrHR[BufNum, ] = pmin(SummaryStats$MinBYrHR[BufNum, ], TempBYrHR, na.rm=TRUE)
  SummaryStats$MaxBYrHR[BufNum, ] = pmax(SummaryStats$MaxBYrHR[BufNum, ], TempBYrHR, na.rm=TRUE)
  
  SummaryStats$AveRanMarine[BufNum] = SummaryStats$AveRanMarine[BufNum] + mean(YearStats$RanMarine)/inputs$NRuns
  SummaryStats$AveRanFlow[BufNum] = SummaryStats$AveRanFlow[BufNum] + mean(YearStats$RanFlow)/inputs$NRuns
  
  return(SummaryStats)
}