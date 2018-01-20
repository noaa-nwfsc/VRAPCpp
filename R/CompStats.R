#' @title CompStats
#' @description Returns the statistics (calculated values) needed to produce the summary output files
#' @details This function similar but not identical to the original VB function in VRAP
#' @param inputs Inputs from .rav file
#' @param SimResults A list from \link{RunSims2R} or \link{RunSims2C} with totEsc and totAEQMort for each ER/Pop value, each sim rep, and each year.
#' @return SummaryStats and staticvars list
CompStats = function(inputs, SimResults){ 

  # Set up the SummaryStats
  SummaryStats = list()
  for(el in c("MinEscpmnt","MaxEscpmnt", "AvgEscpmnt"))
    SummaryStats[[el]] = matrix(NA, inputs$BufMax, inputs$NYears)
  for(el in c("MinBYrHR","MaxBYrHR","AvgBYrHR"))
    SummaryStats[[el]] = matrix(NA, inputs$BufMax, inputs$NYears - (inputs$MaxAge - inputs$MinAge))
  for(el in c("AvgAEQMort","AvgECrit","AvgCaHR", "BufAvgBYrHR", "PropExt", "PropRec", "BufAvgBYrHR", "AveRanFlow", "AveRanMarine"))
    SummaryStats[[el]] = rep(NA, inputs$BufMax)
  
  # Calculate the SummaryStats sensu VRAP 1.0
  totEsc <- SimResults$totEsc 
  totAEQmort <- SimResults$totAEQmort 
  
  SummaryStats$AvgEscpmnt = apply(totEsc,c(1,3), mean)
  SummaryStats$MinEscpmnt = apply(totEsc,c(1,3), min)
  SummaryStats$MaxEscpmnt = apply(totEsc,c(1,3), max)
  
  SummaryStats$AvgAEQMort = apply(totAEQmort,c(1,3), mean)
  
  CalendarHR = totAEQmort/( totAEQmort + totEsc)
  SummaryStats$AvgCaHR = apply(CalendarHR, 1, mean)

  BelowECrit = totEsc < inputs$ECrit
  SummaryStats$AvgECrit = apply(BelowECrit, 1, function(x){sum(x)/(inputs$NYears*inputs$NRuns)})
  
  assessmentYears = (inputs$NYears - (inputs$EndAv - 1)):inputs$NYears
  assesstotEsc = totEsc[,,assessmentYears]
  tmp = apply( assesstotEsc,c(1,2), function(x){any(x<(inputs$DL2 + 1))} )
  SummaryStats$PropExt = apply(tmp,1,sum)/inputs$NRuns
  
  EscpmntPositive = assesstotEsc
  EscpmntPositive[EscpmntPositive<1]=1
  #assessmentYears defined above; uses geometric mean over last EndAv years
  GeomeanEscpmnt = exp(mean(log(EscpmntPositive)[assessmentYears]))
  tmp = apply( assesstotEsc,c(1,2), function(x){ exp(mean(log(x))) } )
  tmp = tmp > inputs$ERecovery
  SummaryStats$PropRec = apply(tmp,1,sum)/inputs$NRuns
  
  return(SummaryStats)
}