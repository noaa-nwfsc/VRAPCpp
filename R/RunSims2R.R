#' @title Run VRAP 2.0 Simulations in native R
#' @description Run the VRAP2 simulations in native R over a specified range of escapement rates (ERs). 
#' @details Runs the simulations and returns an 3D array of the total escapement
#' at each exploitation rate (ER) for NRuns over NYears.
#' @param inputs A list of the necessary input values (can be taken from .rav file).
#' @param silent Whether print progress as the current ER value.
#' @return A list with the inputs and the 3D array of total escapment values.

RunSims2R <- function(inputs, silent=TRUE){
  # initialize output array
  totEsc <- array(NA,dim=c(inputs$ERnumSteps+1,inputs$NRuns,inputs$NYears))
  HRscale <- 1 # multiply this times the harvest rates.
  # calcualte the target exploitation rates based on start, step size, and steps
  targetER <- inputs$BufferStart + inputs$BufferStepSize * (0:inputs$BufferNumSteps)
  # calculate AEQ and recruitsFromAgeOneFish
  AEQ <- c(0,0,0,0,inputs$MatRate[5]) 
  for(age in 4:2){
    AEQ[age] <- inputs$MatRate[age] + (1-inputs$NatMort[age+1]) * (1 - inputs$MatRate[age]) * AEQ[age+1]
  }
  recruitsFromAgeOneFish <- (1-inputs$NatMort[1])*(1-inputs$NatMort[2])*AEQ[2]
  
  for(ERind in 1:(inputs$ERnumSteps+1)){
    if(!silent) print(paste("============= target ER =",targetER[ERind]))
    for(sim in 1:inputs$NRuns){ # loop through 1000 25 yr simulations
      logSRerror <- rnorm(1, 0, sd=inputs$SRerrorB) # not currently used
      Cohort <- inputs$CohortStart # initialize population
      for(year in 1:25){ # loop through 25 year simulation
        # apply natural mortality
        Cohort <- Cohort*(1-inputs$NatMort)
        # generate management error
        actualER <- targetER[ERind]
        if(inputs$MgmtError) 
          actualER <- min(actualER * rgamma(1, inputs$GammaMgmtA, scale=inputs$GammaMgmtB),1)
        # loop to achieve target exploitation rate unless targetER=0
        if(actualER==0){
          PTUAdj <- 0
          MatUAdj <- 0
        }else{
          numTrys <- 1
          lastAEQmort <- 99
          repeat{
            # adjust preterminal and terminal fishing rates
            PTUAdj <- inputs$PTU*HRscale 
            MatUAdj <- inputs$MatU*HRscale
            # can't be larger than 1
            PTUAdj[PTUAdj>1] <- 1
            MatUAdj[MatUAdj>1] <- 1
            # calculate AEQ fishing mortality, escapement, and the exploitation rate
            AEQmort <- Cohort*(PTUAdj*AEQ + (1-PTUAdj)*inputs$MatRate*MatUAdj) 
            Escpmnt <- Cohort*(1-PTUAdj)*(1-MatUAdj)*inputs$MatRate
            totAEQmort <- sum(AEQmort)
            totEscpmnt <- sum(Escpmnt)
            ER <- totAEQmort/(totAEQmort+totEscpmnt)
            # calculate the error rate (how far the actual ER is from the target)
            ERerror <- abs(ER-actualER)/actualER  
            # exit loop if you are close enough OR other criteria are met. Otherwise adjust HRscale.
            if(totAEQmort+totEscpmnt < 1 | totAEQmort==0 | numTrys > 100 | totAEQmort==lastAEQmort){
              if(!silent){
                cat(paste("Target ER = ",targetER[ERind],"  Sim = ",sim,"  Year = ",year,
                          "  goal - actual = ",round(actualER,3)," - ",round(ER,3),
                          "  HRscale = ",round(HRscale,3),"  numTrys = ",numTrys,
                          "  totEsc = ",round(totEscpmnt,1),"  totAEQmort = ",round(totAEQmort,1),"\n",sep=""))
              }
              break
            }else if(ERerror < inputs$ConvergeCrit) break
            else HRscale <- HRscale*actualER/ER
            numTrys <- numTrys+1
            lastAEQmort <- totAEQmort
          } 
        }
        # calculate new cohort
        newCohort <- Cohort*(1-PTUAdj)*(1-inputs$MatRate)
        Escpmnt <- Cohort*(1-PTUAdj)*(1-MatUAdj)*inputs$MatRate
        Escpmnt[Escpmnt < 1] <- 0
        # calculate adult escapement
        adultEscapement <- sum(Escpmnt[3:5])
        # age the cohort
        Cohort[2:5] <- newCohort[1:4]
        # now fill in age 1 fish using the spawner-recruit function.
        AEQrecruits <- inputs$prod * adultEscapement * exp(-adultEscapement / inputs$cap)
        if(inputs$errorType=="GAMMA"){
          SRerror <- rgamma(1,inputs$SRErrorA,scale=inputs$SRErrorB)
        }else if(inputs$errorType=="LOGNORMAL"){
          # SRErrorA = lognormal sd, SRErrorB = autocorrelation
          logSRerror <- inputs$SRErrorB*logSRerror + sqrt(1-inputs$SRErrorB^2)*rnorm(1, 0, inputs$SRErrorA)
          SRerror <- exp(logSRerror)
        }
        Cohort[1] <- AEQrecruits*SRerror/recruitsFromAgeOneFish
        totEsc[ERind,sim,year] <- sum(Escpmnt[2:5])
      }
    }
  }
  list(inputs=inputs, totEsc=totEsc)
  return(list(inputs=inputs, SummaryStats=SummaryStats, staticvars=staticvars, time=comp.time))
}