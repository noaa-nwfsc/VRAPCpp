runSimulationsR <- function(input,rngSeed=NULL,verbose=FALSE){
  # initialize output array
  totEsc <- array(NA,dim=c(input$ERnumSteps+1,input$numSims,input$numYears))
  HRscale <- 1 # multiply this times the harvest rates.
  # calcualte the target exploitation rates based on start, step size, and steps
  targetER <- input$ERstart + input$ERstepSize * (0:input$ERnumSteps)
  # calculate AEQ and recruitsFromAgeOneFish
  AEQ <- c(0,0,0,0,input$mat[5]) 
  for(age in 4:2){
    AEQ[age] <- input$mat[age] + (1-input$mort[age+1]) * (1 - input$mat[age]) * AEQ[age+1]
  }
  recruitsFromAgeOneFish <- (1-input$mort[1])*(1-input$mort[2])*AEQ[2]
  
  for(ERind in 1:(input$ERnumSteps+1)){
    print(paste("============= target ER =",targetER[ERind]))
    for(sim in 1:input$numSims){ # loop through 1000 25 yr simulations
      logSRerror <- rnorm(1, 0, sd=input$SRerrorB) # not currently used
      Cohort <- input$initPop # initialize population
      for(year in 1:25){ # loop through 25 year simulation
        # apply natural mortality
        Cohort <- Cohort*(1-input$mort)
        # generate management error
        actualER <- targetER[ERind]
        if(input$managementError) 
          actualER <- min(actualER * rgamma(1, input$manageErrorA, scale=input$manageErrorB),1)
        # loop to achieve target exploitation rate unless targetER=0
        if(actualER==0){
          HRptAdj <- 0
          HRtAdj <- 0
        }else{
          numTrys <- 1
          lastAEQmort <- 99
          repeat{
            # adjust preterminal and terminal fishing rates
            HRptAdj <- input$HRpt*HRscale 
            HRtAdj <- input$HRt*HRscale
            # can't be larger than 1
            HRptAdj[HRptAdj>1] <- 1
            HRtAdj[HRtAdj>1] <- 1
            # calculate AEQ fishing mortality, escapement, and the exploitation rate
            AEQmort <- Cohort*(HRptAdj*AEQ + (1-HRptAdj)*input$mat*HRtAdj) 
            Escpmnt <- Cohort*(1-HRptAdj)*(1-HRtAdj)*input$mat
            totAEQmort <- sum(AEQmort)
            totEscpmnt <- sum(Escpmnt)
            ER <- totAEQmort/(totAEQmort+totEscpmnt)
            # calculate the error rate (how far the actual ER is from the target)
            ERerror <- abs(ER-actualER)/actualER  
            # exit loop if you are close enough OR other criteria are met. Otherwise adjust HRscale.
            if(totAEQmort+totEscpmnt < 1 | totAEQmort==0 | numTrys > 100 | totAEQmort==lastAEQmort){
              if(verbose){
                cat(paste("Target ER = ",targetER[ERind],"  Sim = ",sim,"  Year = ",year,
                          "  goal - actual = ",round(actualER,3)," - ",round(ER,3),
                          "  HRscale = ",round(HRscale,3),"  numTrys = ",numTrys,
                          "  totEsc = ",round(totEscpmnt,1),"  totAEQmort = ",round(totAEQmort,1),"\n",sep=""))
              }
              break
            }else if(ERerror < 0.001) break
            else HRscale <- HRscale*actualER/ER
            numTrys <- numTrys+1
            lastAEQmort <- totAEQmort
          } 
        }
        # calculate new cohort
        newCohort <- Cohort*(1-HRptAdj)*(1-input$mat)
        Escpmnt <- Cohort*(1-HRptAdj)*(1-HRtAdj)*input$mat
        Escpmnt[Escpmnt < 1] <- 0
        # calculate adult escapement
        adultEscapement <- sum(Escpmnt[3:5])
        # age the cohort
        Cohort[2:5] <- newCohort[1:4]
        # now fill in age 1 fish using the spawner-recruit function.
        AEQrecruits <- input$prod * adultEscapement * exp(-adultEscapement / input$cap)
        if(input$errorType=="gamma"){
          SRerror <- rgamma(1,input$SRerrorA,scale=input$SRerrorB)
        }else if(input$errorType=="logNormal"){
          # SRerrorA = lognormal sd, SRerrorB = autocorrelation
          logSRerror <- input$SRerrorB*logSRerror + sqrt(1-input$SRerrorB^2)*rnorm(1, 0, input$SRerrorA)
          SRerror <- exp(logSRerror)
        }
        Cohort[1] <- AEQrecruits*SRerror/recruitsFromAgeOneFish
        totEsc[ERind,sim,year] <- sum(Escpmnt[2:5])
      }
    }
  }
  list(input=input, totEsc=totEsc)
}