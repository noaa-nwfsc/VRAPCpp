runSimulationsC <- function(input,rngSeed=NULL){
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
    totEsc[ERind,,] <- simFish(numSims=input$numSims,numYears=input$numYears,targetER=targetER[ERind],
                               managementError=input$managementError, 
                               manageErrorA=input$manageErrorA,manageErrorB=input$manageErrorB,
                               errorType=input$errorType,
                               SRerrorA=input$SRerrorA,SRerrorB=input$SRerrorB,
                               initPop=input$initPop,prod=input$prod,cap=input$cap,
                               mat=input$mat,mort=input$mort,
                               HRpt=input$HRpt,HRt=input$HRt,AEQ=AEQ)
    
  }
  list(input=input, totEsc=totEsc)
}