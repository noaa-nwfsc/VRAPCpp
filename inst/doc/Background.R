## ----setup, include=FALSE------------------------------------------------
require(here) #package to intelligently figure out project base directory
#require(devtools); install_github("eeholmes/VRAP")
require(VRAP) 
require(VRAPS)
knitr::opts_chunk$set(echo = TRUE)
vignetteFiles <- here("vignette_files")
if(!file.exists(vignetteFiles)) dir.create(vignetteFiles)
runSims <- TRUE # set this to true to run all simulations
clean <- FALSE # clean up the files made by VRAP 1.0

# note that using .at(i,j) for indexing in c++ is safer because it includes bounds checks.
## the following was necessary to get the c++ code to compile
Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")

## ------------------------------------------------------------------------
# initial calendar year population (ages 1 - 5)
CohortStart <- c(8000,4000,2000,700,250)

# harvest rates (these will evenutally be scaled)
MatU <- c(0,0.05,0.2,0.4,0.1)     # pre-termina or mixed-maturity FR
PTU <- c(0,0.03, 0.2, 0.3, 0.2) # terminal or mature FR

# maturation rates
MatRate <- c(0,0.01,0.15,0.7,1)

# natural mortality
NatMort <- c(0.5,0.4,0.3,0.2,0.1)

## ------------------------------------------------------------------------
AEQ = c(0,0,0,0,MatRate[5]) 
for(age in 4:2){
  AEQ[age] = MatRate[age] + (1-NatMort[age+1]) * (1 - MatRate[age]) * AEQ[age+1]
}

## ------------------------------------------------------------------------
# initialize cohort
Cohort <- CohortStart

### Code straight from the VRAP function CompEscpmnt.R ###
# COMPUTE PRETERMINAL MORTALITY AND UPDATE COHORT
PTMort = PTU * Cohort
TempCohort = Cohort - PTMort
# COMPUTE MATURE RUN AND UPDATE COHORT
MatRun = TempCohort * MatRate
TempCohort = TempCohort - MatRun
# COMPUTE MATURE MORTALITY AND ESCAPEMENT
MatMort = MatU * MatRun
Escpmnt = MatRun - MatMort #spawners
Escpmnt[Escpmnt < 1] = 0
# COMPUTE AEQ TOTAL MORTALITY EXPLOITATION RATE
AEQMort = AEQ * PTMort + MatMort
TotAEQMort = sum(AEQMort)
TotEscpmnt = sum(Escpmnt)
### End code ###

# exploitation rate
ER <- TotAEQMort/(TotAEQMort+TotEscpmnt)

## ------------------------------------------------------------------------
# we can simplify the code above to:
PTMort <- PTU * Cohort
MatMort <- MatU * Cohort * (1 - PTU) * MatRate 
AEQmort <- PTMort * AEQ + MatMort 
Escapmnt <- Cohort * (1 - PTU) * (1 - MatU) * MatRate 
ER <- sum(AEQmort)/(sum(AEQmort)+sum(Escpmnt))

# or even simpler:
AEQmort <- Cohort*(PTU*AEQ + (1-PTU)*MatRate*MatU)  # AEQMort
Escpmnt <- Cohort*(1-PTU)*(1-MatU)*MatRate          # Escapmnt
ER <- sum(AEQmort)/(sum(AEQmort)+sum(Escpmnt))   # Exploitation rate

## ----results="hide"------------------------------------------------------
targetER <- 0.4  
HRscale <- 1  # multiply this times the harvest rates.
repeat{
  # adjust preterminal and terminal fishing rates
  PTUAdj <- PTU*HRscale 
  MatUAdj <- MatU*HRscale 
  # calculate AEQ fishing mortality, escapement, and the exploitation rate
  AEQmort <- Cohort*(PTUAdj*AEQ + (1-PTUAdj)*MatRate*MatUAdj) 
  Escpmnt <- Cohort*(1-PTUAdj)*(1-MatUAdj)*MatRate 
  ER <- sum(AEQmort)/(sum(AEQmort)+sum(Escpmnt))
  # calculate the error rate (how far the actual ER is from the target)
  ERerror <- abs(ER-targetER)/targetER  
  # print the results
  cat(paste("actual ER = ",round(ER,3),",  goal = ",targetER,",  abs(actual-target)/target = ",round(ERerror,3),"\n",sep=""))
  # exit loop if you are close enough
  if(ERerror < 0.001) break else HRscale <- HRscale*targetER/ER
}


## ------------------------------------------------------------------------
inputs <- list(
  RanSeed = 0,
  ConvergeCrit = 0.001,
  SRType = "Ric2",
  depen = "NO",
  EscChoice = "YES",
  
  # define the productivity and capacity parameters for the spawner-recruit function
  prod = 2,
  cap = 1000,
  
  # maturation rates
  MatRate = c(0,0.01,0.15,0.7,1),

  # natural mortality
  NatMort = c(0.5,0.4,0.3,0.2,0.1),
  
  # harvest rates (these will eventually be scaled)
  MatU = c(0,0.05,0.2,0.4,0.1),     # terminal or mature FR
  PTU = c(0,0.03, 0.2, 0.3, 0.2), # pre-terminal or mixed-maturity FR

  # initial popultion
  CohortStart = c(8000,4000,2000,700,250),

  # the type of distribution used for the recruitment residuals (gamma or logNormal)
  errorType = "GAMMA",
  
  # If errorType == "GAMMA" SRErrorA and SRerror B are 
  #   - the shape and scale parameters of the gamma distribution.
  # If errorType == "logNormal" they are the
  #   - log of the standard deviation and lag 1 autocorrelation of a normal distribution with mean zero
  #     that is then exponentiated.
  SRErrorA = 5.7749,
  SRErrorB = 0.1875,
  
  # set management error gamma parameters
  MgmtError = TRUE,
  GammaMgmtA = 100,
  GammaMgmtB = 1/100,

  # set escapement thresholds
  ECrit = 200,
  ERecovery = 400,
  EndAv = 5, #years to average
  
  # parametere for setting exploitation rate start, stop and steps
  StepFunc = "ER",
  StepStart = 0,
  StepEnd = 0.8,
  StepSize = 0.02, # stepsize in terms of the exploitation rate, baseER*stepSize  
    
  NYears = 25, # years in each forward projectiong
  NRuns = 1000 # the number of times the simulation should be repeated.
)
inputs$StepNum = round((inputs$StepEnd - inputs$StepStart) / inputs$StepSize + 1) # number of ER targets


## ----eval=FALSE----------------------------------------------------------
#  library(VRAPS)
#  RunSims2R

## ----results="hide"------------------------------------------------------
setwd(vignetteFiles)
# Only run if var runSims is TRUE, otherwise use saved results.
if(runSims){
  results <- RunSims2R(inputs)
  save(results,file="results.Rdat")
}else{
  load("results.Rdat")
} 

## ----eval=FALSE----------------------------------------------------------
#  library(VRAPS)
#  WriteRavFile2

## ----message=FALSE-------------------------------------------------------
setwd(vignetteFiles)
WriteRavFile2(inputs)

# Either use the VRAP Main function to run VRAP (twice) and then save the results, or load the saved results. 
if(runSims){
  library(VRAP)
  vrapOut1 <- Main(InFile="tmp.rav", OutFileBase="vrapOut.tmp", NRuns=1000, silent=TRUE, lcores=4)
  save(vrapOut1,file="vrapOut1.Rdat")
  vrapOut2 <- Main(InFile="tmp.rav", OutFileBase="vrapOut.tmp", NRuns=1000, silent=TRUE, lcores=4)
  save(vrapOut2,file="vrapOut2.Rdat")
}else{
  load("vrapOut1.Rdat")
  load("vrapOut2.Rdat")
}

## ----echo=FALSE----------------------------------------------------------
# first create functions to make the plots
plotSimAvg <- function(simDat,vrapOut){
  avgs <- apply(simDat[,,],c(1,3),mean)
  plot(1,1,xlim=c(1,25),ylim=range(c(vrapOut$SummaryStats$AvgEscpmnt,avgs)),xlab="Year", ylab="Average escapement", type="n", bty="l")
  for(i in 1:inputs$StepNum){
    lines(1:25,vrapOut$SummaryStats$AvgEscpmnt[i,])
    lines(1:25,avgs[i,],lty=2)
  }
  legend(x=15,y=max(avgs),legend=c("VRAP","Simulations above"),lty=c(1,2))
}

compPlot<- function(vrapOut1,vrapOut2){
  plot(1,1,xlim=c(1,25),ylim=range(c(vrapOut1$SummaryStats$AvgEscpmnt,vrapOut2$SummaryStats$AvgEscpmnt)),xlab="Year", ylab="Average escapement", type="n", bty="l")
  for(i in 1:inputs$StepNum){
    lines(1:25,vrapOut1$SummaryStats$AvgEscpmnt[i,])
    lines(1:25,vrapOut2$SummaryStats$AvgEscpmnt[i,],lty=2)
  }
  legend(x=15,y=max(vrapOut1$SummaryStats$AvgEscpmnt),legend=c("VRAP 1","VRAP 2"),lty=c(1,2))
}

# then use the functions to create the plots
plotSimAvg(results$totEsc,vrapOut1)
compPlot(vrapOut1,vrapOut2)

## ----echo=FALSE----------------------------------------------------------
bEcrit <- apply(results$totEsc,1,function(x) mean(x<200))

plot(1,1,xlim=c(0,0.8),ylim=c(0,1),xlab="Year",ylab="% years below critical Esc threshold",type="n",bty="l")
targetER <- inputs$StepStart + inputs$StepSize * (0:(inputs$StepNum-1))
lines(targetER,bEcrit)
lines(targetER,vrapOut1$SummaryStats$AvgECrit,lty=3)
lines(targetER,vrapOut2$SummaryStats$AvgECrit,lty=3)
legend(x=0,y=0.8,legend=c("Simulations above","VRAP (2 sims)"),lty=c(1,3))

## ----echo=FALSE----------------------------------------------------------
n <- inputs$NYears
meanVals <- apply(results$totEsc,c(1,2),function(x) exp(mean(log(x[(n-4):n]))))
aRcrit <- apply(meanVals,1,function(x) mean(x>=400))

plot(1,1,xlim=c(0,0.8),ylim=c(0,1),xlab="Year",ylab="% years above rebuilding esc threshold",type="n",bty="l")
targetER <- inputs$StepStart + inputs$StepSize * (0:(inputs$StepNum-1))
lines(targetER,aRcrit)
lines(targetER,vrapOut1$SummaryStats$PropRec,lty=3)
lines(targetER,vrapOut2$SummaryStats$PropRec,lty=3)
legend(x=0.5,y=0.8,legend=c("Simulations above","VRAP (2 sims)"),lty=c(1,3))

## ------------------------------------------------------------------------
# calculate AEQ
AEQ <- c(0,0,0,0,inputs$MatRate[5]) 
for(age in 4:2){
  AEQ[age] <- inputs$MatRate[age] + (1-inputs$NatMort[age+1]) * (1 - inputs$MatRate[age]) * AEQ[age+1]
}

# run the simulation
xx <- simFish(NRuns=inputs$NRuns,NYears=inputs$NYears,targetER=0.2,
              MgmtError=inputs$MgmtError, 
              GammaMgmtA=inputs$GammaMgmtA,GammaMgmtB=inputs$GammaMgmtB,
              errorType=inputs$errorType,
              SRErrorA=inputs$SRErrorA,SRErrorB=inputs$SRErrorB,
              CohortStart=inputs$CohortStart,prod=inputs$prod,cap=inputs$cap,
              MatRate=inputs$MatRate,NatMort=inputs$NatMort,
              PTU=inputs$PTU,MatU=inputs$MatU,AEQ=AEQ)

## ---- eval=FALSE---------------------------------------------------------
#  library(VRAPS)
#  RunSims2C

## ----results="hide"------------------------------------------------------
cResults <- RunSims2C(inputs)

## ------------------------------------------------------------------------
plotSimAvg(cResults$totEsc,vrapOut1)

## ------------------------------------------------------------------------
bEcrit <- apply(cResults$totEsc,1,function(x) mean(x<200))

plot(1,1,xlim=c(0,0.8),ylim=c(0,1),xlab="Year",ylab="% years below critical Esc threshold",type="n",bty="l")
targetER <- inputs$StepStart + inputs$StepSize * (0:(inputs$StepNum-1))
lines(targetER,bEcrit)
lines(targetER,vrapOut1$SummaryStats$AvgECrit,lty=3)
lines(targetER,vrapOut2$SummaryStats$AvgECrit,lty=3)
legend(x=0,y=0.8,legend=c("Simulations above","VRAP (2 sims)"),lty=c(1,3))

## ----results="hide"------------------------------------------------------
t1 <- Sys.time()
cResults <- RunSims2C(inputs)
t2 <- Sys.time()
rResults <- RunSims2R(inputs)
t3 <- Sys.time()

## ----results="hold", echo=FALSE------------------------------------------
cat(paste("c-version ",format(t2-t1),"\n",sep=""))
cat(paste("r-version ",format(t3-t2),"\n",sep=""))
tRat <- round(100*as.numeric(difftime(t2,t1,units="secs"))/as.numeric(difftime(t3,t2,units="secs")),1)
cat(paste("The c-version takes ",tRat,"% as much time as the r-version\n",sep=""))
cat(paste("Or is ",round(100/tRat)," times faster.\n",sep=""))

## ------------------------------------------------------------------------
inputs2 <- inputs
inputs2$errorType <- "LOGNORMAL"
inputs2$SRErrorA <- 0.5   # lognormal stdev = 0.5
inputs2$SRErrorB <- 0.75  # autocorrelation = 0.75

## ----results="hide"------------------------------------------------------
resultsLNr <- RunSims2R(inputs2)
resultsLNc <- RunSims2C(inputs2)

## ----echo=FALSE----------------------------------------------------------
bEcritR <- apply(resultsLNr$totEsc,1,function(x) mean(x<200))
bEcritC <- apply(resultsLNc$totEsc,1,function(x) mean(x<200))

plot(1,1,xlim=c(0,0.8),ylim=c(0,1),xlab="ER Target",ylab="% years below critical Esc threshold",type="n",bty="l")
targetER <- inputs2$StepStart + inputs2$StepSize * (0:(inputs2$StepNum-1))
lines(targetER,bEcritR)
lines(targetER,bEcritC,lty=3)
legend(x=0,y=0.8,legend=c("R function","C++ function"),lty=c(1,3))

## ----results="hide"------------------------------------------------------
inputs3 <- inputs2
inputs3$SRErrorB <- 0
resultsLNc2 <- RunSims2C(inputs3)

## ----fig.width=7, fig.height=7, echo=FALSE-------------------------------
par(mfrow=c(3,1),mar=c(1,1,1,1),oma=c(5,4,5,1))
plot(1:25,resultsLNc$totEsc[1,1,],type="o",pch=16,xlab="",ylab="")
plot(1:25,resultsLNc$totEsc[2,1,],type="o",pch=16,xlab="",ylab="")
plot(1:25,resultsLNc$totEsc[3,1,],type="o",pch=16,xlab="",ylab="")
mtext(side=1,outer=TRUE,text="Year",line=1)
mtext(side=2,outer=TRUE,text="Total escapement",line=1)
mtext(side=3,outer=TRUE,text="Autocor=0.75",line=1)

## ----fig.width=7, fig.height=7, echo=FALSE-------------------------------
par(mfrow=c(3,1),mar=c(1,1,1,1),oma=c(5,4,5,1))
plot(1:25,resultsLNc2$totEsc[1,1,],type="o",pch=16,xlab="",ylab="")
plot(1:25,resultsLNc2$totEsc[2,1,],type="o",pch=16,xlab="",ylab="")
plot(1:25,resultsLNc2$totEsc[3,1,],type="o",pch=16,xlab="",ylab="")
mtext(side=1,outer=TRUE,text="Year",line=1)
mtext(side=2,outer=TRUE,text="Total escapement",line=1)
mtext(side=3,outer=TRUE,text="Autocor=0",line=1)

## ----echo=FALSE----------------------------------------------------------
bEcritC <- apply(resultsLNc$totEsc,1,function(x) mean(x<inputs$ECrit))
bEcritC2 <- apply(resultsLNc2$totEsc,1,function(x) mean(x<inputs$ECrit))

plot(1,1,xlim=c(0,0.8),ylim=c(0,1),xlab="ER target",ylab="% years below critical Esc threshold",type="n",bty="l")
targetER <- inputs2$StepStart + inputs2$StepSize * (0:(inputs2$StepNum-1))
lines(targetER,bEcritC)
lines(targetER,bEcritC2,lty=3)
legend(x=0,y=0.8,legend=c("autocor=0.75","autocor=0"),lty=c(1,3))

## ----echo=FALSE----------------------------------------------------------
if(clean){
  file.remove("tmp.rav","tmprav.rav","results.Rdat","vrapOut.tmp.esc","vrapOut.tmp.byr","vrapOut.tmp.sum","vrapOut1.Rdat","vrapOut2.Rdat")
}

