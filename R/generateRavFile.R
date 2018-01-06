#' @title Write Rav File from input list
#' @description Takes the input list and write a .rav file. 
#' @details Takes the input list and write a .rav file that can be input into the VRAP function \link[VRAP]{Main}.
#' @param input A list of the necessary input values for \link{runSimulationsR} 
#' and \link{runSimulationsC}.
#' @param ravFileName Name of the .rav file that data will be written to.
#' @return Nothing. The data is written to ravFileName.

generateRavFile <- function(input, ravFileName="tmp.rav"){
  if(input$errorType != "gamma"){
    stop("ERROR: generateRavFile requires errorType = gamma")
  }
  ravText <- paste("Example, Title
                   1, Random seed; 0 gives random seed; numbers give fixed seed
                   1000, Number of runs
                   25, Number of years
                   2,5 , Minimum and maximum age (for now this is fixed; do not change)
                   0.001, Convergence criterion (% error) for target ER
                   YES, 0, -Inf, Center covariate flag and log MS and log Flow mean
                   Ric2, Spawner Recruit function (Ric2;Ric3;Ric4; Bev2;Bev3;Bev4; Hoc2;Hoc3;Hoc4)
                   ",input$prod,",",input$cap,", S/R a; b parameters; c (Marine); d (Freshwater)
                   Mean and CV  for marine survival index (M^c)
                   Trend; Cycle; or Autoc(orrelation) for Marine Survival?
                   Trend/Cycle parameters: rate for trend- amplitude- period & starting pt for cycle; correl for autocorrelation
                   Mean and CV  for flow (or other fw) index (exp(dF))
                   Trend; Cycle; or Autoc(orrelation) for Flow?
                   Trend/Cycle parameters: rate for trend- amplitude- period & starting pt for cycle; correl for autocorrelation
                   NO, Depensation? (YES or NO)
                   300, 63,1, 1) Esc. level for depensation to start 2) QET 3)% predicted return at QET (or for r/s=1 third parameter = 1)
                   YES, Determine recruits from adult spawners (not total)?
                   YES, Stock-recruit variation (YES or NO)
                   ",input$SRerrorA,",",input$SRerrorB,", 0, A and B parameters S/R error and error autocorrelation
                   NO, Smolt to adult survival w/variation (YES or NO);  if Yes beta variation on cohort size (2 parameters) on next line
                   Beta distribution a and b parameters and autocorrelation
                   0, Number of breakpoints; in escapement to trigger management action
                   1, Level to use as base regime
                   0.67, base exploitation rate
                   YES, Include error (YES or NO) in ER management; Norma Jean Sands: If no put zeros in cells A27 and B27
                   ",input$manageErrorA,",",input$manageErrorB,", Gamma parameters for management error
                   200, Lower escapement threshold
                   400, 5, Upper escapement threshold (MSY);  # yrs to ave.
                   ER, Step ER (ER) or  Pop Capacity (Pop)?
                   ",input$ERstepSize/0.67,",",", Buffer step size as percent of base ER or Pop capacity
                   ",input$ERstart/0.67,",",(input$ERstart+input$ERnumSteps*input$ERstepSize)/0.67,", Min & max buffer (x base for start & end)
                   ",input$initPop[1],", Initial population size at Age  1 
                   ",input$initPop[2],", Initial population size at Age  2 
                   ",input$initPop[3],", Initial population size at Age  3 
                   ",input$initPop[4],", Initial population size at Age  4 
                   ",input$initPop[5],", Initial population size at Age  5 
                   0.5, Age 1 natural mortality
                   0.4, Age 2 natural mortality
                   0.3, Age 3 natural mortality
                   0.2, Age 4 natural mortality
                   0.1, Age 5 natural mortality
                   ",input$mat[2],", Age 2 average maturation rate
                   ",input$mat[3],", Age 3 average maturation rate
                   ",input$mat[4],", Age 4 average maturation rate
                   ",input$mat[5],", Age 5 average maturation rate
                   ",input$HRpt[2],",",input$HRt[2],", Age 2 average mixed-maturity and mature fishery fishing rates
                   ",input$HRpt[3],",",input$HRt[3],", Age 3 average mixed-maturity and mature fishery fishing rates
                   ",input$HRpt[4],",",input$HRt[4],", Age 4 average mixed-maturity and mature fishery fishing rates
                   ",input$HRpt[5],",",input$HRt[5],", Age 5 average mixed-maturity and mature fishery fishing rates
                   endofinput, end of input indicator
                   ",sep="")
  
  cat(ravText, file=ravFileName)
}