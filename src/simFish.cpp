#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix simFish(int numSims, int numYears, 
                      double targetER, 
                      bool managementError,
                      double manageErrorA, double manageErrorB,
                      String errorType,
                      double SRerrorA, double SRerrorB,
                      NumericVector initPop,
                      double prod, double cap,
                      NumericVector mat, NumericVector mort,
                      NumericVector HRpt, NumericVector HRt,
                      NumericVector AEQ){
  
  NumericMatrix totEsc(numSims, numYears);
  NumericVector Cohort(5);
  NumericVector newCohort(5);
  NumericVector HRptAdj(5);
  NumericVector HRtAdj(5); 
  NumericVector AEQmort(5);
  NumericVector Escpmnt(5);
  
  double lastAEQmort, HRscale, totAEQmort, totEscpmnt, adultEscapement, 
  AEQrecruits, ER, realizedER, ERerror, SRerror, logSRerror;
  bool converged;
  int numTrys;
  
  double recruitsFromAgeOneFish = (1-mort(0))*(1-mort(1))*AEQ(1);
  
  //Rcpp::Rcout << "targetER=" << targetER << "\n";
  
  for(int sim = 0; sim < numSims; sim++) { // loop through simulations
    Cohort = clone(initPop); // initialize population. Use clone to create a deep copy (i.e do not just copy a pointer)
    logSRerror = rnorm(1, 0, SRerrorA)[0];
    for(int year = 0; year < numYears; year++) { // loop through years
      Cohort = Cohort*(1-mort);
      if(managementError) ER = std::min(targetER * rgamma(1, manageErrorA, manageErrorB)[0],1.0);
      else ER = targetER;
      //Rcpp::Rcout << ER << ",";
      if(ER==0){
        std::fill(HRptAdj.begin(), HRptAdj.end(), 0); // set all elements to 0
        std::fill(HRtAdj.begin(), HRtAdj.end(), 0);   // set all elements to 0
      }else{
        numTrys = 1;
        lastAEQmort = 99;
        converged = false;
        HRscale = 1;
        // Rcpp::Rcout <<"year=" << year << "Cohort=" << Cohort << "\n";
        while(!converged){
          // adjust preterminal and terminal fishing rates
          HRptAdj = pmin(HRpt*HRscale,1); 
          HRtAdj = pmin(HRt*HRscale,1);
          
          // calculate AEQ fishing mortality, escapement, and the exploitation rate
          AEQmort = Cohort*(HRptAdj*AEQ + (1-HRptAdj)*mat*HRtAdj);
          Escpmnt = Cohort*(1-HRptAdj)*(1-HRtAdj)*mat;
          totAEQmort = sum(AEQmort);
          totEscpmnt = sum(Escpmnt);
          realizedER = totAEQmort/(totAEQmort+totEscpmnt);
          // calculate the error rate (how far the actual ER is from the target)
          // Rcpp::Rcout << "year=" << year << "ER=" << ER << "\n";
          ERerror = std::abs(ER-realizedER)/ER;  
          // exit loop if you are close enough OR other criteria are met
          
          //Rcpp::Rcout << "HRptAdj,HRtAdj=" << HRptAdj << "," << HRtAdj << "\n";
          //Rcpp::Rcout << "numTrys=" << numTrys << "   HRscale=" << HRscale << "\n";
          //Rcpp::Rcout << "totAEQmort=" << totAEQmort << "   totEscpmnt=" << totEscpmnt << "\n";
          //Rcpp::Rcout << "ER=" << ER << "   realizedER=" << realizedER << "\n";
          //Rcpp::Rcout << "ERerror=" << ERerror << "\n";
          
          if((totAEQmort+totEscpmnt < 1) || (totAEQmort==0) || (numTrys > 100) || (totAEQmort==lastAEQmort)){
            converged = true;
          }else if(ERerror < 0.001){
            converged = true;
          }else{
            HRscale = HRscale*ER/realizedER;
          } 
          numTrys = numTrys+1;
          lastAEQmort = totAEQmort;
        } 
      }
      // calculate new cohort
      newCohort = Cohort*(1-HRptAdj)*(1-mat);
      // Rcpp::Rcout << "newCohort=" << newCohort << "\n";
      Escpmnt = pmax(Cohort*(1-HRptAdj)*(1-HRtAdj)*mat,0);
      // calculate adult escapement
      adultEscapement = Escpmnt(2) + Escpmnt(3) + Escpmnt(4);
      // age the cohort
      for(int ageInd = 0; ageInd < 4; ageInd++) Cohort(ageInd+1) = newCohort(ageInd);
      // now fill in age 1 fish using the spawner-recruit function.
      AEQrecruits = prod * adultEscapement * exp(-adultEscapement / cap);
      if(errorType=="gamma"){
        SRerror = rgamma(1,SRerrorA,SRerrorB)[0];
      }else if(errorType=="logNormal"){
        // SRerrorA = lognormal sd, SRerrorB = autocorrelation
        logSRerror = SRerrorB*logSRerror + sqrt(1-pow(SRerrorB,2.0))*rnorm(1, 0, SRerrorA)[0];
        SRerror = exp(logSRerror);
      }
      Cohort(0) = AEQrecruits*SRerror/recruitsFromAgeOneFish;
      totEsc(sim,year) = Escpmnt(1) + Escpmnt(2) + Escpmnt(3) + Escpmnt(4);
    }
  }
  return totEsc;
}
