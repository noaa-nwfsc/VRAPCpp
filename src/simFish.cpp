#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix simFish(int NRuns, int NYears, 
                      double targetER, 
                      bool MgmtError,
                      double GammaMgmtA, double GammaMgmtB,
                      String errorType,
                      double SRErrorA, double SRErrorB,
                      NumericVector CohortStart,
                      double prod, double cap,
                      NumericVector MatRate, NumericVector NatMort,
                      NumericVector PTU, NumericVector MatU,
                      NumericVector AEQ){
  
  NumericMatrix totEsc(NRuns, NYears);
  NumericVector Cohort(5);
  NumericVector newCohort(5);
  NumericVector PTUAdj(5);
  NumericVector MatUAdj(5); 
  NumericVector AEQmort(5);
  NumericVector Escpmnt(5);
  
  double lastAEQmort, HRscale, totAEQmort, totEscpmnt, adultEscapement, 
  AEQrecruits, ER, realizedER, ERerror, SRerror, logSRerror;
  bool converged;
  int numTrys;
  
  double recruitsFromAgeOneFish = (1-NatMort(0))*(1-NatMort(1))*AEQ(1);
  
  //Rcpp::Rcout << "targetER=" << targetER << "\n";
  
  for(int sim = 0; sim < NRuns; sim++) { // loop through simulations
    Cohort = clone(CohortStart); // initialize population. Use clone to create a deep copy (i.e do not just copy a pointer)
    logSRerror = rnorm(1, 0, SRErrorA)[0];
    for(int year = 0; year < NYears; year++) { // loop through years
      Cohort = Cohort*(1-NatMort);
      if(MgmtError) ER = std::min(targetER * rgamma(1, GammaMgmtA, GammaMgmtB)[0],1.0);
      else ER = targetER;
      //Rcpp::Rcout << ER << ",";
      if(ER==0){
        std::fill(PTUAdj.begin(), PTUAdj.end(), 0); // set all elements to 0
        std::fill(MatUAdj.begin(), MatUAdj.end(), 0);   // set all elements to 0
      }else{
        numTrys = 1;
        lastAEQmort = 99;
        converged = false;
        HRscale = 1;
        // Rcpp::Rcout <<"year=" << year << "Cohort=" << Cohort << "\n";
        while(!converged){
          // adjust preterminal and terminal fishing rates
          PTUAdj = pmin(PTU*HRscale,1); 
          MatUAdj = pmin(MatU*HRscale,1);
          
          // calculate AEQ fishing mortality, escapement, and the exploitation rate
          AEQmort = Cohort*(PTUAdj*AEQ + (1-PTUAdj)*MatRate*MatUAdj);
          Escpmnt = Cohort*(1-PTUAdj)*(1-MatUAdj)*MatRate;
          totAEQmort = sum(AEQmort);
          totEscpmnt = sum(Escpmnt);
          realizedER = totAEQmort/(totAEQmort+totEscpmnt);
          // calculate the error rate (how far the actual ER is from the target)
          // Rcpp::Rcout << "year=" << year << "ER=" << ER << "\n";
          ERerror = std::abs(ER-realizedER)/ER;  
          // exit loop if you are close enough OR other criteria are met
          
          //Rcpp::Rcout << "PTUAdj,MatUAdj=" << PTUAdj << "," << MatUAdj << "\n";
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
      newCohort = Cohort*(1-PTUAdj)*(1-MatRate);
      // Rcpp::Rcout << "newCohort=" << newCohort << "\n";
      Escpmnt = pmax(Cohort*(1-PTUAdj)*(1-MatUAdj)*MatRate,0);
      // calculate adult escapement
      adultEscapement = Escpmnt(2) + Escpmnt(3) + Escpmnt(4);
      // age the cohort
      for(int ageInd = 0; ageInd < 4; ageInd++) Cohort(ageInd+1) = newCohort(ageInd);
      // now fill in age 1 fish using the spawner-recruit function.
      AEQrecruits = prod * adultEscapement * exp(-adultEscapement / cap);
      if(errorType=="GAMMA"){
        SRerror = rgamma(1,SRErrorA,SRErrorB)[0];
      }else if(errorType=="LOGNORMAL"){
        // SRErrorA = lognormal sd, SRErrorB = autocorrelation
        logSRerror = SRErrorB*logSRerror + sqrt(1-pow(SRErrorB,2.0))*rnorm(1, 0, SRErrorA)[0];
        SRerror = exp(logSRerror);
      }
      Cohort(0) = AEQrecruits*SRerror/recruitsFromAgeOneFish;
      totEsc(sim,year) = Escpmnt(1) + Escpmnt(2) + Escpmnt(3) + Escpmnt(4);
    }
  }
  return totEsc;
}
