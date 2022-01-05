#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double gamma_r_cpp(NumericVector pars, int r, NumericVector parGrp) {
  
  int k = pars.size();
  
  if (r == 0) { return 1;
  } else if (r > k || r < 0){ return 0;
  } else if (r != 0 || r <= k) {
                                  NumericVector pars1 = pars[(parGrp==1)];
                                  int n = pars1.size();
    
                                  NumericVector parsn1 = pars[(parGrp!=1)];
    
                                  NumericVector parGrpn1 = parGrp[(parGrp!=1)] ;
                                  parGrpn1 = parGrpn1 - 1;
    
                                  double p = gamma_r_cpp( parsn1,  r , parGrpn1 );
    
                                  for(int i = 0; i < n; ++i) {
      
                                          p += exp(pars1[i]) * gamma_r_cpp(parsn1,  r-(i+1), parGrpn1 );
      
                                  }
    
                                  return p ;}
}
  