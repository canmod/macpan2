// Simple linear regression.
#include <Eigen/Eigen>
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <math.h> 	// isnan() is defined
#include <sys/time.h>
#include <TMB.hpp>
#include <cppad/local/cond_exp.hpp>


template<class Type>
struct ll {
  vector<Type> Y;
  vector<Type> x;
  ll(SEXP ii){ // Constructor
    Y = asVector<Type>(getListElement(ii,"Y"));
    x = asVector<Type>(getListElement(ii,"x"));
  }
};

// struct numeric_vector_indexer {
//
// };

template<class Type>
Type objective_function<Type>::operator() ()
{

  //SEXP MatrixList = getListElement(TMB_OBJECTIVE_PTR -> data, "MatrixList")

  //DATA_VECTOR(Y);
  //DATA_VECTOR(x);
  DATA_STRUCT(Yx, ll);
  //DATA_STRUCT(numeric_vectors, numeric_vector_indexer);

  PARAMETER(a);
  PARAMETER(b);
  PARAMETER(logSigma);
  ADREPORT(exp(2*logSigma));
  Type nll = -sum(dnorm(Yx.Y, a+b*Yx.x, exp(logSigma), true));
  return nll;
}
