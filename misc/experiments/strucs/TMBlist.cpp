#include <TMB.hpp>
#include <iostream>
#include <string>

template<class Type>
Type objective_function<Type>::operator() ()
{

  std::cout << "a\n";

  SEXP MatrixList(getListElement(TMB_OBJECTIVE_PTR -> data, "MatrixList"));
  SEXP VectorList(getListElement(TMB_OBJECTIVE_PTR -> data, "VectorList"));
  PARAMETER(theta);

  std::cout << "b\n";
  // for (int i = 0; i<(int)Rf_length(MatrixList); i++) {
    matrix<Type> m(asMatrix<Type>(VECTOR_ELT(MatrixList, 0)));
    vector<Type> v(asVector<Type>(VECTOR_ELT(VectorList, 0)));

    std::cout << "c\n";
    //vector<Type> v2 = m * v * theta;
    Type yy = m.coeff(0, 0) * v[0] * theta;

    std::cout << "d\n";
    //}
  //Type yy = 0;
  return yy;
}
