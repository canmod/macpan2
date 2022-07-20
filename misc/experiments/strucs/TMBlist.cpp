#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  SEXP MatrixList(getListElement(TMB_OBJECTIVE_PTR -> data, "MatrixList"));
  SEXP VectorList(getListElement(TMB_OBJECTIVE_PTR -> data, "VectorList"));

  for (int i = 0; i<(int)Rf_length(MatrixList); i++) {
    matrix<Type> m(asMatrix<Type>(VECTOR_ELT(MatrixList, i)));
    vector<Type> v(asVector<Type>(VECTOR_ELT(VectorList, i)));
      
    vector<Type> v2 = m * v;
    
  }
  return 0;
}
