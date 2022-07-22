#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
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
Type EvalExpr(
    const vector<int>& table_x,
    const vector<int>& table_n,
    const vector<int>& table_i,
    const vector<Type>& valid_vars,
    const vector<Type>& valid_literals,
    int row = 0
)
{
    switch (table_n[row]) {
        case -1:
            return valid_literals[table_x[row]-1];
        case 0: // if we make valid_vars be a matrix in the future, what indexing should be used?
            return valid_vars[table_x[row]-1];
        default:
            int n = table_n[row];
            vector<Type> r(n);
            for (int i=0; i<n; i++)
                r[i] = EvalExpr(table_x, table_n, table_i, valid_vars, valid_literals, table_i[row]-1+i);

            switch(table_x[row]) {
                case 1: // +
                    return r[0]+r[1];
                case 2: // -
                    return r[0]-r[1];
                case 3: // *
                    return r[0]*r[1];
                case 4: // /
                    return r[0]/r[1];
                case 5: // ^
                    return pow(r[0], r[1]);
                case 6: // (
                    return r[0];
                // below is an example for possible extension in the future
		//case 7: // averaging
                //    Type sum = 0.0;
                //    for (i=0; i<n; i++)
                //        sum += r[i];
                //    return sum;
                default:
                    Rf_error("invalid operator in arithmatic expression");
                    return 0.0;
            }
    }
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_IVECTOR(parse_table_x);
  DATA_IVECTOR(parse_table_n);
  DATA_IVECTOR(parse_table_i);
  DATA_VECTOR(valid_literals);
  PARAMETER_VECTOR(valid_vars);
  return EvalExpr(parse_table_x, parse_table_n, parse_table_i, valid_vars, valid_literals);
}
