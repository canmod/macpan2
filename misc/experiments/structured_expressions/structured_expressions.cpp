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

// Operands and intermediate/final results are all generalized as matrix, i.e.,
//   scalar as 1x1 matrix
//   vector as mx1 matrix
//   matrix as mxn matrix
// so that we can unified the definition of function EvalExpr

// suppose we replace matrix<Type> with a struct abc
// 
//struct abc {
//    char type; // 0: scalar, 1: vector, 2: matrix
//    union {
//        Type scale;
//        vector<Type> vec;
//        matrix<Type> mat;
//    } data;
//};

template<class Type>
void Test(
    const matrix<Type>& scalar,
    const matrix<Type>& vec1,
    const matrix<Type>& vec2,
    const matrix<Type>& mat
)
{
    std::cout << "begin of test ===============" << std::endl;

    std::cout << "scalar = " << scalar << std::endl;
    std::cout << "vec1 = " << vec1 << std::endl;
    std::cout << "vec2 = " << vec2 << std::endl;
    std::cout << "mat = " << mat << std::endl;

    std::cout << "vec1.transpose() = " << vec1.transpose() << std::endl;

    // +,-,*, / between scalar and non-scaler (should be element-wise operations)
    // You have to convert non-scalar to Eigen::Array before the operation to enable elsement-wise operations
    std::cout << "mat - scalar = " << mat.array() - scalar.coeff(0,0) << std::endl;
    std::cout << "scalar - mat = " << scalar.coeff(0,0) - mat.array() << std::endl;

    std::cout << "vec1 - scalar = " << vec1.array() - scalar.coeff(0,0) << std::endl;
    std::cout << "scalar - vec1 = " << scalar.coeff(0,0) - vec1.array() << std::endl;

    std::cout << "mat * scalar = " << mat.array() * scalar.coeff(0,0) << std::endl;
    std::cout << "scalar * mat = " << scalar.coeff(0,0) * mat.array() << std::endl;

    std::cout << "vec1 * scalar = " << vec1.array() * scalar.coeff(0,0) << std::endl;
    std::cout << "scalar * vec1 = " << scalar.coeff(0,0) * vec1.array() << std::endl;

    // +,- between non-scalars with unmatched dimensions take the shape of the second operand.
    // The first operand is trimmed or expanded to the shape of the second. 
    // In the case of expansion, undefined elements are filled with zeros
    std::cout << "vec1 - vec2 = " << vec1 - vec2 << std::endl;
    std::cout << "vec2 + vec1 = " << vec2 + vec1 << std::endl;
    
    std::cout << "vec1 - mat = " << vec1 - mat << std::endl;
    std::cout << "mat + vec1.transpose() = " << mat + vec1.transpose() << std::endl;

    // *, / between non-scalars with unmatched dimensions (invalid)

    // *, / between non-scalars with matched dimensions 
    std::cout << "vec1 * vec1.transpose() = " << vec1 * vec1.transpose() << std::endl;
    std::cout << "vec1.transpose() * vec1 = " << vec1.transpose() * vec1 << std::endl;

    std::cout << "vec1 * vec2.transpose() = " << vec1 * vec2.transpose() << std::endl;
 
    std::cout << "end of test =================" << std::endl;
}

template<class Type>
matrix<Type> EvalExpr(
    const vector<int>& table_x,
    const vector<int>& table_n,
    const vector<int>& table_i,
    const vector<Type>& valid_vars,
    const vector<Type>& valid_literals,
    int row = 0
)
{
    // Just for testing
    matrix<Type> scalar = matrix<Type>::Zero(1,1);
    scalar.coeffRef(0,0) = 10;

    matrix<Type> vector1(3,1);
    for (int i=0; i<3; i++)
        vector1.coeffRef(i,0) = i + 1;

    matrix<Type> vector2(5,1);
    for (int i=0; i<5; i++)
        vector2.coeffRef(i,0) = i + 10;

    matrix<Type> mat(5,2);
    for (int i=0; i<5; i++) {
        mat.coeffRef(i,0) = i + 20;
        mat.coeffRef(i,1) = i + 25;
    }

    Test(scalar, vector1, vector2, mat);

    // 
    matrix<Type> m;
    Type sum;
    int rows, cols;

    switch (table_n[row]) {
        case -1:
            m = matrix<Type>::Zero(1,1);
            m.coeffRef(0,0) = valid_literals[table_x[row]-1];
            return m;
        case 0: // if we make valid_vars be a matrix in the future, what indexing should be used?
            m = matrix<Type>::Zero(1,1);
            m.coeffRef(0,0) = valid_vars[table_x[row]-1];
            return m;
        default:
            int n = table_n[row];
            vector<matrix<Type>> r(n);
            for (int i=0; i<n; i++)
                r[i] = EvalExpr(table_x, table_n, table_i, valid_vars, valid_literals, table_i[row]-1+i);

            switch(table_x[row]) {
                case 1: // +
                    #ifdef MP_VERBOSE
                        std::cout << r[0] << " + " << r[1] << " = " << r[0]+r[1] << std::endl << std::endl;
                    #endif
                    return r[0]+r[1];
                case 2: // -
                    #ifdef MP_VERBOSE
                        std::cout << r[0] << " - " << r[1] << " = " << r[0]-r[1] << std::endl << std::endl;
                    #endif
                    return r[0]-r[1];
                case 3: // *
                case 9: // %*%
                    #ifdef MP_VERBOSE
                        std::cout << r[0] << " * " << r[1] << " = " << r[0]*r[1] << std::endl << std::endl;
                    #endif
                    return r[0]*r[1];
                case 4: // /
                    #ifdef MP_VERBOSE
                        std::cout << r[0] << " / " << r[1] << " = " << r[0]/r[1].coeff(0,0) << std::endl << std::endl;
                    #endif
                    return r[0]/r[1].coeff(0,0);
                case 5: // ^
                    #ifdef MP_VERBOSE
                        std::cout << r[0] << " ^ " << r[1] << " = " << pow(r[0].array(), r[1].coeff(0,0)).matrix() << std::endl << std::endl;
                    #endif
                    return pow(r[0].array(), r[1].coeff(0,0)).matrix();
                    //return r[0].pow(r[1].coeff(0,0));
                case 6: // (
                    return r[0];
                case 7: // c
                    m = matrix<Type>::Zero(n,1);
                    for (int i=0; i<n; i++)
                        m.coeffRef(i,0) = r[i].coeff(0,0);
                    #ifdef MP_VERBOSE
                        std::cout << "c(" << r[0] << ", ...," << r[n-1] << ") = " << m << std::endl << std::endl;
                    #endif
                    return m;
                case 8: // matrix
                    m = r[0];

                    // Type->double or int
                    //Type sum;
                    //sum = 123.45;
                    //rows = sum; // not working
                    //rows = CppAD::Value(sum); // not working
                    //rows = CppAD::Value(CppAD::Var2Par(sum)); //not working

                    rows = CppAD::Integer(r[1].coeff(0,0));
                    cols = CppAD::Integer(r[2].coeff(0,0));

                    m.resize(rows, cols);
 
                   #ifdef MP_VERBOSE
                        std::cout << "matrix(" << r[0] << ") in shape of [" << rows << ", " << cols << "] = " \
                                  << m << std::endl << std::endl;
                    #endif
 
                    return m; 
		case 10: // sum
                    m = matrix<Type>::Zero(1,1);
                    sum = 0.0;
                    for (int i=0; i<n; i++)
                        sum += r[i].sum();
                    m.coeffRef(0,0) = sum;
                    
                    #ifdef MP_VERBOSE
                        std::cout << "sum(" << r[0] << ", ..., " << r[n-1] << ") = " << m << std::endl << std::endl;
                    #endif
                    return m;
                case 11: // rep
                    rows = CppAD::Integer(r[1].coeff(0,0));
                    m = matrix<Type>::Zero(rows,1);
                    for (int i=0; i<rows; i++)
                        m.coeffRef(i,0) = r[0].coeff(0,0);

                    #ifdef MP_VERBOSE
                        std::cout << "rep(" << r[0] << ", " << r[1] << ") = " << m << std::endl << std::endl;
                    #endif
                    return m;
                default:
                    Rf_error("invalid operator in arithmatic expression");
                    return m;
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
  matrix<Type> result = EvalExpr(parse_table_x, parse_table_n, parse_table_i, valid_vars, valid_literals);
  return result.coeff(0, 0);
  //return EvalExpr(parse_table_x, parse_table_n, parse_table_i, valid_vars, valid_literals);
}
