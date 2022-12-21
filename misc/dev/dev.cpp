#define MP_VERBOSE
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <Eigen/Eigen>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <math.h> 	// isnan() is defined
#include <sys/time.h>
#include <TMB.hpp>
#include <cppad/local/cond_exp.hpp>

////////////////////////////////////////////////////////////////////////////////
// Macpan2 is redesigned architecture. The spec is
// https://canmod.net/misc/cpp_side.html
//
// Operands in a math expression and its intermediate/final results are all
// generalized as matrices, i.e.,
//   scalar as 1x1 matrix
//   vector as mx1 or 1xn matrix
//   matrix as mxn matrix
// so that we can unified the definition of function EvalExpr. The Eigen library
// (https://eigen.tuxfamily.org/dox/group__TutorialMatrixClass.html) has similar
// generalization idea.
//
// Elementwise Binary Operators (+ - * / ^)
//   + if both operands have the same shape, then do the elementwise operation
//   + elif one of the operands is a scalar, then do the operation of scalar vs
//     elements of the other operand
//   + elif one of the operands is a vector, then
//     - if the length of the vector is equal to the corresponding dimension of the
//       other operand, then do row- or column-wise element-vs-element operation.
//     - else ERROR
//   + else ERROR
//
// Matrix Binary operations (%*%)
//   + if the second dimension of first operand == the first dimension of
//     the second operand, then do matrix multiplication
//   + else ERROR
//   NOTE: 1 Inner product can be made by row_vec %*% col_vec while outer product
//         can be made by col_vec %*% row_vec.
//         2 We need to add "transpose" and "flatten" operators
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Functions we Support
// Please follow exactly the format below when adding a new function:
//   MP2_CPP_CASE_NAME = {case number}, // {R function name}
// Note that the last entry does not have the comma that follows
// {case number}.
// If an entry in this enum does not have an associated case in the
// switch(table_x[row]+1) statement, then it _must_ be commented out.
// An analogous R-side list can be automatically produced using the
// package makefile. The ordering of the functions must follow their
// associated integers, and the integers must increase from 1 with no
// gaps. Therefore, you should add any new functions at the end,
// although it is possible to change the order of the functions as
// long as the integers increase from 1 without gaps as long as the
// R/enum.R file is regenerated.
enum macpan2_func {
    MP2_ADD = 1, // +
    MP2_SUBTRACT = 2, // -
    MP2_MULTIPLY = 3, // *
    MP2_DIVIDE = 4, // /
    MP2_POWER = 5, // ^
    MP2_EXP = 6, // exp
    MP2_LOG = 7, // log
    MP2_ROUND_BRACKET = 8, // (
    MP2_COMBINE = 9, // c
    MP2_MATRIX = 10, // matrix
    MP2_MATRIX_MULTIPLY = 11, // %*%
    MP2_SUM = 12, // sum
    MP2_REPLICATE = 13, // rep
    MP2_ROWSUMS = 14, // rowSums
    MP2_COLSUMS = 15, // colSums
    MP2_GROUPSUMS = 16, // groupSums
    MP2_SQUARE_BRACKET = 17, // [
    MP2_TRANSPOSE = 18, // t
    MP2_RBIND_TIME = 19, // rbind_time
    MP2_RBIND_LAG = 20, // rbind_lag
    MP2_CBIND_TIME = 21, // cbind_time
    MP2_CBIND_LAG = 22, // cbind_lag
    MP2_COLON = 23, // :
    MP2_SEQUENCE = 24, // seq
    MP2_CONVOLUTION = 25, // convolution
    MP2_CBIND = 26, // cbind
    MP2_RBIND = 27, // rbind
    MP2_TIME_STEP = 28, // time_step
    MP2_ASSIGN = 29, // assign
    MP2_CLAMP = 30, // clamp
    MP2_POISSON_DENSITY = 31, // dpois
    MP2_NORMAL_DENSITY = 32, // dnorm
    MP2_POISSON_SIM = 33, // rpois
    MP2_NORMAL_SIM = 34 // rnorm
    // MP2_NEGBIN_DENSITY = 30, // dnbinom
};

// Helper function
template<class Type>
bool RecycleInPlace(
    matrix<Type>& mat,
    int rows,
    int cols
) {
    if (mat.rows()==rows && mat.cols()==cols) // don't need to do anything.
        return true;

    matrix<Type> m(rows, cols);
    if (mat.rows()==rows) {
        if (mat.cols()==1)
            for (int i=0; i<cols; i++)
                m.col(i) = mat.col(0);
        else
            return false;
    }
    else if (mat.cols()==cols) {
        if (mat.rows()==1)
            for (int i=0; i<rows; i++)
                m.row(i) = mat.row(0);
        else
            return false;
    }
    else
        return false;

    // final step
    mat = m;
    return true;
}


template<class Type>
struct ListOfMatrices {
    // below is a vector of matrices that passed from R
    vector<matrix<Type> > m_matrices;

    ListOfMatrices(SEXP ii){ // Constructor
        // Get elements by their indices
        int n = length(ii);
        vector<matrix<Type> > vs(n);
        m_matrices = vs;

        for (int i = 0; i < n; i++) {
            m_matrices[i] = asMatrix<Type>(VECTOR_ELT(ii, i));
        }
    }

    ListOfMatrices() { // Default Constructor
    }

    // Copy constructor
    ListOfMatrices(const ListOfMatrices& another) {
        m_matrices = another.m_matrices;
    }

    // Overload assign operator
    ListOfMatrices & operator=(const ListOfMatrices& another) {
        m_matrices = another.m_matrices;
        return *this;
    }
};

template<class Type>
class ExprEvaluator {
public:
    ExprEvaluator() {
        error_code = 0;	// non-zero means error has occurred; otherwise, no error
        strcpy(error_message, "OK");
    };

    unsigned char GetErrorCode() { return error_code; };
    const char* GetErrorMessage() { return error_message; };

    void SetError(unsigned char code, const char* message)
    {
        error_code = code;
        strcpy(error_message, message);
        std::cout << "MACPAN ERROR #" << (int) code << ": " << message << std::endl;
    };

    matrix<Type> EvalExpr(
        const vector<ListOfMatrices<Type> >& hist,
        int t,
        const vector<int>& mats_save_hist,
        const vector<int>& table_x,
        const vector<int>& table_n,
        const vector<int>& table_i,
        ListOfMatrices<Type>& valid_vars,
        const vector<Type>& valid_literals,
        int row = 0
    )
    {
        matrix<Type> m, m1, m2, step, sim;
        Type sum, s, eps;
        int rows, cols, rowIndex, colIndex, matIndex;

        if (GetErrorCode()) return m; // Check if error has already happened at some point of the recursive call.

        switch (table_n[row]) {
            case -1: // literals
                m = matrix<Type>::Zero(1,1);
                m.coeffRef(0,0) = valid_literals[table_x[row]];
                return m;
            case 0:
                m = valid_vars.m_matrices[table_x[row]];
                return m;
            default:
                int n = table_n[row];
                vector<matrix<Type> > r(n);
                vector<int> index2mats(n);
                for (int i=0; i<n; i++) {
                    r[i] = EvalExpr(hist, t, mats_save_hist, table_x, table_n, table_i, \
                                    valid_vars, valid_literals, table_i[row]+i);
                    index2mats[i] = table_x[table_i[row]+i];
                    if (GetErrorCode()) return m;
                }

                // Check dimensions compatibility. If needed, expand one operand to make its dimensions compatible with the other
                if (table_x[row]+1<6 && table_n[row]==2) { // elementwise binary operations + - * / ^
                    if (r[0].rows()==r[1].rows()) {
                        if (r[0].cols()!=r[1].cols()) {
                            if (r[0].cols()==1) { // vector vs matrix or scalar vs vector
                                m = r[0];
                                r[0] = r[1]; // for the shape
                                for (int i=0; i<r[0].cols(); i++)
                                    r[0].col(i) = m.col(0);
                            }
                            else if (r[1].cols()==1) { // vector vs matrix or scalar vs vector
                                m = r[1];
                                r[1] = r[0]; // for the shape
                                for (int i=0; i<r[1].cols(); i++)
                                    r[1].col(i) = m.col(0);
                            }
                            else {
                                SetError(201, "The two operands do not have the same number of columns");
                                return m;
                                //Rf_error("The two operands do not have the same number of columns");
                            }
                        }
                        // else: do nothing
                    }
                    else {
                        if (r[0].cols()==r[1].cols()) { // only one compatible dimension
                            if (r[0].rows()==1) { // vector vs matrix or scalar vs vector
                                m = r[0];
                                r[0] = r[1]; // for the shape
                                for (int i=0; i<r[0].rows(); i++)
                                    r[0].row(i) = m.row(0);
                            }
                            else if (r[1].rows()==1) { // vector vs matrix or scalar vs vector
                                m = r[1];
                                r[1] = r[0]; // for the shape
                                for (int i=0; i<r[1].rows(); i++)
                                    r[1].row(i) = m.row(0);
                            }
                            else {
                                SetError(202, "The two operands do not have the same number of rows");
                                return m;
                                // Rf_error("The two operands do not have the same number of rows");
                            }
                        }
                        else { // no dimensions are equal
                            if (r[0].rows()==1 && r[0].cols()==1) { // scalar vs non-scalar
                                s = r[0].coeff(0,0);
                                r[0] = r[1];
                                r[0].setConstant(s);
                            }
                            else if (r[1].rows()==1 && r[1].cols()==1) { // scalar vs non-scalar
                                s = r[1].coeff(0,0);
                                r[1] = r[0];
                                r[1].setConstant(s);
                            }
                            else {
                                SetError(203, "The two operands do not have the same number of columns or rows");
                                return m;
                                //Rf_error("The dimensions of the two operands are not equal to each other");
                            }
                        }
                    }
                }
                else if (table_x[row]+1==9) { // %*% matrix multiplication
                    if (r[0].cols()!=r[1].rows()) {
                        SetError(204, "The two operands are not compatible to do matrix multiplication");
                        return m;
                        //Rf_error("The two operands are not compatible to do matrix multiplication");
                    }
                }

                if (GetErrorCode()) return m; // early return

                // #' Engine Functions
                // #'
                // #' Functions currently supported by the C++ TMB engine
                // #' for constructing expressions for defining model
                // #' simulations.
                // #'
                switch(table_x[row]+1) {

                    // #' ## Elementwise Binary Operators
                    // #'
                    // #' Elementwise binary operators take two matrix-valued
                    // #' arguments and apply a binary operator (e.g. `+`, `*`)
                    // #' to each set of corresponding elements, and return the
                    // #' corresponding matrix-valued output containing the
                    // #' resulting elements. What does 'corresponding' mean? If
                    // #' the two matrix-valued arguments have the same shape
                    // #' (same number of rows and columns), then two elements
                    // #' correspond if they occur in the same row and column
                    // #' position in the two matrices. If the two matrices are
                    // #' not of the same shape but there is either one row or
                    // #' one column in either matrix, then the singleton rows
                    // #' and then columns are repeated sufficiently many times
                    // #' so that they match the shape of the other matrix. If
                    // #' after repeating singleton rows and columns the
                    // #' matrices are still of different shape, then an error
                    // #' is thrown. Currently the following elementwise binary
                    // #' operators are available: `+`, `-`, `*`, `/`, `^`.
                    case MP2_ADD: // +
                        #ifdef MP_VERBOSE
                            std::cout << r[0] << " + " << r[1] << " = " << r[0]+r[1] << std::endl << std::endl;
                        #endif
                        return r[0]+r[1];
                    case MP2_SUBTRACT: // -
                        #ifdef MP_VERBOSE
                            if (table_n[row]==1)
                                std::cout << "Unary - " << r[0] << std::endl << std::endl;
                            else
                                std::cout << r[0] << " - " << r[1] << " = " << r[0]-r[1] << std::endl << std::endl;
                        #endif
                        if (table_n[row]==1)
                            return -r[0];
                        else
                            return r[0]-r[1];
                    case MP2_MULTIPLY: // *
                        #ifdef MP_VERBOSE
                            std::cout << r[0] << " .* " << r[1] << " = " << r[0].cwiseProduct(r[1]) << std::endl << std::endl;
                        #endif
                        //return r[0].array()*r[1].array();   // doesn't work
                        return r[0].cwiseProduct(r[1]);
                    case MP2_DIVIDE: // /
                        #ifdef MP_VERBOSE
                            std::cout << r[0] << " ./ " << r[1] << " = " << r[0].array()/r[1].array() << std::endl << std::endl;
                        #endif
                        // return r[0].array()/r[1].array();  // doesn't work
                        return r[0].cwiseQuotient(r[1]);
                    case MP2_POWER: // ^
                        #ifdef MP_VERBOSE
                            std::cout << r[0] << " ^ " << r[1] << " = " << pow(r[0].array(), r[1].coeff(0,0)).matrix() << std::endl << std::endl;
                        #endif
                        return pow(r[0].array(), r[1].array()).matrix();
                        //return r[0].pow(r[1].coeff(0,0));


                    // #' ## Unary Elementwise Math Functions
                    // #'
                    // #' Currently the `log` and `exp` functions.
                    // #'
                    case MP2_LOG:
                        return r[0].array().log().matrix();

                    case MP2_EXP:
                        return r[0].array().exp().matrix();

                    // #' ## Sequences and Repeated Patterns
                    // #'
                    case MP2_COLON: // :

                    // #' The colon operator works much like the base R version
                    // #' \code{\link{:}}. It takes two scalar-valued integers
                    // #' and returns a column vector with all integers between
                    // #' the two inputs.
                    // #'
                        int from, to;
                        from = CppAD::Integer(r[0].coeff(0,0));
                        to = CppAD::Integer(r[1].coeff(0,0));
                        if (from>to) {
                            SetError(MP2_COLON, "Lower bound greater than upper bound in : operation");
                            return m;
                        }
                        m = matrix<Type>::Zero(to-from+1,1);
                        for (int i=from; i<=to; i++)
                            m.coeffRef(i-from,0) = i;
                        #ifdef MP_VERBOSE
                            std::cout << from << ":" << to << " = " << m << std::endl << std::endl;
                        #endif
                        return m;

                    case MP2_SEQUENCE: // seq

                    // #' The `seq` function is similar to the base R default
                    // #' \code{\link{seq}} function. It takes the following
                    // #' scalar-valued integer arguments, and returns a
                    // #' column vector.
                    // #'
                    // #' * `from` -- First integer of the output column vector.
                    // #' * `length` -- Length of the output column vector.
                    // #' * `by` -- Difference between subsequent elements in
                    // #' the output column vector.
                    // #'
                        int length, by;
                        from = CppAD::Integer(r[0].coeff(0,0));
                        length = CppAD::Integer(r[1].coeff(0,0));
                        by = CppAD::Integer(r[2].coeff(0,0));
                        if (length<=0) {
                            SetError(MP2_SEQUENCE, "Sequence length is less than or equal to zero in seq operation");
                            return m;
                        }
                        m = matrix<Type>::Zero(length,1);
                        for (int i=0; i<length; i++)
                            m.coeffRef(i,0) = from + i*by;
                        #ifdef MP_VERBOSE
                            std::cout << "seq(" <<from << ", " << length << ", " << by << ") = " \
                                      << m << std::endl << std::endl;
                        #endif
                        return m;

                    case MP2_REPLICATE: // rep

                    // #' The \code{\link{rep}} function can be used to repeat
                    // #' the elements of a scalar `n` times. The following
                    // #' arguments are available.
                    // #'
                    // #' * `x` -- A scalar-valued variable to repeat.
                    // #' * `times` -- A scalar-valued integer variable giving
                    // #' the number of times to repeat `x`.
                    // #'
                    // #' The result is a column vector. This function differs
                    // #' from its base R version in that `x` must be a scalar,
                    // #' otherwise only the first element of `x` will be used.
                    // #' TODO: Consider allowing generic `x`
                    // #'
                        rows = CppAD::Integer(r[1].coeff(0,0));
                        m = matrix<Type>::Constant(rows,1, r[0].coeff(0,0));
                        //for (int i=0; i<rows; i++)
                        //    m.coeffRef(i,0) = r[0].coeff(0,0);

                        #ifdef MP_VERBOSE
                            std::cout << "rep(" << r[0] << ", " << r[1] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;

                    case MP2_MATRIX_MULTIPLY: // %*%

                    // #' ## Matrix Multiplication
                    // #'
                    // #' Standard matrix multiplication, \code{\link{%*%}},
                    // #' is available.
                        #ifdef MP_VERBOSE
                            std::cout << r[0] << " %*% " << r[1] << " = " << r[0]*r[1] << std::endl << std::endl;
                        #endif
                        return r[0]*r[1];

                    case MP2_ROUND_BRACKET: // (

                    // #' ## Parenthesis
                    // #'
                    // #' The order of operations can be enforced in the usual
                    // #' way with round parentheses, \code{\link{(}}.
                    // #'
                        return r[0];

                    // #' ## Reshaping and Combining Matrices
                    // #'
                    case MP2_COMBINE: // c

                    // #' Any number of scalars can be combined into a column
                    // #' vector using the \code{\link{c}} function. If
                    // #' non-scalars are provided then only the element in
                    // #' the first row and column of each input are used.
                    // #' TODO: Consider modifying so that `c` works more like
                    // #' the base R `c`, in that it stacks matrix columns.
                    // #'
                        m = matrix<Type>::Zero(n,1);
                        for (int i=0; i<n; i++)
                            m.coeffRef(i,0) = r[i].coeff(0,0);
                        #ifdef MP_VERBOSE
                            std::cout << "c(" << r[0] << ", ...," << r[n-1] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;

                    // #' Column and row vectors of the same length can be
                    // #' combined using the \code{\link{cbind}} and
                    // #' \code{\link{rbind}} functions respectively
                    // #'
                    case MP2_CBIND:
                    {
                        rows = r[0].rows();
                        // std::cout << "rows: " << rows << std::endl;
                        // std::cout << "n: " << n << std::endl;
                        int cols_per_arg;
                        int totcols, colmarker;
                        totcols = 0;
                        colmarker = 0;
                        for (int j=0; j<n; j++){
                            totcols += r[j].cols();
                        }
                        m = matrix<Type>::Zero(rows, totcols);
                        for (int i=0; i<n; i++) {
                            if (r[i].rows()==rows){
                                cols_per_arg = r[i].cols();
                                for (int k=0; k<cols_per_arg; k++){
                                    m.col(colmarker+k) = r[i].col(k);
                                }
                                colmarker += cols_per_arg;
                            }
                            else {
                                SetError(MP2_CBIND, "Inconsistent size in cbind function");
                                return m;
                            }
                        }
                    }
                        //m = matrix<Type>::Zero(rows, 1);
                        return m;
                    case MP2_RBIND:
                    {
                        cols = r[0].cols();
                        // std::cout << "cols: " << cols << std::endl;
                        // std::cout << "n: " << n << std::endl;
                        int rows_per_arg;
                        int totrows, rowmarker;
                        totrows = 0;
                        rowmarker = 0;
                        for (int j=0; j<n; j++){
                            totrows += r[j].rows();
                        }
                        m = matrix<Type>::Zero(totrows, cols);
                        for (int i=0; i<n; i++) {
                            if (r[i].cols()==cols){
                                rows_per_arg = r[i].rows();
                                for (int k=0; k<rows_per_arg; k++){
                                    m.row(rowmarker+k) = r[i].row(k);
                                }
                                rowmarker += rows_per_arg;
                            }
                            else {
                                SetError(MP2_RBIND, "Inconsistent size in rbind function");
                                return m;
                            }
                        }
                    }
                        //m = matrix<Type>::Zero(1, cols);
                        return m;
                    case MP2_MATRIX: // matrix

                    // #' The `matrix` function can be used to redefine the
                    // #' numbers of rows and columns to use for arranging
                    // #' the values of a matrix. It works similarly to
                    // #' the base R \code{\link{matrix}} function in that it
                    // #' takes the following arguments.
                    // #'
                    // #' * `data` -- A matrix to reshape.
                    // #' * `nrow` -- An integer scalar giving the number of
                    // #' rows in the output matrix.
                    // #' * `ncol` -- An integer scalar giving the number of
                    // #' columns in the output matrix.
                    // #'
                    // #' On the other hand, this function differs substantially
                    // #' from the base R version in that it must be filled
                    // #' by column and there is no `byrow` option.
                    // #'
                        m = r[0];

                        rows = CppAD::Integer(r[1].coeff(0,0));
                        cols = CppAD::Integer(r[2].coeff(0,0));

                        //m.conservativeResize(rows, cols); // don't know why this doesn't work
                        m.resize(rows, cols);

                        // m2 = m.transpose(); // m = m.transpose() doesn't work !!!
                        m2 = m;

                        #ifdef MP_VERBOSE
                            std::cout << "matrix(" << r[0] << ") reshaped into [" << rows << ", " << cols << "] = " \
                                      << m2 << std::endl << std::endl;
                        #endif

                        return m2;

                    // #' Matrices can be transposed with the usual
                    // #' function, \code{\link{t}}.
                    // #'
                    case MP2_TRANSPOSE: // t or transpose
                        m = r[0].transpose();
                        return m;

                    // #' ## Summarizing Matrix Values
                    // #'
                    case MP2_SUM: // sum

                    // #' The elements of a matrix can be summed together using
                    // #' the standard \code{\link{sum}} function.
                    // #'

                        m = matrix<Type>::Zero(1,1);
                        sum = 0.0;
                        for (int i=0; i<n; i++)
                            sum += r[i].sum();
                        m.coeffRef(0,0) = sum;

                        #ifdef MP_VERBOSE
                            std::cout << "sum(" << r[0] << ", ..., " << r[n-1] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;

                    // #' The standard \code{\link{rowSums}} and
                    // #' \code{\link{colSums}} can be used, but they have
                    // #' slightly different behaviour from their base R
                    // #' versions. In particular, the `rowSums` function
                    // #' returns a column vector and the `colSums` function
                    // #' returns a row vector. If a specific shape is required
                    // #' then the transpose \code{\link{t}} function must be
                    // #' explicitly used.
                    // #'
                    case MP2_ROWSUMS: // rowSums
                        //m = matrix<Type>::Zero(r[0].rows(), 1);
                        m = r[0].rowwise().sum().matrix();
                        #ifdef MP_VERBOSE
                            std::cout << "rowSums(" << r[0] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;
                    case MP2_COLSUMS: // colSums
                        m = r[0].colwise().sum().matrix();
                        #ifdef MP_VERBOSE
                            std::cout << "colSums(" << r[0] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;
                    case MP2_GROUPSUMS: // groupSums
                        rows = CppAD::Integer(r[1].maxCoeff()+0.1f) + 1;
                        m = matrix<Type>::Zero(rows, 1);
                        for (int i = 0; i < r[0].rows(); i++) {
                            rowIndex = CppAD::Integer(r[1].coeff(i,0)+0.1f);
                            m.coeffRef(rowIndex,0) += r[0].coeff(i,0);
                        }
                        return m;

                    // #' ## Extracting Matrix Elements
                    // #'
                    // #' It is possible to extract a single element from a
                    // #' matrix using square brackets. Two
                    // #' indices must be supplied for both the row and column
                    // #' positions. Note that zero-based indexing is used
                    // #' so the first element gets index, `0`, etc. It is
                    // #' currently not possible to extract sub-matrices
                    // #' of arbitrary dimensions, but this GitHub issue
                    // #' will address this shortcoming when it is completed,
                    // #' \url{https://github.com/canmod/macpan2/issues/10}.
                    // #'
                    case MP2_SQUARE_BRACKET: // [
                        #ifdef MP_VERBOSE
                            std::cout << "square bracket" << std::endl << std::endl;
                        #endif

                        int nrow;
                        int ncol;
                        nrow = r[1].size();
                        ncol = r[2].size();
                        m = matrix<Type>::Zero(nrow,ncol);
                        // if we can assume contiguous sets of rows and columns
                        // then mat.block(...) will be faster, so should we
                        // have a block function on the R side when speed
                        // matters?
                        // Can we vectorize CppAD::Integer casting??
                        for (int i=0; i<nrow; i++) {
                            for (int j=0; j<ncol; j++) {
                                rowIndex = CppAD::Integer(r[1].coeff(i,0));
                                colIndex = CppAD::Integer(r[2].coeff(j,0));
                                m.coeffRef(i,j) = r[0].coeff(rowIndex, colIndex);
                            }
                        }
                        return m;

                    // #' ## Accessing Past Values in the Simulation History
                    // #'
                    // #' For matrices with their simulation history saved,
                    // #' it is possible to bind the rows or columns of past
                    // #' versions of such matrices into a single matrix.
                    // #'
                    // #' There are four versions of this functionality.
                    // #'
                    // #' * `rbind_lag(x, lag)` -- Bind the rows of versions of
                    // #' `x` that were recorded at the end of all
                    // #' simulation iterations corresponding to time lags given
                    // #' by integers in `lag`.
                    // #' * `rbind_time(x, t)` -- Bind the rows of versions of
                    // #' `x` that were recorded at the end of all
                    // #' simulation iterations corresponding to integers in
                    // #' `t`.
                    // #' * `cbind_lag(x, lag)` -- Bind the columns of versions of
                    // #' `x` that were recorded at the end of all
                    // #' simulation iterations corresponding to time lags given
                    // #' by integers in `lag`. (TODO -- cbind_lag is not developed yet)
                    // #' * `cbind_time(x, t)` -- Bind the columns of versions of
                    // #' `x` that were recorded at the end of all
                    // #' simulation iterations corresponding to integers in
                    // #' `t`. (TODO -- cbind_lag is not developed yet)
                    // #'
                    case MP2_RBIND_LAG:
                        r[1] = -r[1];
                        r[1].array() += t; // += t+0.1f; // +0.1 won't work when t<0
                    case MP2_RBIND_TIME:
                        matIndex = index2mats[0]; // m
                        if (mats_save_hist[matIndex]==0 && !(r[1].size()==1 && CppAD::Integer(r[1].coeff(0,0))==t)) {
                            SetError(MP2_RBIND_TIME, "Cannot rbind_time (or rbind_lag) a matrix with no history");
                            return m;
                        }

                        int lowerTimeBound;
                        if (table_n[row]==3)
                            lowerTimeBound = CppAD::Integer(r[2].coeff(0,0));
                        else
                            lowerTimeBound = 0;

                        // Get the length of legitimate times in rbind_time.
                        // Check if the shape of the matrix changes.
                        //    Error if yes or assign variables "rows" and "cols" with
                        //    the correct values otherwise.
                        int rbind_length, nRows, nCols;
                        rbind_length = 0; // count of legitimate time steps to select
                        for (int i=0; i<r[1].size(); i++) {
                            rowIndex = CppAD::Integer(r[1].coeff(i,0));
                            if (rowIndex<t && rowIndex>=lowerTimeBound) {
                                nRows = hist[rowIndex].m_matrices[matIndex].rows();
                                nCols = hist[rowIndex].m_matrices[matIndex].cols();
                            }
                            else if (rowIndex==t) {
                                nRows = valid_vars.m_matrices[matIndex].rows();
                                nCols = valid_vars.m_matrices[matIndex].cols();
                            }
                            else
                                continue;

                            if (nRows==0 || nCols==0) // skip empty matrix
                                continue;

                            if (rbind_length==0) { // first one
                                rows = nRows;
                                cols = nCols;
                            }
                            else {
                                if (rows!=nRows || cols!=nCols) { // Shall we allow inconsistent rows?
                                    SetError(MP2_RBIND_TIME, "Inconsistent rows or columns in rbind_time (or rbind_lag)");
                                    return m;
                                }
                            }

                            rbind_length++;
                        }
                        #ifdef MP_VERBOSE
                            std::cout << "rbind_time(" << r[1] << ") = " << std::endl;
                        #endif

                        if (rbind_length>0) {
                            //rows = hist[0].m_matrices[matIndex].rows();
                            //cols = hist[0].m_matrices[matIndex].cols();
                            m = matrix<Type>::Zero(rbind_length*rows, cols);
                            rbind_length = 0;
                            for (int i=0; i<r[1].size(); i++) {
                                rowIndex = CppAD::Integer(r[1].coeff(i,0));
                                if (rowIndex<t && rowIndex>=lowerTimeBound) {
                                    if (hist[rowIndex].m_matrices[matIndex].rows()!=0 &&
                                        hist[rowIndex].m_matrices[matIndex].cols()!=0) {
                                        m.block(rbind_length*rows, 0, rows, cols) = hist[rowIndex].m_matrices[matIndex];
                                        rbind_length++;
                                    }
                                }
                                else if (rowIndex==t) {
                                    if (valid_vars.m_matrices[matIndex].rows()!=0 &&
                                        valid_vars.m_matrices[matIndex].cols()!=0) {
                                        m.block(rbind_length*rows, 0, rows, cols) = valid_vars.m_matrices[matIndex];
                                        rbind_length++;
                                    }
                                }
                                #ifdef MP_VERBOSE
                                    std::cout << m.block((rbind_length-1)*rows, 0, rows, cols) << std::endl << std::endl;
                                #endif
                            }
                        }

                        return m; // empty matrix (if colIndex==0) or non-empty one (otherwise)

                    case MP2_TIME_STEP:
                        m = matrix<Type>::Zero(1,1);
                        m.array() = CppAD::Integer(t+0.1f);
                        //m.coeffRef(0,0) = step - r[0];
                        //std::cout << step << std::endl;
                        return m;

                    case MP2_CONVOLUTION:

                    // #' ## Convolution
                    // #'
                    // #' One may take the convolution of each element in a
                    // #' matrix, x, over simulation time using a kernel, k.
                    // #' There are two arguments of this function.
                    // #'
                    // #' * `x` -- The matrix to be convolved.
                    // #' * `k` -- A column vector giving the convolution kernel.
                    // #'
                        matIndex = index2mats[0]; // m
                        #ifdef MP_VERBOSE
                            std::cout << "matIndex: " << matIndex << std::endl << std::endl;
                        #endif
                        length = r[1].rows();
                        #ifdef MP_VERBOSE
                            std::cout << "length: " << length << std::endl << std::endl;
                        #endif
                        if (length>0 && r[1].cols()==1) {
                            #ifdef MP_VERBOSE
                                std::cout << "kernel 1: " << r[1] << std::endl << std::endl;
                            #endif
                            if (t+1<length) {
                                length = t+1;
                                r[1] = r[1].block(0, 0, length, 1);
                            }
                            #ifdef MP_VERBOSE
                                std::cout << "kernel 2: " << r[1] << std::endl << std::endl;
                            #endif

                            rows = r[0].rows();
                            cols = r[0].cols();
                            m = matrix<Type>::Zero(rows, cols);

                            for (int i=0; i<rows; i++)
                                for (int j=0; j<cols; j++)
                                    m.coeffRef(i,j) = r[1].coeff(0,0) * valid_vars.m_matrices[matIndex].coeff(i,j);

                            for (int k=1; k<=length-1; k++)
                                if (hist[t-k].m_matrices[matIndex].rows()!=0 &&
                                    hist[t-k].m_matrices[matIndex].cols()!=0)
                                    for (int i=0; i<rows; i++)
                                        for (int j=0; j<cols; j++)
                                            m.coeffRef(i,j) += r[1].coeff(k,0) * hist[t-k].m_matrices[matIndex].coeff(i,j);

                            return m;
                        }
                        else {
                            SetError(MP2_CONVOLUTION, "Either empty or non-column vector used as kernel in convolution");
                            return m;
                        }

                    // #' ## Clamp
                    // #'
                    // #' Clamp the elements of a matrix so that they do not
                    // #' get closer to 0 than 1e-12 (TODO: make this tolerance
                    // #' an optional second argument).
                    case MP2_CLAMP:
                        eps = 1e-12;
                        rows = r[0].rows();
                        cols = r[0].cols();
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                           for (int j=0; j<cols; j++) {
                               m.coeffRef(i,j) = r[0].coeff(i,j) + eps * (1.0 / (1.0-(r[0].coeff(i,j)-eps)/eps + ((r[0].coeff(i,j)-eps)*(r[0].coeff(i,j)-eps))/(eps*eps)));
                           }
                        }
                        return m;

                    // #' ## Probability Densities
                    // #'
                    // #' All probability densities have the same first two
                    // #' arguments.
                    // #'
                    // #' * `observed`
                    // #' * `simulated`
                    case MP2_POISSON_DENSITY:
                        rows = r[0].rows();
                        cols = r[0].cols();
                        RecycleInPlace(r[1], rows, cols);
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                            for (int j=0; j<cols; j++) {
                                m.coeffRef(i,j) = dpois(r[0].coeff(i,j), r[1].coeff(i,j), 1);
                            }
                        }
                        return m;

                    case MP2_NORMAL_DENSITY:
                        rows = r[0].rows();
                        cols = r[0].cols();
                        RecycleInPlace(r[1], rows, cols);
                        RecycleInPlace(r[2], rows, cols);
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                            for (int j=0; j<cols; j++) {
                                m.coeffRef(i,j) = dnorm(r[0].coeff(i,j), r[1].coeff(i,j), r[2].coeff(i,j), 1);
                            }
                        }
                        return m;

                    case MP2_POISSON_SIM:
                        rows = r[0].rows();
                        cols = r[0].cols();
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                            for (int j=0; j<cols; j++) {
                                m.coeffRef(i,j) = rpois(r[0].coeff(i,j));
                            }
                        }
                        return m;

                    case MP2_NORMAL_SIM:
                        rows = r[0].rows();
                        cols = r[0].cols();
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                            for (int j=0; j<cols; j++) {
                                m.coeffRef(i,j) = rnorm(r[0].coeff(i,j), r[1].coeff(i,j));
                            }
                        }
                        return m;

                    case MP2_ASSIGN:
                        int size, sz, start;
                        // matIndex = index2mats[0]; // m
                        // valid_vars.m_matrices[matIndex]


                        m = r[0];
                        size = m.rows()*m.cols();
                        m.resize(size, 1);

                        start = 0;
                        for (int i=1; i<n; i++) {
                            sz = r[i].rows() * r[i].cols();
                            if (size>=sz) {
                                m1 = m.block(start, 0, sz, 1);
                                m1.resize(r[i].rows(), r[i].cols());
                                //std::cout << "MMMAAATTTRRRIIIXXX " << valid_vars.m_matrices[index2mats[i]] << std::endl << std::endl;
                                valid_vars.m_matrices[index2mats[i]] = m1;
                                // r[i] = m1;
                                size -= sz;
                                start += sz;
                            }
                            else
                                break;
                        }
                        return m2; // empty matrix

                    default:
                        SetError(255, "invalid operator in arithmetic expression");
                        return m;
                }
        }
    };

private:
    unsigned char error_code;
    char error_message[256];
};

#define REPORT_ERROR { \
    int error = exprEvaluator.GetErrorCode(); \
    REPORT(error); \
 \
    logfile.open (LOG_FILE_NAME, std::ios_base::app); \
    logfile << "Error code = " << error << std::endl; \
    logfile << "Error message = " << exprEvaluator.GetErrorMessage() << std::endl; \
    logfile.close(); \
}

// Helper function
template<class Type>
void UpdateSimulationHistory(
    vector<ListOfMatrices<Type> >& hist,
    int t,
    const ListOfMatrices<Type>& mats,
    const vector<int>& mats_save_hist
) {
    matrix<Type> emptyMat;

    ListOfMatrices<Type> ms(mats);
    // if the history of the matrix is not to be saved,
    // just save a 1-by-1 with a zero instead to save space
    for (int i=0; i<mats_save_hist.size(); i++)
        if (mats_save_hist[i]==0)
            ms.m_matrices[i] = emptyMat;

    hist[t] = ms;
}


const char LOG_FILE_NAME[] = "macpan2.log";

// "main" function
template<class Type>
Type objective_function<Type>::operator() ()
{
    std::cout << "============== objective_function =============" << std::endl;

    std::ofstream logfile;
    logfile.open (LOG_FILE_NAME);
    logfile << "======== log file of MacPan2 ========\n";
    logfile.close();

    std::setprecision(9); // Set the precision of std::cout

    // 1 Get all data and parameters from the R side
    // Parameters themselves
    int n;

    // Fixed effects
    PARAMETER_VECTOR(params);

    // Random effects
    PARAMETER_VECTOR(random);

    // Matrices
    DATA_STRUCT(mats, ListOfMatrices);
    DATA_IVECTOR(mats_save_hist);
    DATA_IVECTOR(mats_return);

    // Parameters replacements
    DATA_IVECTOR(p_par_id);
    DATA_IVECTOR(p_mat_id);
    DATA_IVECTOR(p_row_id);
    DATA_IVECTOR(p_col_id);
    DATA_IVECTOR(r_par_id);
    DATA_IVECTOR(r_mat_id);
    DATA_IVECTOR(r_row_id);
    DATA_IVECTOR(r_col_id);

    // Trajectory simulation
    DATA_INTEGER(time_steps)

    // Expressions and parse table
    DATA_IVECTOR(expr_output_id);
    DATA_IVECTOR(expr_sim_block);
    DATA_IVECTOR(expr_num_p_table_rows);
    DATA_IVECTOR(eval_schedule)
    DATA_IVECTOR(p_table_x);
    DATA_IVECTOR(p_table_n);
    DATA_IVECTOR(p_table_i);

    // Literals
    DATA_VECTOR(literals);

    // Objective function parse table
    DATA_IVECTOR(o_table_n);
    DATA_IVECTOR(o_table_x);
    DATA_IVECTOR(o_table_i);

    // DATA_STRUCT(settings, settings_struct);

    #ifdef MP_VERBOSE
    std::cout << "params = " << params << std::endl;

    std::cout << "random = " << random << std::endl;

    n = mats.m_matrices.size();
    for (int i = 0; i < n; i++)
        std::cout << "mats = " << mats.m_matrices[i] << std::endl;

    std::cout << "p_par_id = " << p_par_id << std::endl;
    std::cout << "p_mat_id = " << p_mat_id << std::endl;
    std::cout << "p_row_id = " << p_row_id << std::endl;
    std::cout << "p_col_id = " << p_col_id << std::endl;

    std::cout << "r_par_id = " << r_par_id << std::endl;
    std::cout << "r_mat_id = " << r_mat_id << std::endl;
    std::cout << "r_row_id = " << r_row_id << std::endl;
    std::cout << "r_col_id = " << r_col_id << std::endl;

    std::cout << "time_steps = " << time_steps << std::endl;

    std::cout << "mats_save_hist = " << mats_save_hist << std::endl;
    std::cout << "mats_return = " << mats_return << std::endl;

    std::cout << "eval_schedule = " << eval_schedule << std::endl;

    //std::cout << "expr_output_count = " << expr_output_count << std::endl;
    std::cout << "expr_output_id = " << expr_output_id << std::endl;
    std::cout << "expr_sim_block = " << expr_sim_block << std::endl;
    std::cout << "expr_num_p_table_rows = " << expr_num_p_table_rows << std::endl;

    std::cout << "p_table_x = " << p_table_x << std::endl;
    std::cout << "p_table_n = " << p_table_n << std::endl;
    std::cout << "p_table_i = " << p_table_i << std::endl;

    std::cout << "literals = " << literals << std::endl;

    std::cout << "o_table_x = " << o_table_x << std::endl;
    std::cout << "o_table_n = " << o_table_n << std::endl;
    std::cout << "o_table_i = " << o_table_i << std::endl;
    #endif

    // 2 Replace some of elements of some matrices with parameters
    n = p_par_id.size();
    for (int i=0; i<n; i++)
        mats.m_matrices[p_mat_id[i]].coeffRef(p_row_id[i], p_col_id[i]) = params[p_par_id[i]];

    n = r_par_id.size();
    for (int i=0; i<n; i++)
        mats.m_matrices[r_mat_id[i]].coeffRef(r_row_id[i], r_col_id[i]) = random[r_par_id[i]];

    //////////////////////////////////
    // Define an expression evaluator
    ExprEvaluator<Type> exprEvaluator;
    //////////////////////////////////

    // Simulation history
    /// each element of this history 'vector' is a list of the matrices
    /// in the model at a particular point in history
    vector<ListOfMatrices<Type> > simulation_history(time_steps+2);

    // 3 Pre-simulation
    int expr_index = 0;
    int p_table_row = 0;

    for (int i=0; i<eval_schedule[0]; i++) {
        #ifdef MP_VERBOSE
        std::cout << "in pre-simulation --- " << i << std::endl;
        std::cout << "expr_num_p_table_rows[i] " << expr_num_p_table_rows[i] << std::endl;
        #endif
        matrix<Type> result;
        if (expr_sim_block[i]==1) {
            SIMULATE {
                result  = exprEvaluator.EvalExpr(
                    simulation_history,
                    0,
                    mats_save_hist,
                    p_table_x,
                    p_table_n,
                    p_table_i,
                    mats,
                    literals,
                    p_table_row
                );
            }
        }
        else
            result  = exprEvaluator.EvalExpr(
                simulation_history,
                0,
                mats_save_hist,
                p_table_x,
                p_table_n,
                p_table_i,
                mats,
                literals,
                p_table_row
            );

        if (exprEvaluator.GetErrorCode()) {
            REPORT_ERROR
            return 0.0;
        }

        mats.m_matrices[expr_output_id[expr_index+i]] = result;

        p_table_row += expr_num_p_table_rows[i];
    }

    //simulation_history[0] = mats;
    UpdateSimulationHistory(
        simulation_history,
        0,
        mats,
        mats_save_hist
    );

    // 4 During simulation
    expr_index += eval_schedule[0];

    int p_table_row2;
    for (int k=0; k<time_steps; k++) {
        p_table_row2 = p_table_row;
        #ifdef MP_VERBOSE
        std::cout << "simulation step --- " << k << std::endl;
        #endif
        for (int i=0; i<eval_schedule[1]; i++) {
            #ifdef MP_VERBOSE
            std::cout << "Eval expression --- " << i << std::endl;
            std::cout << "expr_num_p_table_rows[i] " << expr_num_p_table_rows[expr_index+i] << std::endl;
            #endif
            matrix<Type> result;
            if (expr_sim_block[i]==1) {
                SIMULATE {
                    result = exprEvaluator.EvalExpr(
                        simulation_history,
                        k+1,
                        mats_save_hist,
                        p_table_x,
                        p_table_n,
                        p_table_i,
                        mats,
                        literals,
                        p_table_row2
                   );
                }
            }
            else
                result = exprEvaluator.EvalExpr(
                    simulation_history,
                    k+1,
                    mats_save_hist,
                    p_table_x,
                    p_table_n,
                    p_table_i,
                    mats,
                    literals,
                    p_table_row2
               );

            if (exprEvaluator.GetErrorCode()) {
                REPORT_ERROR
                return 0.0;
            }
            mats.m_matrices[expr_output_id[expr_index+i]] = result;

            p_table_row2 += expr_num_p_table_rows[expr_index+i];

            #ifdef MP_VERBOSE
            int n = mats.m_matrices.size();
            for (int ii = 0; ii < n; ii++)
                std::cout << "mats = " << mats.m_matrices[ii] << std::endl;
            #endif
        }
        //simulation_history[k+1] = mats;
        UpdateSimulationHistory(
            simulation_history,
            k+1,
            mats,
            mats_save_hist
        );
    }
    p_table_row = p_table_row2;

    // 5 Post-simulation
    expr_index += eval_schedule[1];

    for (int i=0; i<eval_schedule[2]; i++) {
        #ifdef MP_VERBOSE
        std::cout << "in post-simulation --- " << i << std::endl;
        std::cout << "expr_num_p_table_rows[i] " << expr_num_p_table_rows[expr_index+i] << std::endl;
        #endif
        matrix<Type> result;
        if (expr_sim_block[i]==1) {
            SIMULATE {
                result = exprEvaluator.EvalExpr(
                    simulation_history,
                    time_steps+1,
                    mats_save_hist,
                    p_table_x,
                    p_table_n,
                    p_table_i,
                    mats,
                    literals,
                    p_table_row
                );
            }
        }
        else
            result  = exprEvaluator.EvalExpr(
                simulation_history,
                time_steps+1,
                mats_save_hist,
                p_table_x,
                p_table_n,
                p_table_i,
                mats,
                literals,
                p_table_row
            );

        if (exprEvaluator.GetErrorCode()) {
            REPORT_ERROR
            return 0.0;
        }

        mats.m_matrices[expr_output_id[expr_index+i]] = result;

        p_table_row += expr_num_p_table_rows[expr_index+i];
    }

    //simulation_history[time_steps+1] = mats;
    UpdateSimulationHistory(
        simulation_history,
        time_steps+1,
        mats,
        mats_save_hist
    );

#ifdef MP_VERBOSE
    std::cout << "Simulation history ..." << std::endl;
    int m = simulation_history.size();
    for (int t=0; t<m; t++) {
        std::cout << "----- t = " << t << std::endl;
        ListOfMatrices<Type> mats = simulation_history[t];
        int n = mats.m_matrices.size();
        for (int i = 0; i < n; i++)
            std::cout << "mats = " << mats.m_matrices[i] << std::endl;
    }
#endif

    // 6 Report a table as history, each row contains
    //   mat_id, time_step, row_id, col_id, value (This is the the order the current
    //   implementation uses)
    //   These indices are all zero-based.

    // int r = 0;
    int table_rows = 0;
    for (int i=0; i<mats_return.size(); i++) {
        if (mats_return[i]==1) {
            if (mats_save_hist[i]==0) { // Report the last one
                table_rows += mats.m_matrices[i].rows() * mats.m_matrices[i].cols();
            }
            else { // Report the whole simulation history
                int hist_len = time_steps+2;
                for (int k=0; k<hist_len; k++)
                    table_rows += simulation_history[k].m_matrices[i].rows() * \
                                  simulation_history[k].m_matrices[i].cols();
            }
        }
    }

    matrix<Type> values(table_rows, 5);

    int cur = 0;
    for (int i=0; i<mats_return.size(); i++) {
        if (mats_return[i]==1) {
            if (mats_save_hist[i]==0) { // Report the last one
                for (int jj=0; jj<mats.m_matrices[i].cols(); jj++)
                    for (int ii=0; ii<mats.m_matrices[i].rows(); ii++) {
                        values(cur,0) = i;
                        values(cur,1) = time_steps+1;
                        values(cur,2) = ii;
                        values(cur,3) = jj;
                        values(cur,4) = mats.m_matrices[i].coeff(ii, jj);

                        cur++;
                    }
            }
            else { // Report the whole simulation history
                int hist_len = time_steps+2;
                for (int k=0; k<hist_len; k++)
                    for (int jj=0; jj<simulation_history[k].m_matrices[i].cols(); jj++)
                        for (int ii=0; ii<simulation_history[k].m_matrices[i].rows(); ii++) {
                            values(cur,0) = i;
                            values(cur,1) = k;
                            values(cur,2) = ii;
                            values(cur,3) = jj;
                            values(cur,4) = simulation_history[k].m_matrices[i].coeff(ii, jj);

                            cur++;
                        }
            }
        }
    }

    REPORT(values)

    // 7 Calc the return of the objective function
    matrix<Type> ret;
    ret = exprEvaluator.EvalExpr(
              simulation_history,
              time_steps+2,
              mats_save_hist,
              o_table_x,
              o_table_n,
              o_table_i,
              mats,
              literals,
              0
          );

    if (exprEvaluator.GetErrorCode()) {
        REPORT_ERROR;
        return 0.0;
    }

    REPORT_ERROR

    std::cout << "======== end of objective function ========" << std::endl;
    return ret.coeff(0,0);
}
