// Auto-generated - do not edit by hand

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
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Functions we Support
// Please follow exactly the format below when adding a new function:
//   MP2_CPP_CASE_NAME = {case number} // {R function name}
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
      MP2_ADD = 1 // binop,null: `+`(x, y)
    , MP2_SUBTRACT = 2 // binop,null: `-`(x, y)
    , MP2_MULTIPLY = 3 // binop,null: `*`(x, y)
    , MP2_DIVIDE = 4 // binop,null: `/`(x, y)
    , MP2_POWER = 5 // binop,null: `^`(x, y)
    , MP2_EXP = 6 // fwrap,null: exp(x)
    , MP2_LOG = 7 // fwrap,null: log(x)
    , MP2_ROUND_BRACKET = 8 // null,null: `(`(...)
    , MP2_COMBINE = 9 // null,null: c(...)
    , MP2_MATRIX = 10 // fwrap,null: matrix(x, i, j)
    , MP2_MATRIX_MULTIPLY = 11 // binop,null: `%*%`(x, y)
    , MP2_SUM = 12 // null,null: sum(...)
    , MP2_REPLICATE = 13 // fwrap,null: rep(x, times)
    , MP2_ROWSUMS = 14 // fwrap,null: rowSums(x)
    , MP2_COLSUMS = 15 // fwrap,null: colSums(x)
    , MP2_GROUPSUMS = 16 // fwrap,null: groupSums(x, f, n)
    , MP2_SQUARE_BRACKET = 17 // null,null: `[`(x, i, j)
    , MP2_BLOCK = 18 // fwrap,fail: block(x, i, j, n, m)
    , MP2_TRANSPOSE = 19 // fwrap,null: t(x)
    , MP2_RBIND_TIME = 20 // fwrap,fail: rbind_time(x, t, t_min)
    , MP2_RBIND_LAG = 21 // fwrap,fail: rbind_lag(x, lag, t_min)
    , MP2_CBIND_TIME = 22 // fwrap,fail: cbind_time(x, t, t_min)
    , MP2_CBIND_LAG = 23 // fwrap,fail: cbind_lag(x, lag, t_min)
    , MP2_COLON = 24 // null,null: `:`(from, to)
    , MP2_SEQUENCE = 25 // fwrap,fail: seq(from, length, by)
    , MP2_CONVOLUTION = 26 // fwrap,fail: convolution(x, k)
    , MP2_CBIND = 27 // fwrap,null: cbind(...)
    , MP2_RBIND = 28 // fwrap,null: rbind(...)
    , MP2_TIME_STEP = 29 // fwrap,fail: time_step(lag)
    , MP2_ASSIGN = 30 // fwrap,null: assign(x, i, j, v)
    , MP2_UNPACK = 31 // fwrap,fail: unpack(x, ...)
    , MP2_RECYCLE = 32 // fwrap,null: recycle(x, rows, cols)
    , MP2_CLAMP = 33 // fwrap,null: clamp(x, eps)
    , MP2_POISSON_DENSITY = 34 // fwrap,fail: dpois(observed, simulated)
    , MP2_NEGBIN_DENSITY = 35 // fwrap,fail: dnbinom(observed, simulated, over_dispersion)
    , MP2_NORMAL_DENSITY = 36 // fwrap,fail: dnorm(observed, simulated, standard_deviation)
    , MP2_POISSON_SIM = 37 // fwrap,fail: rpois(mean)
    , MP2_NEGBIN_SIM = 38 // fwrap,fail: rnbinom(mean, over_dispersion)
    , MP2_NORMAL_SIM = 39 // fwrap,fail: rnorm(mean, standard_deviation)
    , MP2_KRONECKER = 40 // binop,null: `%x%`(x, y)
    , MP2_TO_DIAG = 41 // fwrap,fail: to_diag(x)
    , MP2_FROM_DIAG = 42 // fwrap,fail: from_diag(x)
    , MP2_TIME_GROUP = 43 //fwrap,fail: time_group(i, change_points)
    , MP2_COS = 44 // fwrap,null: cos(x)
    //, MP2_LOGISTIC = 45 // fwrap,null: logistic(x)
    //, MP2_LOGIT = 46 // fwrap,null: logit(x)
};

// Helper function
template<class Type>
int CheckIndices(
    matrix<Type>& x,
    matrix<Type>& rowIndices,
    matrix<Type>& colIndices
) {
    int rows = x.rows();
    int cols = x.cols();
    Type maxRowIndex = rowIndices.maxCoeff();
    Type maxColIndex = colIndices.maxCoeff();
    Type minRowIndex = rowIndices.minCoeff();
    Type minColIndex = colIndices.minCoeff();

    if ((maxRowIndex < rows) & (maxColIndex < cols) && (minRowIndex > -0.1) && (minColIndex > -0.1)) {
        return 0;
    }
    return 1;
}

// Helper function
template<class Type>
int RecycleInPlace(
    matrix<Type>& mat,
    int rows,
    int cols
) {
    #ifdef MP_VERBOSE
        std::cout << "recycling ... " << std::endl;
    #endif
    if (mat.rows()==rows && mat.cols()==cols) // don't need to do anything.
        return 0;

    matrix<Type> m(rows, cols);
    if (mat.rows()==1 && mat.cols()==1) {
        m = matrix<Type>::Constant(rows, cols, mat.coeff(0,0));
    }
    else if (mat.rows()==rows) {
        if (mat.cols()==1) {
            #ifdef MP_VERBOSE
                std::cout << "recycling columns ... " << std::endl;
            #endif
            for (int i=0; i<cols; i++)
                m.col(i) = mat.col(0);
        } else
            return 501;
            //SetError(501, "cannot recycle columns because the input is neither a scalar nor a column vector", row);
    }
    else if (mat.cols()==cols) {
        if (mat.rows()==1) {
            #ifdef MP_VERBOSE
                std::cout << "recycling rows ... " << std::endl;
            #endif
            for (int i=0; i<rows; i++)
                m.row(i) = mat.row(0);
        } else
            return 501;
            //SetError(501, "cannot recycle rows because the input is neither a scalar nor a row vector", row);
    } else
        return 501;
        //SetError(501, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", row);

    // final step
    mat = m;
    return 0;
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
    // constructor
    ExprEvaluator() {
        error_code = 0;	// non-zero means error has occurred; otherwise, no error
        expr_row = 0;
        strcpy(error_message, "OK");
    };

    // getters
    unsigned char GetErrorCode() { return error_code; };
    const char* GetErrorMessage() { return error_message; };
    int GetExprRow() {return expr_row; };

    // setters
    void SetError(unsigned char code, const char* message, int row)
    {
        error_code = code;
        expr_row = row;
        strcpy(error_message, message);
        std::cout << "MACPAN ERROR #" << (int) code << ": " << message << std::endl;
    };

    // evaluators
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
        // Variables to use locally in function bodies
        matrix<Type> m, m1, m2;  // return values
        matrix<Type> timeIndex; // for rbind_time
        Type sum, s, eps, var;  // intermediate scalars
        int rows, cols, lag, rowIndex, colIndex, matIndex, reps, cp, off, size, sz, start, err_code, err_code1, err_code2;

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
                vector<matrix<Type> > args(n);
                vector<int> index2mats(n);
                for (int i=0; i<n; i++) {
                    args[i] = EvalExpr(hist, t, mats_save_hist, table_x, table_n, table_i, \
                                    valid_vars, valid_literals, table_i[row]+i);
                    index2mats[i] = table_x[table_i[row]+i];
                    if (GetErrorCode()) return m;
                }

                // Check dimensions compatibility. If needed, expand one operand to make its dimensions compatible with the other
                if (table_x[row]+1<6 && table_n[row]==2) { // elementwise binary operations + - * / ^
                    if (args[0].rows()==args[1].rows()) {
                        if (args[0].cols()!=args[1].cols()) {
                            if (args[0].cols()==1) { // vector vs matrix or scalar vs vector
                                m = args[0];
                                args[0] = args[1]; // for the shape
                                for (int i=0; i<args[0].cols(); i++)
                                    args[0].col(i) = m.col(0);
                            }
                            else if (args[1].cols()==1) { // vector vs matrix or scalar vs vector
                                m = args[1];
                                args[1] = args[0]; // for the shape
                                for (int i=0; i<args[1].cols(); i++)
                                    args[1].col(i) = m.col(0);
                            }
                            else {
                                SetError(201, "The two operands do not have the same number of columns", row);
                                return m;
                                //Rf_error("The two operands do not have the same number of columns");
                            }
                        }
                        // else: do nothing
                    }
                    else {
                        if (args[0].cols()==args[1].cols()) { // only one compatible dimension
                            if (args[0].rows()==1) { // vector vs matrix or scalar vs vector
                                m = args[0];
                                args[0] = args[1]; // for the shape
                                for (int i=0; i<args[0].rows(); i++)
                                    args[0].row(i) = m.row(0);
                            }
                            else if (args[1].rows()==1) { // vector vs matrix or scalar vs vector
                                m = args[1];
                                args[1] = args[0]; // for the shape
                                for (int i=0; i<args[1].rows(); i++)
                                    args[1].row(i) = m.row(0);
                            }
                            else {
                                SetError(202, "The two operands do not have the same number of rows", row);
                                return m;
                                // Rf_error("The two operands do not have the same number of rows");
                            }
                        }
                        else { // no dimensions are equal
                            if (args[0].rows()==1 && args[0].cols()==1) { // scalar vs non-scalar
                                s = args[0].coeff(0,0);
                                args[0] = args[1];
                                args[0].setConstant(s);
                            }
                            else if (args[1].rows()==1 && args[1].cols()==1) { // scalar vs non-scalar
                                s = args[1].coeff(0,0);
                                args[1] = args[0];
                                args[1].setConstant(s);
                            }
                            else {
                                SetError(203, "The two operands do not have the same number of columns or rows", row);
                                return m;
                                //Rf_error("The dimensions of the two operands are not equal to each other");
                            }
                        }
                    }
                }
                else if (table_x[row]+1==11) { // %*% matrix multiplication
                    // std::cout << "mat mult index" << MP2_MATRIX_MULTIPLY << std::endl;
                    if (args[0].cols()!=args[1].rows()) {
                        SetError(204, "The two operands are not compatible to do matrix multiplication", row);
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
                // #' The quickest way to experiment with these functions is
                // #' to use the \code{\link{engine_eval}} function, as in the
                // #' following example that calculates a force of infection.
                // #'
                // #' ```
                // #' engine_eval(~ beta * I / N
                // #'   , beta = 0.25
                // #'   , I = 1e3
                // #'   , N = 1e7
                // #' )
                // #' ```
                // #'
                // #' To produce a simulation using these functions, one may
                // #' use \code{\link{simple_sims}}.
                // #'
                // #' ```
                // #' simple_sims(
                // #'   iteration_exprs = list(x ~ x - 0.9 * x),
                // #'   time_steps = 5,
                // #'   x = 1
                // #' )
                // #' ```
                // #'
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
                    // #' not of the same shape but there is one row and/or
                    // #' one column in either matrix, then the singleton rows
                    // #' and columns are recycled sufficiently many times
                    // #' so that they match the shape of the other matrix. If
                    // #' after recycling singleton rows and columns the
                    // #' matrices are still of different shape, then an error
                    // #' is thrown and the matrices are said to be incompatible.
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `x + y`
                    // #' * `x - y`
                    // #' * `x * y`
                    // #' * `x / y`
                    // #' * `x ^ y`
                    // #'
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `x` -- Any matrix with dimensions compatible with `y`.
                    // #' * `y` -- Any matrix with dimensions compatible with `x`.
                    // #'
                    // #' ### Return
                    // #'
                    // #' * A matrix with the binary operator applied elementwise
                    // #' after any necessary recycling of rows and/or columns.
                    // #'
                    // #' ### Examples
                    // #'
                    // #' ```
                    // #' engine_eval(~ 1 + 2)
                    // #' engine_eval(~ y * z, y = 1:3, z = matrix(1:6, 3, 2))
                    // #' engine_eval(~ 1 / (1 - y), y = 1/4)
                    // #' ```
                    // #'
                    case MP2_ADD: // +
                        #ifdef MP_VERBOSE
                            std::cout << args[0] << " + " << args[1] << " = " << args[0]+args[1] << std::endl << std::endl;
                        #endif
                        return args[0]+args[1];
                    case MP2_SUBTRACT: // -
                        #ifdef MP_VERBOSE
                            if (table_n[row]==1)
                                std::cout << "Unary - " << args[0] << std::endl << std::endl;
                            else
                                std::cout << args[0] << " - " << args[1] << " = " << args[0]-args[1] << std::endl << std::endl;
                        #endif
                        if (table_n[row]==1)
                            return -args[0];
                        else
                            return args[0]-args[1];
                    case MP2_MULTIPLY: // *
                        #ifdef MP_VERBOSE
                            std::cout << args[0] << " .* " << args[1] << " = " << args[0].cwiseProduct(args[1]) << std::endl << std::endl;
                        #endif
                        //return args[0].array()*args[1].array();   // doesn't work
                        return args[0].cwiseProduct(args[1]);
                    case MP2_DIVIDE: // /
                        #ifdef MP_VERBOSE
                            std::cout << args[0] << " ./ " << args[1] << " = " << args[0].array()/args[1].array() << std::endl << std::endl;
                        #endif
                        // return args[0].array()/args[1].array();  // doesn't work
                        return args[0].cwiseQuotient(args[1]);
                    case MP2_POWER: // ^
                        #ifdef MP_VERBOSE
                            std::cout << args[0] << " ^ " << args[1] << " = " << pow(args[0].array(), args[1].coeff(0,0)).matrix() << std::endl << std::endl;
                        #endif
                        return pow(args[0].array(), args[1].array()).matrix();
                        //return args[0].pow(args[1].coeff(0,0));


                    // #' ## Unary Elementwise Math
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `log(x)` -- Natural logarithm
                    // #' * `exp(x)` -- Exponential function
                    // #' * `cos(x)` -- Cosine function
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `x` -- Any matrix
                    // #'
                    // #' ### Return
                    // #'
                    // #' * A matrix with the same dimensions as `x`, with the
                    // #' unary function applied elementwise.
                    // #'
                    // #' ### Examples
                    // #'
                    // #' ```
                    // #' engine_eval(~ log(y), y = c(2, 0.5))
                    // #' ```
                    // #'
                    case MP2_LOG:
                        return args[0].array().log().matrix();

                    case MP2_EXP:
                        return args[0].array().exp().matrix();

                    case MP2_COS:
                        return args[0].array().cos().matrix();


                    // case MP2_LOGISTIC:
                    //     return (
                    //         1 / (1 + (-args[0].array()).exp())
                    //     ).matrix();
                    //
                    // case MP2_LOGIT:
                    //     return (
                    //         -(1 / args[0].array() - 1).log()
                    //     ).matrix();

                    // #' ## Integer Sequences
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `from:to` -- Inclusive and ordered sequence of
                    // #' integers between two bounds.
                    // #' * `seq(from, length, by)` -- Ordered sequence of
                    // #' integers with equal spacing between adjacent
                    // #' values.
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `from` -- Scalar integer giving the first integer
                    // #' in the sequence.
                    // #' * `to` -- Scalar integer giving the last integer in
                    // #' the sequence.
                    // #' * `length` -- Number of integers in the sequence.
                    // #' * `by` -- Integer scalar giving the difference
                    // #' between adjacent values in the sequence.
                    // #'
                    // #' ### Return
                    // #'
                    // #' * Column vector with a sequence of integers.
                    // #'
                    case MP2_COLON: // :

                    // #' ### Details
                    // #'
                    // #' The colon operator works much like the base R version
                    // #' \code{\link{:}}. It takes two scalar-valued integers
                    // #' and returns a column vector with all integers between
                    // #' the two inputs.
                    // #'
                        int from, to;
                        from = CppAD::Integer(args[0].coeff(0,0));
                        to = CppAD::Integer(args[1].coeff(0,0));
                        if (from>to) {
                            SetError(MP2_COLON, "Lower bound greater than upper bound in : operation", row);
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

                    // #' The `seq` function is a little different from the
                    // #' base R default, \code{\link{seq}}, in that it
                    // #' allows the user precise control over the length of
                    // #' the output through the `length` argument. The
                    // #' base R function gives the user this option, but not
                    // #' as the default.
                    // #'
                        int length, by;
                        from = CppAD::Integer(args[0].coeff(0,0));
                        length = CppAD::Integer(args[1].coeff(0,0));
                        by = CppAD::Integer(args[2].coeff(0,0));
                        if (length<=0) {
                            SetError(MP2_SEQUENCE, "Sequence length is less than or equal to zero in seq operation", row);
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

                    // #' ### Examples
                    // #'
                    // #' ```
                    // #' engine_eval(~ 1:10)
                    // #' engine_eval(~ seq(1, 10, 2))
                    // #' ```
                    // #'


                    // #' Replicate Elements
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `rep(x, times)` -- Replicate a column vector a
                    // #' number of times, by repeatedly stacking it on top of
                    // #' itself.
                    // #' * `rep_each` -- Not yet developed.
                    // #' * `rep_length` -- Not yet developed.
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `x` -- A scalar-valued variable to repeat.
                    // #' * `times` -- A scalar-valued integer variable giving
                    // #' the number of times to repeat `x`.
                    // #'
                    // #' ### Return
                    // #'
                    // #' * Column vector with `times` copies of `x` stacked
                    // #' on top of each other.
                    // #'
                    // #' ### Examples
                    // #'
                    // #' ```
                    // #' engine_eval(~ rep(1, 10))
                    // #' ```
                    // #'
                    case MP2_REPLICATE: // rep
                        //m = matrix<Type>::Constant(rows, 1, args[0].coeff(0,0));
                        rows = args[0].rows();
                        reps = CppAD::Integer(args[1].coeff(0,0));
                        m = matrix<Type>::Zero(rows * reps,1);
                        off = 0;
                        for (int i=0; i<reps; i++) {
                            m.block(off, 0, rows, 1) = args[0];
                            off += rows;
                        }
                        #ifdef MP_VERBOSE
                            std::cout << "rep(" << args[0] << ", " << args[1] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;

                    case MP2_MATRIX_MULTIPLY: // %*%

                    // #' ## Matrix Multiplication
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `x %*% y` -- Standard matrix multiplication.
                    // #' * `x %x% y` -- Kronecker product
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `x` -- A matrix. For the standard product, `x`
                    // #' must have as many columns as `y` has rows.
                    // #' * `y` -- A matrix. For standard product, `y`
                    // #' must have as many rows as `x` has columns.
                    // #'
                    // #' ### Return
                    // #'
                    // #' * The matrix product of `x` and `y`.
                    // #'
                    // #' ### Examples
                    // #'
                    // #' ```
                    // #' engine_eval(~ (1:10) %*% t(1:10))
                    // #' engine_eval(~ (1:10) %x% t(1:10))
                    // #' ```
                    // #'
                        #ifdef MP_VERBOSE
                            std::cout << args[0] << " %*% " << args[1] << " = " << args[0]*args[1] << std::endl << std::endl;
                        #endif
                        return args[0]*args[1];

                    case MP2_KRONECKER: // %x%

                        rows = args[0].rows() * args[1].rows();
                        cols = args[0].cols() * args[1].cols();
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<args[0].rows(); i++) {
                            for (int j=0; j<args[0].cols(); j++) {
                                m.block(i*args[1].rows(), j*args[1].cols(), args[1].rows(), args[1].cols()) = args[0].coeff(i, j) * args[1];
                            }
                        }
                        return m;

                    case MP2_ROUND_BRACKET: // (

                    // #' ## Parenthesis
                    // #'
                    // #' The order of operations can be enforced in the usual
                    // #' way with round parentheses, \code{\link{(}}.
                    // #'
                        return args[0];

                    // #' ## Reshaping and Combining Matrices
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `c(...)` -- Stack columns of arguments into a
                    // #' single column vector.
                    // #' * `cbind(...)` -- Create a matrix containing all of
                    // #' the columns of a group of matrices with the same
                    // #' number of rows.
                    // #' * `rbind(...)` -- Create a matrix containing all of
                    // #' the rows of a group of matrices with the same number
                    // #' of columns.
                    // #' * `matrix(x, rows, cols)` -- Reshape a matrix to have
                    // #' `rows` rows and `cols` columns. The input `x` must
                    // #' have `rows * cols` elements.
                    // #' * `t(x)` -- Standard matrix transpose.
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `...` -- Any number of dimensionally consistent
                    // #' matrices. The definition of dimensionally consistent
                    // #' depends on the function.
                    // #' * `x` -- Can be any matrix for `t`, but for `matrix`
                    // #' it must have `rows * cols` elements.
                    // #' * `rows` -- Scalar integer giving the number of
                    // #' rows in the output.
                    // #' * `cols` -- Scalar integer giving the number of
                    // #' columns in the output.
                    // #'
                    // #' ### Return
                    // #'
                    // #' * A combined or reshaped matrix.
                    // #'
                    // #' ### Details
                    // #'
                    case MP2_COMBINE: // c

                    // #' Any number of column vectors can be combined into a
                    // #' bigger column vector.
                    // #'
                        // std::cout << "in c(...)" << std::endl;
                        size = 0;
                        for (int i=0; i<n; i++) {
                            size += args[i].rows() * args[i].cols();
                        }
                        m = matrix<Type>::Zero(size,1);
                        off = 0;
                        for (int i=0; i<n; i++) {
                            cols = args[i].cols();
                            for (int j=0; j<cols; j++) {
                                rows = args[i].rows();
                                // std::cout << "number of rows in c(...): " << rows << std::endl;
                                if (rows != 0) { // avoid adding empty matrices
                                  // std::cout << "adding rows" << std::endl;
                                  m.block(off, 0, rows, 1) = args[i].col(j);
                                  off += rows;
                                }
                            }
                        }

                        #ifdef MP_VERBOSE
                            std::cout << "c(" << args[0] << ", ...," << args[n-1] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;

                    // #' Column and row vectors of the same length can be
                    // #' combined using the \code{\link{cbind}} and
                    // #' \code{\link{rbind}} functions respectively
                    // #'
                    case MP2_CBIND:
                    {
                        rows = args[0].rows();
                        // std::cout << "rows: " << rows << std::endl;
                        // std::cout << "n: " << n << std::endl;
                        int cols_per_arg;
                        int totcols, colmarker;
                        totcols = 0;
                        colmarker = 0;
                        for (int j=0; j<n; j++){
                            totcols += args[j].cols();
                        }
                        m = matrix<Type>::Zero(rows, totcols);
                        for (int i=0; i<n; i++) {
                            if (args[i].rows()==rows){
                                cols_per_arg = args[i].cols();
                                for (int k=0; k<cols_per_arg; k++){
                                    m.col(colmarker+k) = args[i].col(k);
                                }
                                colmarker += cols_per_arg;
                            }
                            else {
                                SetError(MP2_CBIND, "Inconsistent size in cbind function", row);
                                return m;
                            }
                        }
                    }
                        //m = matrix<Type>::Zero(rows, 1);
                        return m;
                    case MP2_RBIND:
                    {
                        cols = args[0].cols();
                        // std::cout << "cols: " << cols << std::endl;
                        // std::cout << "n: " << n << std::endl;
                        int rows_per_arg;
                        int totrows, rowmarker;
                        totrows = 0;
                        rowmarker = 0;
                        for (int j=0; j<n; j++){
                            totrows += args[j].rows();
                        }
                        m = matrix<Type>::Zero(totrows, cols);
                        for (int i=0; i<n; i++) {
                            if (args[i].cols()==cols){
                                rows_per_arg = args[i].rows();
                                for (int k=0; k<rows_per_arg; k++){
                                    m.row(rowmarker+k) = args[i].row(k);
                                }
                                rowmarker += rows_per_arg;
                            }
                            else {
                                SetError(MP2_RBIND, "Inconsistent size in rbind function", row);
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
                    // #' takes the same arguments.
                    // #' On the other hand, this function differs substantially
                    // #' from the base R version in that it must be filled
                    // #' by column and there is no `byrow` option.
                    // #'
                        m = args[0];

                        rows = CppAD::Integer(args[1].coeff(0,0));
                        cols = CppAD::Integer(args[2].coeff(0,0));

                        //m.conservativeResize(rows, cols); // don't know why this doesn't work
                        m.resize(rows, cols);

                        // m2 = m.transpose(); // m = m.transpose() doesn't work !!!
                        m2 = m;

                        #ifdef MP_VERBOSE
                            std::cout << "matrix(" << args[0] << ") reshaped into [" << rows << ", " << cols << "] = " \
                                      << m2 << std::endl << std::endl;
                        #endif

                        return m2;

                    // #' Matrices can be transposed with the usual
                    // #' function, \code{\link{t}}.
                    // #'
                    case MP2_TRANSPOSE: // t or transpose
                        m = args[0].transpose();
                        return m;

                    // #' ### Examples
                    // #'
                    // #' ```
                    // #' engine_eval(~ c(a, b, c), a = 1, b = 10:13, c = matrix(20:25, 3, 2))
                    // #' engine_eval(~ cbind(a, 10 + a), a = 0:3)
                    // #' engine_eval(~ rbind(a, 10 + a), a = t(0:3))
                    // #' engine_eval(~ matrix(1:12, 4, 3))
                    // #' engine_eval(~ t(1:3))
                    // #' ```
                    // #'


                    // #' ## Matrix Diagonals
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `to_diag(x)` -- Create a diagonal matrix by setting
                    // #' the diagonal to a column vector, `x`.
                    // #' * `from_diag(x)` -- Extract the diagonal from a
                    // #' matrix, `x`, and return the diagonal as a column
                    // #' vector.
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `x` -- Any matrix (for `from_diag`) or a
                    // #' column vector (for `to_diag`). It is common to assume
                    // #' that `x` is square for `from_diag` but this is
                    // #' not required.
                    // #'
                    // #' ### Return
                    // #'
                    // #' * `to_diag(x)` -- Diagonal matrix with `x` on the
                    // #' diagonal.
                    // #' * `from_diag(x)` -- Column vector containing the
                    // #' diagonal of `x`. A value is considered to be on
                    // #' the diagonal if it has a row index equal to
                    // #' the column index.
                    // #'
                    // #' ### Details
                    // #'
                    // #' The `to_diag` function can be used to produce a
                    // #' diagonal matrix by setting a column vector equal
                    // #' to the desired diagonal. The `from_diag` does
                    // #' (almost) the opposite, which is to get a column vector
                    // #' containing the diagonal of an existing matrix.
                    // #'
                    // #' ### Examples
                    // #'
                    // #' ```
                    // #' engine_eval(~from_diag(matrix(1:9, 3, 3)))
                    // #' engine_eval(~to_diag(from_diag(matrix(1:9, 3, 3))))
                    // #' engine_eval(~from_diag(to_diag(from_diag(matrix(1:9, 3, 3)))))
                    // #' ```
                    // #'
                    case MP2_TO_DIAG: // to_diag
                        rows = args[0].rows();
                        m = matrix<Type>::Zero(rows, rows);
                        for (int i=0; i<rows; i++){
                            m.coeffRef(i, i) = args[0].coeff(i, 0);
                        }
                        return m;

                    case MP2_FROM_DIAG: // from_diag
                        m = args[0].diagonal();
                        return m;




                    // #' ## Summarizing Matrix Values
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `sum(...)` -- Sum all of the elements of all of the
                    // #' matrices passed to `...`.
                    // #' * `colSums(x)` -- Row vector containing the sums
                    // #' of each column.
                    // #' * `rowSums(x)` -- Column vector containing the sums
                    // #' of each row.
                    // #' * `groupSums(x, f, n)` -- Column vector containing the
                    // #' sums of groups of elements in `x`. The groups are
                    // #' determined by the integers in `f` and the order of
                    // #' the sums in the output is determined by these
                    // #' integers.
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `...` -- Any number of matrices of any shape.
                    // #' * `x` -- A matrix of any dimensions, except for
                    // #' `groupSums` that expects `x` to be a column vector.
                    // #' * `f` -- A column vector the same length as `x`
                    // #' containing integers between `0` and `n-`.
                    // #' * `n` -- Length of the output column vector.
                    // #'
                    // #' ### Return
                    // #'
                    // #' * A matrix containing sums of various groups of
                    // #' the elements of `x`.
                    // #'
                    case MP2_SUM: // sum

                        m = matrix<Type>::Zero(1,1);
                        sum = 0.0;
                        for (int i=0; i<n; i++)
                            sum += args[i].sum();
                        m.coeffRef(0,0) = sum;

                        #ifdef MP_VERBOSE
                            std::cout << "sum(" << args[0] << ", ..., " << args[n-1] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;

                    // #' ### Details
                    // #'
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
                        //m = matrix<Type>::Zero(args[0].rows(), 1);
                        m = args[0].rowwise().sum().matrix();
                        #ifdef MP_VERBOSE
                            std::cout << "rowSums(" << args[0] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;

                    case MP2_COLSUMS: // colSums
                        m = args[0].colwise().sum().matrix();
                        #ifdef MP_VERBOSE
                            std::cout << "colSums(" << args[0] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;

                    case MP2_GROUPSUMS: // groupSums
                        // rows = CppAD::Integer(args[1].maxCoeff()+0.1f) + 1;
                        rows = CppAD::Integer(args[2].coeff(0,0)+0.1f);
                        m = matrix<Type>::Zero(rows, 1);
                        for (int i = 0; i < args[0].rows(); i++) {
                            rowIndex = CppAD::Integer(args[1].coeff(i,0)+0.1f);
                            m.coeffRef(rowIndex,0) += args[0].coeff(i,0);
                        }
                        return m;
                    // #' ### Examples
                    // #'
                    // #' ```
                    // #' x = 1
                    // #' y = 1:3
                    // #' A = matrix(1:12, 4, 3)
                    // #' engine_eval(~ sum(y), y = y)
                    // #' engine_eval(~sum(x, y, A), x = x, y = y, z = z)
                    // #' engine_eval(~ colSums(A), A = A)
                    // #' engine_eval(~ rowSums(A), A = A)
                    // #' engine_eval(~ groupSums(x, f, n), x = 1:10, f = rep(0:3, 1:4), n = 4)
                    // #' ```
                    // #'

                    // #' ## Extracting Matrix Elements
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `x[i,j]` -- Matrix containing a subset
                    // #' of the rows and columns of `x`.
                    // #' * `block(x,i,j,n,m)` -- Matrix containing a
                    // #' contiguous subset of rows and columns of `x`
                    // #' \url{https://eigen.tuxfamily.org/dox/group__TutorialBlockOperations.html}
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `x` -- Any matrix.
                    // #' * `i` -- An integer column vector (for `[`) or
                    // #' integer scalar (for `block`) containing the indices
                    // #' of the rows to extract (for `[`) or the index of the
                    // #' first row to extract (for `block`).
                    // #' * `j` -- An integer column vector (for `[`) or
                    // #' integer scalar (for `block`) containing the indices
                    // #' of the columns to extract (for `[`) or the index of
                    // #' the first column to extract (for `block`).
                    // #' * `n` -- Number of rows in the block to return.
                    // #' * `m` -- Number of columns in the block to return.
                    // #'
                    // #' ### Return
                    // #'
                    // #' * A matrix containing a subset of the rows and columns
                    // #' in `x`.
                    // #'
                    // #' ### Details
                    // #'
                    // #' Note that zero-based indexing is used
                    // #' so the first row/column gets index, `0`, etc.
                    // #'
                    // #' ### Examples
                    // #'
                    // #' ```
                    // #' engine_eval(~ A[c(3, 1, 2), 2], A = matrix(1:12, 4, 3))
                    // #' ```
                    // #'
                    case MP2_SQUARE_BRACKET: // [
                        #ifdef MP_VERBOSE
                            std::cout << "square bracket" << std::endl << std::endl;
                        #endif



                        int nrow;
                        int ncol;
                        nrow = args[1].size();

                        if(n==2){
                            m1 = matrix<Type>::Zero(1,1);
                            ncol=1;
                        }
                        else{
                            ncol = args[2].size();
                            m1 = args[2];
                        }
                        m = matrix<Type>::Zero(nrow,ncol);

                        err_code = CheckIndices(args[0], args[1], m1);
                        if (err_code) {
                            SetError(MP2_SQUARE_BRACKET, "Illegal index to square bracket", row);
                            return m;
                        }

                        // if we can assume contiguous sets of rows and columns
                        // then mat.block(...) will be faster, so should we
                        // have a block function on the R side when speed
                        // matters?
                        // Can we vectorize CppAD::Integer casting??
                        for (int i=0; i<nrow; i++) {
                            for (int j=0; j<ncol; j++) {
                                rowIndex = CppAD::Integer(args[1].coeff(i,0));
                                colIndex = CppAD::Integer(m1.coeff(j,0));
                                m.coeffRef(i,j) = args[0].coeff(rowIndex, colIndex);
                            }
                        }
                        return m;

                    case MP2_BLOCK: // block
                        rowIndex = CppAD::Integer(args[1].coeff(0,0));
                        colIndex = CppAD::Integer(args[2].coeff(0,0));
                        rows = CppAD::Integer(args[3].coeff(0,0));
                        cols = CppAD::Integer(args[4].coeff(0,0));
                        return args[0].block(rowIndex, colIndex, rows, cols);

                    // #' ## Accessing Past Values in the Simulation History
                    // #'
                    // #' For matrices with their simulation history saved,
                    // #' it is possible to bind the rows or columns of past
                    // #' versions of such matrices into a single matrix.
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `rbind_lag(x, lag, t_min)` -- Bind the rows of versions of
                    // #' `x` that were recorded at the end of all
                    // #' simulation iterations corresponding to time lags given
                    // #' by integers in `lag`.
                    // #' * `rbind_time(x, t, t_min)` -- Bind the rows of versions of
                    // #' `x` that were recorded at the end of all
                    // #' simulation iterations corresponding to integers in
                    // #' `t`.
                    // #' * `cbind_lag(x, lag, t_min)` -- Bind the columns of versions of
                    // #' `x` that were recorded at the end of all
                    // #' simulation iterations corresponding to time lags given
                    // #' by integers in `lag`. (TODO -- cbind_lag is not developed yet)
                    // #' * `cbind_time(x, t, t_min)` -- Bind the columns of versions of
                    // #' `x` that were recorded at the end of all
                    // #' simulation iterations corresponding to integers in
                    // #' `t`. (TODO -- cbind_lag is not developed yet)
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `x` -- Any matrix with saved history such that the
                    // #' number of columns (for `rbind_*`) or rows (for
                    // #' `cbind_*`) does not change throughout the simulation.
                    // #' * `lag` -- Column vector of integers giving numbers
                    // #' of time steps before the current step to obtain
                    // #' past values of `x`.
                    // #' * `t` -- Column vector of integers giving time steps
                    // #' at which to obtain past values of `x`.
                    // #' * `t_min` -- Minimum time step that is allowed to be
                    // #' accessed. All time-steps in `t` or implied by `lag`
                    // #' that are before `t_min` are ignored.
                    // #'
                    // #' ### Return
                    // #'
                    // #' * A matrix containing values of `x` from past times.
                    // #'
                    case MP2_RBIND_LAG:
                        args[1] = -args[1];
                        args[1].array() += t; // += t+0.1f; // +0.1 won't work when t<0
                    case MP2_RBIND_TIME:
                        if (t == 0) {
                            SetError(154, "The simulation loop has not yet begun and so rbind_time (or rbind_lag) cannot be used", row);
                            return args[0];
                        }
                        matIndex = index2mats[0]; // m

                        if (n == 1) {
                            timeIndex = matrix<Type>::Zero(t - 1, 1);
                            for (int i=0; i < t - 1; i++) {
                                timeIndex.coeffRef(i, 0) = i + 1;
                            }
                        } else {
                            timeIndex = args[1];
                        }
                        if (mats_save_hist[matIndex]==0 && !(timeIndex.size()==1 && CppAD::Integer(timeIndex.coeff(0,0))==t)) {
                            SetError(MP2_RBIND_TIME, "Can only rbind_time (or rbind_lag) initialized matrices with saved history", row);
                            return args[0];
                        }

                        int lowerTimeBound;
                        if (table_n[row]==3)
                            lowerTimeBound = CppAD::Integer(args[2].coeff(0,0));
                        else
                            lowerTimeBound = 0;

                        // Get the length of legitimate times in rbind_time.
                        // Check if the shape of the matrix changes.
                        //    Error if yes or assign variables "rows" and "cols" with
                        //    the correct values otherwise.
                        int rbind_length, nRows, nCols;
                        rbind_length = 0; // count of legitimate time steps to select
                        for (int i=0; i<timeIndex.size(); i++) {
                            rowIndex = CppAD::Integer(timeIndex.coeff(i,0));
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
                                    SetError(MP2_RBIND_TIME, "Inconsistent rows or columns in rbind_time (or rbind_lag)", row);
                                    return args[0];
                                }
                            }

                            rbind_length++;
                        }
                        #ifdef MP_VERBOSE
                            std::cout << "rbind_time(" << timeIndex << ") = " << std::endl;
                        #endif

                        if (rbind_length>0) {
                            //rows = hist[0].m_matrices[matIndex].rows();
                            //cols = hist[0].m_matrices[matIndex].cols();
                            m = matrix<Type>::Zero(rbind_length*rows, cols);
                            rbind_length = 0;
                            for (int i=0; i<timeIndex.size(); i++) {
                                rowIndex = CppAD::Integer(timeIndex.coeff(i,0));
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

                    // #' ## Time Indexing
                    // #'
                    // #' Get the index of current or lagged time step or
                    // #' the index of the current time group. A time group
                    // #' is a contiguous set of time steps defined by two
                    // #' change points.
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `time_step(lag)`: Get the time-step associated
                    // #' with a particular lag from the current time-step.
                    // #' If the lagged time-step is less than zero, the
                    // #' function returns zero.
                    // #' * `time_group(index, change_points)`: Update the
                    // #' `index` associated with the current time group.
                    // #' The current group is defined by the minimum
                    // #' of all elements of `change_points` that are
                    // #' greater than the current time step. The time group
                    // #' `index` is the index associated with this element.
                    // #' Please see the examples below, they are easier
                    // #' to understand than this explanation.
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `lag`: Number of time-steps to look back for
                    // #' the time-step to return.
                    // #' * `index`: Index associated with the current time
                    // #' group.
                    // #' * `change_points`: Increasing column vector of
                    // #' time steps giving the lower bound of each time
                    // #' group.
                    // #'
                    // #' ### Return
                    // #'
                    // #' A 1-by-1 matrix with the time-step `lag` steps
                    // #' ago, or with zero if `t+1 < lag`
                    // #'
                    // #' ### Examples
                    // #'
                    // #' ```
                    // #' simple_sims(
                    // #'   iteration_exprs = list(x ~ time_step(0)),
                    // #'   time_steps = 10,
                    // #'   x = empty_matrix
                    // #' )
                    // #' sims = simple_sims(
                    // #'   iteration_exprs = list(
                    // #'     j ~ time_group(j, change_points),
                    // #'     time_varying_parameter ~ time_variation_schedule[j]
                    // #'   ),
                    // #'   time_steps = 10,
                    // #'   j = 0,
                    // #'   change_points = c(0, 4, 7),
                    // #'   time_variation_schedule = c(42, pi, sqrt(2)),
                    // #'   time_varying_parameter = empty_matrix
                    // #' )
                    // #' ```
                    // #'
                    case MP2_TIME_STEP: // time_step(lag)
                        m = matrix<Type>::Zero(1,1);
                        lag = CppAD::Integer(args[0].coeff(0,0));
                        if (lag < 0) {
                            SetError(MP2_TIME_STEP, "Time lag needs to be non-negative", row);
                            return m;
                        }
                        if (t > lag) {
                            m.coeffRef(0,0) = t - lag;
                        }
                        return m;

                    case MP2_TIME_GROUP: // time_group(i, change_points)
                        m = args[0];
                        off = CppAD::Integer(args[0].coeff(0, 0));
                        cp = CppAD::Integer(args[1].coeff(off + 1, 0));
                        if (cp == t) {
                            m.coeffRef(0,0) = off + 1;
                        }
                        return m;

                    case MP2_CONVOLUTION:

                    // #' ## Convolution
                    // #'
                    // #' One may take the convolution of each element in a
                    // #' matrix, x, over simulation time using a kernel, k.
                    // #' There are two arguments of this function.
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `convolution(x, k)`
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `x` -- The matrix containing elements to be
                    // #' convolved.
                    // #' * `k` -- A column vector giving the convolution kernel.
                    // #'
                    // #' ### Return
                    // #'
                    // #' A matrix the same size as `x` but with the
                    // #' convolutions, \eqn{y_{ij}}, of each element,
                    // #' \eqn{x_{ij}}, given by the following.
                    // #'
                    // #' \deqn{y_{ij} = \sum_{\tau = 0} x_{ij}(t-\tau) k(\tau)}
                    // #'
                    // #' unless \eqn{t < \tau}, in which case,
                    // #'
                    // #' \deqn{y_{ij} = }
                    // #'
                    // #' where \eqn{y_{ij}} is the convolution,
                    // #' \eqn{x_{ij}(t)} is the value of \eqn{x_{ij}} at time step, \eqn{t},
                    // #' \eqn{k(\tau)} is the value of the kernel at lag, \eqn{\tau},
                    // #' and \eqn{\lambda} is the length of the kernel.
                    // #'
                    // #' ### Details
                    // #'
                    // #' If any empty matrices are encountered when looking
                    // #' back in time, they are treated as matrices with all
                    // #' zeros. Similarly, any matrices encounte
                    // #' of `x`
                    // #'
                        matIndex = index2mats[0]; // m
                        #ifdef MP_VERBOSE
                            std::cout << "matIndex: " << matIndex << std::endl << std::endl;
                        #endif
                        length = args[1].rows();
                        #ifdef MP_VERBOSE
                            std::cout << "length: " << length << std::endl << std::endl;
                        #endif
                        if (length>0 && args[1].cols()==1) {
                            #ifdef MP_VERBOSE
                                std::cout << "kernel 1: " << args[1] << std::endl << std::endl;
                            #endif
                            if (t+1<length) {
                                length = t+1;
                                args[1] = args[1].block(0, 0, length, 1);
                            }
                            #ifdef MP_VERBOSE
                                std::cout << "kernel 2: " << args[1] << std::endl << std::endl;
                            #endif

                            rows = args[0].rows();
                            cols = args[0].cols();
                            m = matrix<Type>::Zero(rows, cols);

                            for (int i=0; i<rows; i++)
                                for (int j=0; j<cols; j++)
                                    m.coeffRef(i,j) = args[1].coeff(0,0) * valid_vars.m_matrices[matIndex].coeff(i,j);

                            for (int k=1; k<=length-1; k++)
                                if (hist[t-k].m_matrices[matIndex].rows()!=0 &&
                                    hist[t-k].m_matrices[matIndex].cols()!=0)
                                    for (int i=0; i<rows; i++)
                                        for (int j=0; j<cols; j++)
                                            m.coeffRef(i,j) += args[1].coeff(k,0) * hist[t-k].m_matrices[matIndex].coeff(i,j);

                            return m;
                        }
                        else {
                            SetError(MP2_CONVOLUTION, "Either empty or non-column vector used as kernel in convolution", row);
                            return m;
                        }

                    // #' ## Clamp
                    // #'
                    // #' Smoothly clamp the elements of a matrix so that they
                    // #' do not get closer to 0 than a tolerance, `eps`, with
                    // #' a default of 1e-12. The output of the `clamp`
                    // #' function is as follows.
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `clamp(x, eps)`
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `x` : A matrix with elements that should remain positive.
                    // #' * `eps` : A small positive number giving the
                    // #' theoretical minimum of the elements in the returned
                    // #' matrix.
                    case MP2_CLAMP:
                        eps = 1e-12;  // default
                        if (n == 2) eps = args[1].coeff(0,0);
                        rows = args[0].rows();
                        cols = args[0].cols();
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                           for (int j=0; j<cols; j++) {
                               m.coeffRef(i,j) = args[0].coeff(i,j) + eps * (1.0 / (1.0-(args[0].coeff(i,j)-eps)/eps + ((args[0].coeff(i,j)-eps)*(args[0].coeff(i,j)-eps))/(eps*eps)));
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
                    // #'
                    // #' The `simulated` argument gives a matrix of means for
                    // #' the `observed` values at which the densities are
                    // #' being evaluated. Additional arguments are other
                    // #' distributional parameters such as the standard
                    // #' deviation or dispersion parameter. All densities
                    // #' are given as log-densities, so if you would like
                    // #' the density itself you must pass the result through
                    // #' the `exp` function.
                    // #'
                    // #' If the `simulated` matrix or the additional parameter
                    // #' matrices have either a single row or
                    // #' single column, these singleton rows and columns are
                    // #' repeated to match the number of rows and columns in
                    // #' the `observed` matrix. This feature allows one
                    // #' to do things like specify a single common mean for
                    // #' several values.
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `dpois(observed, simulated)` -- Log of the Poisson density
                    // #' based on this [dpois](https://kaskr.github.io/adcomp/group__R__style__distribution.html#gaa1ed15503e1441a381102a8c4c9baaf1)
                    // #' TMB function.
                    // #' * `dnbinom(observed, simulated, over_dispersion)` --
                    // #' Log of the negative binomial density based on this [dnbinom](https://kaskr.github.io/adcomp/group__R__style__distribution.html#ga76266c19046e04b651fce93aa0810351)
                    // #' TMB function. To get the variance that this function
                    // #' requires we use this expression, \code{simulated + simulated^2/over_dispersion},
                    // #' following p.165 in this [book](https://ms.mcmaster.ca/~bolker/emdbook/book.pdf)
                    // #' * `dnorm(observed, simulated, standard_deviation)` --
                    // #' Log of the normal density based on this [dnorm](https://kaskr.github.io/adcomp/dnorm_8hpp.html)
                    // #' TMB function.
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `observed` -- Matrix of observed values
                    // #' at which the density is being evaluated.
                    // #' * `simulated` -- Matrix of distributional means,
                    // #' with singleton rows and columns recycled to match
                    // #' the numbers of rows and columns in `observed`.
                    // #' * `over_dispersion` -- Over-dispersion parameter
                    // #' given by \code{(simulated/standard_deviation)^2 - simulated)}.
                    // #' * `standard_deviation` -- Standard deviation parameter.
                    // #'
                    case MP2_POISSON_DENSITY:
                        if (n < 2) {
                            SetError(MP2_POISSON_DENSITY, "dpois needs two arguments: matrices with observed and expected values", row);
                            return m;
                        }
                        rows = args[0].rows();
                        cols = args[0].cols();
                        err_code = RecycleInPlace(args[1], rows, cols);
                        if (err_code != 0) {
                          SetError(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", row);
                          return m;
                        }
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                            for (int j=0; j<cols; j++) {
                                m.coeffRef(i,j) = dpois(args[0].coeff(i,j), args[1].coeff(i,j), 1);
                            }
                        }
                        return m;

                    case MP2_NEGBIN_DENSITY:
                        if (n < 3) {
                            SetError(MP2_NEGBIN_DENSITY, "dnbinom needs three arguments: matrices with observed values, expected values, and dispersion parameters", row);
                            return m;
                        }
                        rows = args[0].rows();
                        cols = args[0].cols();
                        err_code1 = RecycleInPlace(args[1], rows, cols);
                        err_code2 = RecycleInPlace(args[2], rows, cols);
                        err_code = err_code1 + err_code2;
                        if (err_code != 0) {
                            SetError(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", row);
                            return m;
                        }
                        //   var ~ variance
                        //   mu ~ mean
                        //   k ~ overdispersion parameter = sp[this->spi[0]]
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                            for (int j=0; j<cols; j++) {
                                // p.165: https://ms.mcmaster.ca/~bolker/emdbook/book.pdf
                                // mu ~ mean -- args[1]
                                // k ~ overdispersion -- args[2].coeff(i,j)
                                // var = mu + mu^2/k
                                var = args[1].coeff(i,j) + ((args[1].coeff(i,j)*args[1].coeff(i,j)) / args[2].coeff(i,j));
                                m.coeffRef(i,j) = dnbinom2(args[0].coeff(i,j), args[1].coeff(i,j), var, 1);
                            }
                        }
                        return m;

                    case MP2_NORMAL_DENSITY:
                        if (n < 3) {
                            SetError(MP2_NORMAL_DENSITY, "dnorm needs three arguments: matrices with observed values, expected values, and standard deviation parameters", row);
                            return m;
                        }
                        rows = args[0].rows();
                        cols = args[0].cols();
                        err_code1 = RecycleInPlace(args[1], rows, cols);
                        err_code2 = RecycleInPlace(args[2], rows, cols);
                        err_code = err_code1 + err_code2;
                        if (err_code != 0) {
                            SetError(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", row);
                            return m;
                        }
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                            for (int j=0; j<cols; j++) {
                                m.coeffRef(i,j) = dnorm(args[0].coeff(i,j), args[1].coeff(i,j), args[2].coeff(i,j), 1);
                            }
                        }
                        return m;

                    // #' ## Pseudo-Random Number Generators
                    // #'
                    // #' All random number generator functions have `mean`
                    // #' as the first argument. Subsequent arguments give
                    // #' additional distributional parameters.
                    // #' Singleton rows and columns in the matrices passed to
                    // #' the additional distributional parameters are recycled
                    // #' so that all arguments have the same number of rows
                    // #' and columns. All functions return a matrix the same
                    // #' shape as `mean` but with pseudo-random numbers
                    // #' deviating from each mean in the `mean` matrix.
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `rpois(mean)` -- Pseudo-random Poisson distributed
                    // #' values.
                    // #' * `rnbinom(mean, over_dispersion)` -- Pseudo-random
                    // #' negative binomially distributed values.
                    // #' * `rnorm(mean, standard_deviation)` -- Pseudo-random
                    // #' normal values.
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `mean` -- Matrix of means about which to simulate
                    // #' pseudo-random variation.
                    // #' * `over_dispersion` -- Matrix of over-dispersion parameters
                    // #' given by \code{(simulated/standard_deviation)^2 - simulated)}.
                    // #' * `standard_deviation` -- Matrix of standard deviation parameters.
                    // #'
                    case MP2_POISSON_SIM:
                        rows = args[0].rows();
                        cols = args[0].cols();
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                            for (int j=0; j<cols; j++) {
                                m.coeffRef(i,j) = rpois(args[0].coeff(i,j));
                            }
                        }
                        return m;

                    case MP2_NEGBIN_SIM:
                        if (n < 2) {
                            SetError(MP2_NEGBIN_SIM, "rnbinom needs two arguments: matrices with means and dispersion parameters", row);
                            return m;
                        }
                        eps = 1e-8;
                        rows = args[0].rows();
                        cols = args[0].cols();
                        err_code = RecycleInPlace(args[1], rows, cols);
                        if (err_code != 0) {
                            SetError(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", row);
                            return m;
                        }
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                            for (int j=0; j<cols; j++) {
                                var = args[0].coeff(i,j) + ((args[0].coeff(i,j)*args[0].coeff(i,j)) / args[1].coeff(i,j));
                                if (var < eps)
                                    // more numerically stable to just set the simulations
                                    // to the mean when the var is low
                                    m.coeffRef(i,j) = args[0].coeff(i,j);
                                else
                                    m.coeffRef(i,j) = rnbinom2(args[0].coeff(i,j), var);
                            }
                        }
                        return m;

                    case MP2_NORMAL_SIM:
                        if (n < 2) {
                            SetError(MP2_NORMAL_SIM, "rnorm needs two arguments: matrices with means and standard deviations", row);
                            return m;
                        }
                        rows = args[0].rows();
                        cols = args[0].cols();
                        err_code = RecycleInPlace(args[1], rows, cols);
                        if (err_code != 0) {
                            SetError(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", row);
                            return m;
                        }
                        m = matrix<Type>::Zero(rows, cols);
                        for (int i=0; i<rows; i++) {
                            for (int j=0; j<cols; j++) {
                                m.coeffRef(i,j) = rnorm(args[0].coeff(i,j), args[1].coeff(i,j));
                            }
                        }
                        return m;

                    case MP2_ASSIGN:
                    // #' ## Assign
                    // #'
                    // #' Assign values to a subset of the elements in a matrix.
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `assign(x, i, j, v)`
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `x` -- Matrix with elements that are to be updated
                    // #' by the values in `v`.
                    // #' * `i` -- Column vector of row indices pointing to
                    // #' the elements of `x` to be updated. These indices are
                    // #' paired with those in `v`. If the length of
                    // #' `i` does not equal that of `v`, then it must have a
                    // #' single index that gets paired with every element of
                    // #' `v`.
                    // #' * `j` -- Column vector of column indices pointing to
                    // #' the elements of `x` to be updated. These indices are
                    // #' paired with those in `v`. If the length of
                    // #' `j` does not equal that of `v`, then it must have a
                    // #' single index that gets paired with every element of
                    // #' `v`.
                    // #' * `v` -- Column vector of values to replace elements
                    // #' of `x` at locations given by `i` and `j`.
                    // #'
                    // #' ### Return
                    // #'
                    // #' The `assign` function is not called for its return
                    // #' value, which is an \code{\link{empty_matrix}}, but
                    // #' rather to modify `x` but replacing some of its
                    // #' components with those in `v`.
                    // #'
                    // #' ### Examples
                    // #'
                    // #' ```
                    // #' x = matrix(1:12, 3, 4)
                    // #' engine_eval(~ x + 1, x = x)
                    // #' engine_eval(~ x + 1, x = x, .matrix_to_return = "x")
                    // #' engine_eval(~ assign(x, 2, 1, 100), x = x, .matrix_to_return = "x")
                    // #' engine_eval(~ assign(x
                    // #'   , c(2, 1, 0)
                    // #'   , 0
                    // #'   , c(100, 1000, 10000)
                    // #' ), x = x, .matrix_to_return = "x")
                    // #'
                    // #' ```
                    // #'

                        cols = args[1].cols();
                        if (cols != 1) {
                            SetError(255, "Assignment index matrices must have a single column", row);
                            return m;
                        }
                        cols = args[2].cols();
                        if (cols != 1) {
                            SetError(255, "Assignment index matrices must have a single column", row);
                            return m;
                        }
                        cols = args[3].cols();
                        if (cols != 1) {
                            SetError(255, "Assignment value matrices must have a single column", row);
                            return m;
                        }
                        err_code = CheckIndices(args[0], args[1], args[2]);
                        if (err_code) {
                            SetError(MP2_ASSIGN, "Illegal index used in assign", row);
                            return m;
                        }

                        rows = args[3].rows();
                        err_code1 = RecycleInPlace(args[1], rows, cols);
                        err_code2 = RecycleInPlace(args[2], rows, cols);
                        err_code = err_code1 + err_code2;
                        if (err_code != 0) {
                            SetError(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", row);
                            return m;
                        }

                        for (int k=0; k<rows; k++) {
                            rowIndex = CppAD::Integer(args[1].coeff(k,0));
                            colIndex = CppAD::Integer(args[2].coeff(k,0));
                            valid_vars.m_matrices[index2mats[0]].coeffRef(rowIndex,colIndex) = args[3].coeff(k,0);
                        }
                        return m2; // empty matrix


                    case MP2_UNPACK:
                    // #' ## Unpack
                    // #'
                    // #' Unpack elements of a matrix into smaller matrices.
                    // #'
                    // #' ### Functions
                    // #'
                    // #' * `unpack(x, ...)`
                    // #'
                    // #' ### Arguments
                    // #'
                    // #' * `x` -- Matrix with elements to be distributed to
                    // #' the matrices passed through `...`.
                    // #' * `...` -- Matrices with elements to be replaced by
                    // #' the values of elements in `x` in column-major order.
                    // #'
                    // #' ### Return
                    // #'
                    // #' The `unpack` function is not called for its return
                    // #' value, which is an \code{\link{empty_matrix}}, but
                    // #' rather to modify the matrices in `...` by replacing
                    // #' at least some of its components with those in `x`.
                    // #'
                    // #' ### Examples
                    // #'
                    // #' Here we fill a matrix with integers from `1` to `12`
                    // #' and then unpack them one-at-a-time into two
                    // #' column vectors, `x` and `y`. By returning `y`
                    // #' we see the integers after the first three were
                    // #' used up by `x`.
                    // #' ```
                    // #' engine_eval(~unpack(matrix(1:12, 3, 4), x, y)
                    // #'   , x = rep(0, 3)
                    // #'   , y = rep(1, 5)
                    // #'   , .matrix_to_return = "y"
                    // #' )
                    // #' ```
                    // #'

                        // matIndex = index2mats[0]; // m
                        // valid_vars.m_matrices[matIndex]


                        m = args[0];
                        size = m.rows()*m.cols();
                        m.resize(size, 1);

                        start = 0;
                        for (int i=1; i<n; i++) {
                            sz = args[i].rows() * args[i].cols();
                            if (size>=sz) {
                                m1 = m.block(start, 0, sz, 1);
                                m1.resize(args[i].rows(), args[i].cols());
                                //std::cout << "MATRIX " << valid_vars.m_matrices[index2mats[i]] << std::endl << std::endl;
                                valid_vars.m_matrices[index2mats[i]] = m1;
                                // args[i] = m1;
                                size -= sz;
                                start += sz;
                            }
                            else
                                break;
                        }
                        return m2; // empty matrix

                    case MP2_RECYCLE:
                        m = args[0];
                        rows = CppAD::Integer(args[1].coeff(0,0));
                        cols = CppAD::Integer(args[2].coeff(0,0));
                        err_code = RecycleInPlace(m, rows, cols);
                        if (err_code != 0) {
                            SetError(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", row);
                            return m;
                        }
                        return m;

                    default:
                        SetError(255, "invalid operator in arithmetic expression", row);
                        return m;
                }
        }
    };


private:
    // Functor for computing derivatives of expressions.
    // template <class Type>
    // struct matrix_functor{
    //     // define data members
    //
    //     // define constructor
    //     matrix_functor() : // initialization list
    //     { // the body is empty
    //     }
    //     // the function itself
    //     template <typename T>
    //     vector<T> operator()(vector<T> input_vector_)
    //     {
    //         vector<T> output_vector_ = EvalExpr(
    //             simulation_history_,
    //             0,
    //             mats_save_hist_,
    //             p_table_x_,
    //             p_table_n_,
    //             p_table_i_,
    //             mats_,
    //             literals_,
    //             p_table_row_
    //         );
    //         // call exprEval in here to convert input_vector_ into output_vector_
    //         return (output_vector_);
    //     }
    // }
    unsigned char error_code;
    int expr_row;
    char error_message[256];
};

#define REPORT_ERROR { \
    int error = exprEvaluator.GetErrorCode(); \
    int expr_row = exprEvaluator.GetExprRow(); \
    REPORT(error); \
    REPORT(expr_row); \
 \
    logfile.open (LOG_FILE_NAME, std::ios_base::app); \
    logfile << "Error code = " << error << std::endl; \
    logfile << "Error message = " << exprEvaluator.GetErrorMessage() << std::endl; \
    logfile << "Expression row = " << expr_row << std::endl; \
    logfile.close(); \
}

template<class Type>
vector<ListOfMatrices<Type> > MakeSimulationHistory(
    const int time_steps,
    const vector<int>& mats_save_hist,
    ListOfMatrices<Type>& hist_shape_template
) {
    vector<ListOfMatrices<Type> > simulation_history(time_steps+2);
    matrix<Type> empty_matrix;
    for (int i=0; i<mats_save_hist.size(); i++)
        if (mats_save_hist[i]==0)
            hist_shape_template.m_matrices[i] = empty_matrix;

    return simulation_history;
}

// Helper function
template<class Type>
void UpdateSimulationHistory(
    vector<ListOfMatrices<Type> >& hist,
    int t,
    const ListOfMatrices<Type>& mats,
    const vector<int>& mats_save_hist,
    ListOfMatrices<Type>& hist_shape_template
) {
    // matrix<Type> emptyMat;
    // ListOfMatrices<Type> ms(mats);
    // if the history of the matrix is not to be saved,
    // just save a 1-by-1 with a zero instead to save space
    for (int i=0; i<mats_save_hist.size(); i++)
        if (mats_save_hist[i]!=0)
            hist_shape_template.m_matrices[i] = mats.m_matrices[i];

    hist[t] = hist_shape_template;
}


const char LOG_FILE_NAME[] = "macpan2.log";

// "main" function
template<class Type>
Type objective_function<Type>::operator() ()
{
    #ifdef MP_VERBOSE
        std::cout << "============== objective_function =============" << std::endl;
    #endif

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

    // Fixed parameter replacements
    DATA_IVECTOR(p_par_id);
    DATA_IVECTOR(p_mat_id);
    DATA_IVECTOR(p_row_id);
    DATA_IVECTOR(p_col_id);

    // Random parameter replacements
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

    // Simulation history
    /// each element of this history 'vector' is a list of the matrices
    /// in the model at a particular point in history
    ListOfMatrices<Type> hist_shape_template(mats);
    vector<ListOfMatrices<Type> > simulation_history = MakeSimulationHistory(
        time_steps,
        mats_save_hist,
        hist_shape_template
    );



    //////////////////////////////////
    // Define an expression evaluator
    ExprEvaluator<Type> exprEvaluator;
    //////////////////////////////////

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
        mats_save_hist,
        hist_shape_template
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
            mats_save_hist,
            hist_shape_template
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
        mats_save_hist,
        hist_shape_template
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

    #ifdef MP_VERBOSE
        std::cout << "======== end of objective function ========" << std::endl;
    #endif
    return ret.coeff(0,0);
}
