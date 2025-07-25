#define MP_VERBOSE
#define TMB_LIB_INIT R_init_macpan2
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <Eigen/Eigen>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <math.h> // isnan() is defined
#include <sys/time.h>
#include <sys/stat.h>
// https://github.com/kaskr/adcomp/wiki/Development#distributing-code

#include <Rcpp.h>
// include guarding
// (https://en.wikipedia.org/wiki/Include_guard)
// 
// Rcpp.h includes dnorm, but we want to use TMB's dnorm,
// which we include below.
// https://stackoverflow.com/a/64138126
#ifdef dnorm
#undef dnorm
#endif

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
// long as the integers increase from 1 without gaps and the
// R/enum.R file is regenerated.
enum macpan2_func
{
      MP2_ADD = 1 // binop: `+`(x, y)
    , MP2_SUBTRACT = 2 // binop: `-`(x, y)
    , MP2_MULTIPLY = 3 // binop: `*`(x, y)
    , MP2_DIVIDE = 4 // binop: `/`(x, y)
    , MP2_POWER = 5 // binop: `^`(x, y)
    , MP2_EXP = 6 // fwrap: exp(x)
    , MP2_LOG = 7 // fwrap: log(x)
    , MP2_ROUND_BRACKET = 8 // paren: `(`(...)
    , MP2_COMBINE = 9 // fwrap: c(...)
    , MP2_MATRIX = 10 // fwrap: matrix(x, i, j)
    , MP2_MATRIX_MULTIPLY = 11 // binop: `%*%`(x, y)
    , MP2_SUM = 12 // fwrap: sum(...)
    , MP2_REPLICATE = 13 // fwrap: rep(x, times)
    , MP2_ROWSUMS = 14 // fwrap: row_sums(x)
    , MP2_COLSUMS = 15 // fwrap: col_sums(x)
    , MP2_GROUPSUMS = 16 // fwrap: group_sums(x, f, n)
    , MP2_SQUARE_BRACKET = 17 // paren: `[`(x, i, j)
    , MP2_BLOCK = 18 // fwrap: block(x, i, j, n, m)
    , MP2_TRANSPOSE = 19 // fwrap: t(x)
    , MP2_RBIND_TIME = 20 // fwrap: rbind_time(x, t, t_min)
    , MP2_RBIND_LAG = 21 // fwrap: rbind_lag(x, lag, t_min)
    , MP2_CBIND_TIME = 22 // fwrap: cbind_time(x, t, t_min)
    , MP2_CBIND_LAG = 23 // fwrap: cbind_lag(x, lag, t_min)
    , MP2_COLON = 24 // binop: `:`(from, to)
    , MP2_SEQUENCE = 25 // fwrap: seq(from, length, by)
    , MP2_CONVOLUTION = 26 // fwrap: convolution(x, k)
    , MP2_CBIND = 27 // fwrap: cbind(...)
    , MP2_RBIND = 28 // fwrap: rbind(...)
    , MP2_TIME_STEP = 29 // fwrap: time_step(lag)
    , MP2_RECYCLE = 30 // fwrap: recycle(x, rows, cols)
    , MP2_CLAMP = 31 // fwrap: clamp(x, eps)
    , MP2_POISSON_DENSITY = 32 // fwrap: dpois(observed, simulated)
    , MP2_NEGBIN_DENSITY = 33 // fwrap: dnbinom(observed, simulated, over_dispersion)
    , MP2_NORMAL_DENSITY = 34 // fwrap: dnorm(observed, simulated, standard_deviation)
    , MP2_POISSON_SIM = 35 // fwrap: rpois(mean)
    , MP2_NEGBIN_SIM = 36 // fwrap: rnbinom(mean, over_dispersion)
    , MP2_NORMAL_SIM = 37 // fwrap: rnorm(mean, standard_deviation)
    , MP2_KRONECKER = 38 // binop: `%x%`(x, y)
    , MP2_TO_DIAG = 39 // fwrap: to_diag(x)
    , MP2_FROM_DIAG = 40 // fwrap: from_diag(x)
    , MP2_TIME_GROUP = 41 // fwrap: time_group(i, change_points)
    , MP2_COS = 42 // fwrap: cos(x)
    , MP2_PRINT = 43 // fwrap: print(x)
    , MP2_TIME_VAR = 44 // fwrap: time_var(x, change_points)
    , MP2_BINOM_SIM = 45 // fwrap: rbinom(size, probability)
    , MP2_EULER_MULTINOM_SIM = 46 // fwrap: reulermultinom(size, rate, delta_t)
    , MP2_ROUND = 47 // fwrap: round(x)
    , MP2_PGAMMA = 48 // fwrap: pgamma(q, shape, scale)
    , MP2_MEAN = 49 // fwrap: mean(x)
    , MP2_SD = 50 // fwrap: sd(x)
    , MP2_PROPORTIONS = 51 // fwrap: proportions(x)
    , MP2_LAST = 52 // fwrap: last(x)
    , MP2_CHECK_FINITE = 53 // fwrap: check_finite(x)
    , MP2_BINOM_DENSITY = 54 // fwrap: dbinom(observed, size, probability)
    , MP2_SIN = 55 // fwrap: sin(x)
    , MP2_SQRT = 56 // fwrap: sqrt(x)
    , MP2_PNORM = 57 // fwrap: pnorm(q, mean, sd)
    , MP2_INVLOGIT = 58 // fwrap: invlogit(x)
    , MP2_LOGIT = 59 // fwrap: logit(x)
    , MP2_CUMSUM = 60 // fwrap: cumsum(x)
    , MP2_ASSIGN = 61 // fwrap: assign(x, i, j, v)
    , MP2_UNPACK = 62 // fwrap: unpack(x, ...)
};

enum macpan2_meth
{
      METH_FROM_ROWS = 1 // ~ Y[i], "Y", "i"
    , METH_TO_ROWS = 2 // Y[i] ~ X, c("Y", "X"), "i"
    , METH_ROWS_TO_ROWS = 3 // Y[i] ~ X[j], c("Y", "X"), c("i", "j")
    , METH_MAT_MULT_TO_ROWS = 4 // Y[i] ~ A %*% X[j], c("Y", "A", "X"), c("i", "j")
    , METH_TV_MAT_MULT_TO_ROWS = 5 // Y[i] ~ time_var(A, change_points, block_size, change_pointer) %*% X[j], c("Y", "A", "X"), c("i", "j", "change_points", "block_size", "change_pointer")
    , METH_GROUP_SUMS = 6 // ~ groupSums(Y, i, n), "Y", c("i", "n")
    , METH_TV_MAT = 7 // ~ time_var(Y, change_points, block_size, change_pointer), "Y", c("change_points", "block_size", "change_pointer")
    , METH_ROWS_TIMES_ROWS = 8 // ~ A[i] * X[j], c("A", "X"), c("i", "j")
};

// functions that can only take numerical matrices -- no integer vectors
std::vector<int> mp_math = {
    MP2_ADD, MP2_SUBTRACT, MP2_MULTIPLY, MP2_DIVIDE, MP2_POWER, MP2_EXP, MP2_LOG
  , MP2_MATRIX, MP2_MATRIX_MULTIPLY
  , MP2_SUM, MP2_ROWSUMS, MP2_COLSUMS, MP2_TRANSPOSE
  , MP2_CONVOLUTION, MP2_CBIND, MP2_RBIND, MP2_RECYCLE, MP2_CLAMP
  , MP2_POISSON_DENSITY, MP2_NEGBIN_DENSITY, MP2_NORMAL_DENSITY
  , MP2_BINOM_DENSITY
  , MP2_POISSON_SIM, MP2_NEGBIN_SIM, MP2_NORMAL_SIM, MP2_KRONECKER
  , MP2_TO_DIAG, MP2_FROM_DIAG, MP2_COS, MP2_SIN, MP2_COS
  , MP2_BINOM_SIM, MP2_EULER_MULTINOM_SIM
  , MP2_ROUND, MP2_PGAMMA, MP2_PNORM
  , MP2_MEAN, MP2_SD, MP2_INVLOGIT, MP2_LOGIT
};

std::string bail_out_log_file = ".macpan2/bail-out/log.txt";


// functions that are elementwise binary operators
std::vector<int> mp_elementwise_binop = {
  MP2_ADD, MP2_SUBTRACT, MP2_MULTIPLY, MP2_DIVIDE, MP2_POWER
};

// functions that depend on having a first argument being a matrix with saved history
std::vector<int> mp_history = {
    MP2_CBIND_TIME, MP2_CBIND_LAG, MP2_RBIND_TIME, MP2_RBIND_LAG
  , MP2_CONVOLUTION
};


// MACROS

// convert a function that takes a scalar and returns a scalar
// into one that takes a matrix and returns a matrix, by performing
// the scalar operation on every element.
#define MATRICIZE_1(FUN)                                       \
template <class Type>                                          \
matrix<Type> FUN(const matrix<Type> x) {                       \
    int rows = x.rows();                                       \
    int cols = x.cols();                                       \
    matrix<Type> res;                                          \
    res = matrix<Type>::Zero(rows, cols);                      \
    for (int i = 0; i < rows; i++) {                           \
        for (int j = 0; j < cols; j++) {                       \
            res.coeffRef(i, j) = FUN(x.coeff(i, j));           \
        }                                                      \
    }                                                          \
    return res;                                                \
}

// convert a function that takes a matrix (and an index) and 
// returns a matrix into one that takes a scalar (and an index)
// and returns a matrix, by creating a one-by-one matrix out
// of the scalar and applying the function that takes matrix
// input.
#define SCALARIZE_1_INDEX_1(FUN)                               \
template <class Type>                                          \
Type FUN(const Type x, const int &index) {                     \
    matrix<Type> y;                                            \
    y = matrix<Type>::Constant(1, 1, x);                       \
    FUN(y, index);                                             \
}                                                              \


#define MP2_ERR(CODE, MSG, FUN) SetError(CODE, MSG, row, FUN, args.all_rows(), args.all_cols(), args.all_type_ints(), t); \


// ENGINE FUNCTIONS USED BY OTHER ENGINE FUNCTIONS


template <class Type>
Type mp2_round(const Type &x) {
    Type y;
    y = x < 0 ? x - 0.5f : x + 0.5f;
    return Type(CppAD::Integer(y));
}
MATRICIZE_1(mp2_round)


template <class Type>
matrix<Type> mp2_rep(const matrix<Type> &x, const int &times) {
    int size = x.rows() * x.cols();
    matrix<Type> y;
    matrix<Type> res;
    y = x;
    y.resize(size, 1);
    res = matrix<Type>::Zero(size * times, 1);
    int off = 0;
    for (int i = 0; i < times; i++) {
        res.block(off, 0, size, 1) = y;
        off += size;
    }
    return res;
}
SCALARIZE_1_INDEX_1(mp2_rep)
  

template <class Type>
Type mp2_rbinom(const Type size, const Type prob) {
    Type m, rounded_size;
    rounded_size = mp2_round(size);
    m = ((rounded_size > 0) && (prob > 0)) ? rbinom(rounded_size, prob) : Type(0);
    return m;
}


// UTILITY FUNCTIONS ---------------------------

bool fileExists(const std::string& filename) {
    struct stat buf;
    return stat(filename.c_str(), &buf) != -1;
}

void printIntVector(const std::vector<int> &intVector) {
    for (int element : intVector)
    {
        Rcpp::Rcout << element << ' ';
    }
    Rcpp::Rcout << std::endl;
}

void printIntVectorWithLabel(const std::vector<int> &intVector, const std::string &label) {
    Rcpp::Rcout << label << ": ";
    printIntVector(intVector);
}


template <class Type>
void printMatrix(const matrix<Type> &mat) {
    for (int i = 0; i < mat.rows(); ++i) {
        for (int j = 0; j < mat.cols(); ++j) {
            Rcpp::Rcout << mat.coeff(i, j) << ' ';
        }
        Rcpp::Rcout << std::endl;
    }
}

// CLASSES --------------------------

template <class Type>
struct ListOfMatrices
{
    // below is a vector of matrices that passed from R
    vector<matrix<Type>> m_matrices;

    ListOfMatrices(SEXP ii) { // Constructor
        // Get elements by their indices
        int n = Rf_length(ii);
        vector<matrix<Type>> vs(n);
        m_matrices = vs;

        for (int i = 0; i < n; i++) {
            m_matrices[i] = asMatrix<Type>(VECTOR_ELT(ii, i));
        }
    }

    ListOfMatrices() {} // Default constructor

    // Copy constructor
    ListOfMatrices(const ListOfMatrices &another)
    {
        m_matrices = another.m_matrices;
    }

    // Overload assign operator
    ListOfMatrices &operator=(const ListOfMatrices &another)
    {
        m_matrices = another.m_matrices;
        return *this;
    }

    ListOfMatrices operator[](const std::vector<int> &indices) const
    {
        ListOfMatrices<Type> result;
        // result.m_matrices.clear(); // Ensure the result vector is empty
        // result.m_matrices.reserve(indices.size()); // Reserve memory for expected matrices
        for (int index : indices)
        {
            if (index >= 0 && index < m_matrices.size())
            {
                result.m_matrices[index] = m_matrices[index];
            }
            else
            {
                Rf_error("Index out of range");
            }
        }

        return result;
    }

    // Method to print specific matrices in the list
    void printMatrices(const std::vector<int> &indices, const std::string &label) const
    {
        Rcpp::Rcout << label << ": " << std::endl;
        for (int index : indices)
        {
            const matrix<Type> &mat = m_matrices[index];
            Rcpp::Rcout << "  inner matrix:" << std::endl;
            printMatrix(mat);
        }
    }
};

// class for lists of integer vectors that have been
// 'flattened' into two integer vectors -- one
// containing the concatenation of the vectors and
// another containing the lengths of each concatenated
// vector
class ListOfIntVecs
{
public:
    // this nestedVector will contain examples of unflattened
    // integer vectors
    std::vector<std::vector<int>> nestedVector;

    // Default constructor
    ListOfIntVecs() {}

    // Constructor that takes all_ints and vec_lens vectors
    //   all_ints: concatenated integer vectors
    //   vec_lens: lengths of each concatenated integer vectors
    ListOfIntVecs(const std::vector<int> &all_ints, const std::vector<int> &vec_lens)
    {
        size_t totalElements = 0;
        for (int size : vec_lens)
        {
            totalElements += size;
        }

        if (all_ints.size() != totalElements)
        {
            // Handle mismatched sizes as needed
            Rf_error("all_ints and vec_lens sizes do not match.");
        }

        size_t xIndex = 0;
        for (int size : vec_lens)
        {
            std::vector<int> innerVector;
            for (int i = 0; i < size; ++i)
            {
                innerVector.push_back(all_ints[xIndex++]);
            }
            nestedVector.push_back(innerVector);
        }
    }

    // Overload [] operator to access the unflattened vectors by index
    std::vector<int> &operator[](size_t index)
    {
        if (index < nestedVector.size())
        {
            return nestedVector[index];
        }
        else
        {
            // Handle out-of-range access here (you can throw an exception or handle it as needed)
            Rf_error("Index out of range");
        }
    }

    // Method to return the number of unflattened vectors
    size_t size() const
    {
        return nestedVector.size();
    }

    // Square bracket operator with a vector<int> argument
    ListOfIntVecs operator[](const std::vector<int> &indices) const
    {
        ListOfIntVecs result;

        for (int index : indices)
        {
            if (index >= 0 && static_cast<size_t>(index) < nestedVector.size())
            {
                result.nestedVector.push_back(nestedVector[static_cast<size_t>(index)]);
            }
            else
            {
                // Handle out-of-range index or negative index as needed
                Rf_error("Index out of range");
            }
        }

        return result;
    }

    // Method to print each vector in the list
    void printVectors(const std::string &label) const
    {
        Rcpp::Rcout << label << ": " << std::endl;
        for (const std::vector<int> &innerVector : nestedVector)
        {
            Rcpp::Rcout << "  inner vector: ";
            for (int element : innerVector)
            {
                Rcpp::Rcout << " " << element;
            }
            Rcpp::Rcout << std::endl;
        }
    }

    // Method to set the value of the n'th integer vector
    void setNthIntVec(size_t vec_number, const std::vector<int> &new_vector)
    {
        if (vec_number < nestedVector.size())
        {
            nestedVector[vec_number] = new_vector;
        }
        else
        {
            // Handle out-of-range access here
            Rf_error("Index out of range");
        }
    }
    // overloaded method that sets the ith element of an integer vector
    void setNthIntVec(size_t vec_number, const int &new_value, int i)
    {
        if (vec_number < nestedVector.size())
        {
            nestedVector[vec_number][i] = new_value;
        }
        else
        {
            // Handle out-of-range access here
            Rf_error("Index out of range");
        }
    }
};

// MORE UTILITY FUNCTIONS ------------------------

int is_int_in(int i, std::vector<int> vec) {
    for (unsigned int j = 0; j < vec.size(); j++) {
        //Rcpp::Rcout << "i=" << i << std::endl;
        //Rcpp::Rcout << "vec[j]=" << vec[j] << std::endl;
        if (i == vec[j]) return 1;
    }
    return 0;
}

vector<int> getNthIntVec(
    int vec_number,
    int curr_meth_id,
    ListOfIntVecs &valid_int_vecs,
    ListOfIntVecs &meth_int_vecs)
{
    vector<int> result = valid_int_vecs[meth_int_vecs[curr_meth_id]][vec_number];
    return result;
}

void setNthIntVec(
    int vec_number,
    int curr_meth_id,
    ListOfIntVecs &valid_int_vecs,
    ListOfIntVecs &meth_int_vecs,
    const std::vector<int> &new_vector)
{
    vec_number = meth_int_vecs[curr_meth_id][vec_number];
    if (vec_number >= 0 && vec_number < valid_int_vecs.size())
    {
        valid_int_vecs.setNthIntVec(vec_number, new_vector);
    }
}

int getNthMatIndex(
    int mat_number,
    int curr_meth_id,
    ListOfIntVecs &meth_mats)
{
    // printIntVectorWithLabel(meth_mats[curr_meth_id], "mat index: ");
    int result = meth_mats[curr_meth_id][mat_number];
    return result;
}

template <class Type>
int CheckIndices(matrix<Type> x, const std::vector<int> &row_indices, const std::vector<int> &col_indices)
{
    int rows = x.rows();
    int cols = x.cols();

    for (int row_index : row_indices)
    {
        if (row_index < 0 || row_index >= rows)
        {
            return 1; // Indices are not valid
        }
    }

    for (int col_index : col_indices)
    {
        if (col_index < 0 || col_index >= cols)
        {
            return 1; // Indices are not valid
        }
    }
    return 0; // Indices are valid
}


template <class Type>
matrix<Type> getNthMat(
    int mat_number,
    int curr_meth_id,
    ListOfMatrices<Type> &valid_vars,
    ListOfIntVecs &meth_mats

)
{
    int i = getNthMatIndex(mat_number, curr_meth_id, meth_mats);
    matrix<Type> result = valid_vars.m_matrices[i];
    return result;
}

template <typename Type>
class ArgList
{
public:
    enum class ItemType {
        Matrix,
        IntVector
    };

    ArgList(int size) : items_(size), size_(size) {}

    void set(int index, const matrix<Type> &mat) {
        if (index < 0 || index >= size_) {
            Rf_error("Index out of range");
        }
        items_[index].type = ItemType::Matrix;
        items_[index].mat = mat;
    }

    void set(int index, const std::vector<int> &intVec) {
        if (index < 0 || index >= size_) {
            Rf_error("Index out of range");
        }
        items_[index].type = ItemType::IntVector;
        items_[index].intVec = intVec;
    }
    
    int type_int(int i) {
        if (items_[i].type == ItemType::Matrix) {
            return 0;
        } else if (items_[i].type == ItemType::IntVector) {
            return 1;
        } else {
            Rf_error("Invalid argument type");
        }
    }

    matrix<Type> get_as_mat(int i) const {
        if (i < 0 || i >= items_.size()) {
            Rf_error("Index out of range");
        }

        if (items_[i].type == ItemType::Matrix) {
            return items_[i].mat;
        } else {
            Rf_error("Item at index is not a matrix");
        }
    }

    std::vector<int> get_as_int_vec(int i) {
        if (i < 0 || i >= items_.size()) {
            Rf_error("Index out of range");
        }

        if (items_[i].type == ItemType::IntVector) {
            return items_[i].intVec;
        } else {
            matrix<Type> m = get_as_mat(i);
            std::vector<int> v(m.rows());
            for (unsigned int i = 0; i < v.size(); i++) {
                v[i] = CppAD::Integer(m.coeff(i, 0));
            }
            return v;
        }
    }

    int get_as_int(int i) {
        if (i < 0 || i >= items_.size()) {
            Rf_error("Index out of range");
        }

        if (items_[i].type == ItemType::IntVector) {
            std::vector<int> v = get_as_int_vec(i);
            return v[0];
        }
        else {
            matrix<Type> m = get_as_mat(i);
            int j = m.rows();
            if (j == 1) {
                std::vector<int> v = get_as_int_vec(i);
                return v[0];
            }
            return j;
        }
    }

    // number of rows in argument i
    int rows(int i) {
        if (i < 0 || i >= items_.size()) {
            Rf_error("Index out of range");
        }
        if (items_[i].type == ItemType::IntVector) {
            return items_[i].intVec.size();
        }
        else {
            return items_[i].mat.rows();
        }
    }
    
    // number of columns in argument i
    int cols(int i) {
        if (i < 0 || i >= items_.size()) {
            Rf_error("Index out of range");
        }
        if (items_[i].type == ItemType::IntVector) {
            return 1;
        }
        else {
            return items_[i].mat.cols();
        }
    }
    
    matrix<Type> get_last_element(int i) {
        return get_as_mat(i).block(rows(i) - 1, cols(i) - 1, 1, 1);
    }
    
    std::vector<int> all_rows() {
        std::vector<int> v(items_.size());
        for (unsigned int i = 0; i < v.size(); i++) {
            v[i] = rows(i);
        }
        return v;
    }
    std::vector<int> all_cols() {
        std::vector<int> v(items_.size());
        for (unsigned int i = 0; i < v.size(); i++) {
            v[i] = cols(i);
        }
        return v;
    }
    std::vector<int> all_type_ints() {
        std::vector<int> v(items_.size());
        for (unsigned int i = 0; i < v.size(); i++) {
            v[i] = type_int(i);
        }
        return v;
    }
    int all_matrices() {
        int v = 1;
        for (unsigned int i = 0; i < items_.size(); i++) {
            v = v * (1 - type_int(i));
        }
        return v;
    }
    

    // for back-compatibility and sanity so you can still 
    // do args[0], args[1], etc ...
    matrix<Type> operator[](int i) {
        return get_as_mat(i);
    }

    // Method to recycle elements, rows, and columns to make operands compatible for binary operations
    ArgList<Type> recycle_for_bin_op() const {
        ArgList<Type> result = *this; // Create a new ArgList as a copy of the current instance

        matrix<Type> mat0 = result.get_as_mat(0);
        matrix<Type> mat1 = result.get_as_mat(1);

        if (mat0.rows() == mat1.rows()) {
            if (mat0.cols() != mat1.cols()) {
                if (mat0.cols() == 1) { // Vector vs matrix or scalar vs vector
                    matrix<Type> m = mat0;
                    mat0 = mat1; // for the shape
                    for (int i = 0; i < mat0.cols(); i++) {
                        mat0.col(i) = m.col(0);
                    }
                } else if (mat1.cols() == 1) { // Vector vs matrix or scalar vs vector
                    matrix<Type> m = mat1;
                    // result.set(1, mat0); // Set for the shape
                    mat1 = mat0;
                    for (int i = 0; i < mat1.cols(); i++) {
                        mat1.col(i) = m.col(0);
                    }
                }
                else {
                    result.set_error_code(201); // Set the error code for "The two operands do not have the same number of columns"
                }
            }
            // else: do nothing
        }
        else {
            if (mat0.cols() == mat1.cols()) { // Only one compatible dimension
                if (mat0.rows() == 1) { // Vector vs matrix or scalar vs vector
                    matrix<Type> m = mat0;
                    mat0 = mat1;
                    for (int i = 0; i < mat0.rows(); i++) {
                        mat0.row(i) = m.row(0);
                    }
                }
                else if (mat1.rows() == 1) { // Vector vs matrix or scalar vs vector
                    matrix<Type> m = mat1;
                    mat1 = mat0;
                    for (int i = 0; i < mat0.rows(); i++) {
                        mat1.row(i) = m.row(0);
                    }
                }
                else {
                    result.set_error_code(202); // Set the error code for "The two operands do not have the same number of rows"
                }
            }
            else { // No dimensions are equal
                if (mat0.rows() == 1 && mat0.cols() == 1) { // Scalar vs non-scalar
                    Type s = mat0.coeff(0, 0);
                    mat0 = mat1;
                    mat0.setConstant(s);
                } else if (mat1.rows() == 1 && mat1.cols() == 1) { // Scalar vs non-scalar
                    Type s = mat1.coeff(0, 0);
                    mat1 = mat0;
                    mat1.setConstant(s);
                }
                else {
                    result.set_error_code(203); // Set the error code for "The two operands do not have the same number of columns or rows"
                }
            }
        }
        result.set(0, mat0);
        result.set(1, mat1);
        return result;
    }

    // Method to recycle elements of all arguments
    // pointed at by `indices` so that they match a
    // given shape given by `rows` and `cols`.
    ArgList<Type> recycle_to_shape(const std::vector<int> &indices, int rows, int cols) const
    {
        ArgList<Type> result = *this; // Create a new ArgList as a copy of the current instance

        // Rcpp::Rcout << "step a: " << rows << " and " << cols << std::endl;
        int error_code = 0; // Initialize the error code
        // Rcpp::Rcout << "step b" << std::endl;
        for (int index : indices) {
            matrix<Type> mat = result.get_as_mat(index);
            // Rcpp::Rcout << "step c" << std::endl;
            if (mat.rows() == rows && mat.cols() == cols) {
                // Rcpp::Rcout << "no action" << std::endl;
                // No further action needed for this matrix
                continue;
            }
            // Rcpp::Rcout << "step d" << std::endl;
            matrix<Type> m(rows, cols);
            // Rcpp::Rcout << "step e" << std::endl;
            if (mat.rows() == 1 && mat.cols() == 1) {
                // Rcpp::Rcout << "scalar in" << std::endl;
                m = matrix<Type>::Constant(rows, cols, mat.coeff(0, 0));
            }
            else if (mat.rows() == rows) {
                if (mat.cols() == 1) {
                    // Rcpp::Rcout << "good column vector" << std::endl;
                    for (int i = 0; i < cols; i++) {
                        m.col(i) = mat.col(0);
                    }
                }
                else {
                    // Rcpp::Rcout << "step f" << std::endl;
                    // Rcpp::Rcout << "bad column vector" << std::endl;
                    error_code = 501;
                    // break; // Exit the loop on error
                }
            }
            else if (mat.cols() == cols) {
                if (mat.rows() == 1) {
                    // Rcpp::Rcout << "good row vector" << std::endl;
                    for (int i = 0; i < rows; i++) {
                        m.row(i) = mat.row(0);
                    }
                }
                else
                {
                    // Rcpp::Rcout << "bad row vector" << std::endl;
                    error_code = 501;
                    // break; // Exit the loop on error
                }
            }
            else
            {
                // Rcpp::Rcout << "really bad" << std::endl;
                // Rcpp::Rcout << "step g" << std::endl;
                error_code = 501;
                // break; // Exit the loop on error
            }

            if (error_code != 0)
            {
                // Rcpp::Rcout << "step h" << std::endl;
                result.set_error_code(error_code);
                break; // Exit the loop on error
            }

            // If recycling is successful, update the result ArgList object
            result.set(index, m);
        }

        return result;
    }

    int check_indices(int mat_index, const std::vector<int> &row_indices, const std::vector<int> &col_indices) const
    {
        if (mat_index < 0 || mat_index >= items_.size())
        {
            return 2; // Return an error code for an invalid index
        }

        matrix<Type> x = get_as_mat(mat_index);
        int return_val = CheckIndices(x, row_indices, col_indices);

        return return_val;
    }

    // Method to set an error code
    void set_error_code(int error)
    {
        error_code_ = error;
    }

    // Getter for the error code
    int get_error_code() const
    {
        return error_code_;
    }

private:
    struct Item
    {
        ItemType type;
        matrix<Type> mat;
        std::vector<int> intVec;
    };

    std::vector<Item> items_;
    int size_;
    int error_code_ = 0; // Initialize the error code to 0 (no error) by default
};
// private:
//     std::vector<ItemType> items_;
//     int size_;
//     int error_code_ = 0; // Initialize the error code to 0 (no error) by default
// };

template <class Type>
class ExprEvaluator
{
private:
    vector<int> mats_save_hist;
    vector<int> table_x;
    vector<int> table_n;
    vector<int> table_i;
    vector<int> meth_type_id; // vector over user defined methods, identifying a type of method
    ListOfIntVecs meth_mats;
    ListOfIntVecs meth_int_vecs;
    ListOfIntVecs valid_int_vecs;
    vector<Type> valid_literals;

public:
    // constructor
    ExprEvaluator(
        vector<int> &mats_save_hist_,
        vector<int> &table_x_,
        vector<int> &table_n_,
        vector<int> &table_i_,
        vector<int> &meth_type_id_,
        ListOfIntVecs &meth_mats_,
        ListOfIntVecs &meth_int_vecs_,
        ListOfIntVecs &valid_int_vecs_,
        vector<Type> &valid_literals_

    ) {
        error_code = 0; // non-zero means error has occurred; otherwise, no error
        expr_row = 0;
        func_int = -99; // assume no function information is available
        time_int = 0;
        arg_rows = {0};
        arg_cols = {0};
        arg_type_ints = {0};
        mats_save_hist = mats_save_hist_;
        table_x = table_x_;
        table_n = table_n_;
        table_i = table_i_;
        meth_type_id = meth_type_id_;
        meth_mats = meth_mats_;
        meth_int_vecs = meth_int_vecs_;
        valid_int_vecs = valid_int_vecs_;
        valid_literals = valid_literals_;

        strcpy(error_message, "OK");
    };

    // getters
    unsigned char GetErrorCode() { return error_code; };
    const char *GetErrorMessage() { return error_message; };
    int GetExprRow() { return expr_row; };
    int GetFuncInt() { return func_int; };
    int GetTimeInt() { return time_int; };
    std::vector<int> GetArgRows() { return arg_rows; };
    std::vector<int> GetArgCols() { return arg_cols; };
    std::vector<int> GetArgTypeInts() { return arg_type_ints; };

    // setters
    void SetError(unsigned char code, const char *message, int e_row, int f_int, std::vector<int> a_rows, std::vector<int> a_cols, std::vector<int> a_type_ints, int t_int) {
        error_code = code;
        expr_row = e_row;
        func_int = f_int;
        time_int = t_int;
        arg_rows = a_rows;
        arg_cols = a_cols;
        arg_type_ints = a_type_ints;
        strcpy(error_message, message);
    };

    // evaluator
    matrix<Type> EvalExpr(
        const vector<ListOfMatrices<Type>> &hist, // current simulation history
        int t,                                    // current time step
        ListOfMatrices<Type> &valid_vars,         // current list of values of each matrix
        int row = 0                               // current expression parse table row being evaluated
    ) {

        // total number of time steps in the simulation loop
        int t_max = hist.size() - 2;
        
        // =====================================
        // -----{Available Local Variables}-----
        // =====================================
        // Variables to use locally in 'macpan2 function' and
        // 'macpan2 method' bodies -- these are not real functions and methods 
        // in either the c++ or r sense.
        matrix<Type> m, m1, m2, m3, m4, m5;     // return values
        std::vector<int> v, v1, v2, v3, v4, v5; // integer vectors
        bool is_finite_mat;
        vector<int> u; // FIXME: why not std::vector<int> here??
        matrix<Type> Y, X, A;
        std::vector<int> timeIndex; // for rbind_time and rbind_lag
        int doing_lag = 0;
        Type sum, eps, limit, var, by, left_over, remaining_prop, p0; // intermediate scalars
        Type delta_t; // for reulermultinom
        int rows, cols, lag, rowIndex, colIndex, matIndex, cp, off, size, times;
        unsigned int grpIndex;
        int size_in, size_out;
        int start, sz, err_code, curr_meth_id;
        std::vector<int> curr_meth_mat_id_vec;
        std::vector<int> curr_meth_int_id_vec;
        vector<matrix<Type>> meth_args(meth_mats.size());
        ListOfIntVecs meth_int_args;

        // Check if error has already happened at some point
        // of the recursive call of EvalExpr.
        if (GetErrorCode())
            return m;
        switch (table_n[row])
        {
        case -2: // methods (pre-processed matrices)

            curr_meth_id = table_x[row];

            switch (meth_type_id[curr_meth_id])
            {
            case METH_FROM_ROWS:
                m = getNthMat(0, curr_meth_id, valid_vars, meth_mats);
                v = getNthIntVec(0, curr_meth_id, valid_int_vecs, meth_int_vecs);
                m1 = matrix<Type>::Zero(v.size(), m.cols());
                for (unsigned int i = 0; i < v.size(); i++)
                    m1.row(i) = m.row(v[i]);
                return m1;
            case METH_MAT_MULT_TO_ROWS:
                matIndex = getNthMatIndex(0, curr_meth_id, meth_mats);
                m = getNthMat(1, curr_meth_id, valid_vars, meth_mats);
                m1 = getNthMat(2, curr_meth_id, valid_vars, meth_mats);
                v = getNthIntVec(0, curr_meth_id, valid_int_vecs, meth_int_vecs);
                v1 = getNthIntVec(1, curr_meth_id, valid_int_vecs, meth_int_vecs);
                m2 = matrix<Type>::Zero(v1.size(), m1.cols());
                for (unsigned int i = 0; i < v1.size(); i++)
                {
                    m2.row(i) = m1.row(v1[i]);
                }
                m3 = m * m2;
                for (unsigned int k = 0; k < v.size(); k++)
                    valid_vars.m_matrices[matIndex].row(v[k]) = m3.row(k);
                return m4; // empty matrix

            case METH_GROUP_SUMS:
                m = getNthMat(0, curr_meth_id, valid_vars, meth_mats);
                v = getNthIntVec(0, curr_meth_id, valid_int_vecs, meth_int_vecs);
                rows = getNthIntVec(1, curr_meth_id, valid_int_vecs, meth_int_vecs)[0];
                m1 = matrix<Type>::Zero(rows, 1);

                for (int i = 0; i < m.rows(); i++)
                {
                    rowIndex = v[i];
                    m1.coeffRef(rowIndex, 0) += m.coeff(i, 0);
                }
                return m1;

            case METH_TV_MAT:
                m = getNthMat(0, curr_meth_id, valid_vars, meth_mats);                  // Y -- row-binded blocks, each corresponding to a change-point
                v = getNthIntVec(0, curr_meth_id, valid_int_vecs, meth_int_vecs);       // t -- change-point times
                rows = getNthIntVec(1, curr_meth_id, valid_int_vecs, meth_int_vecs)[0]; // n -- block size
                cols = m.cols();
                u = getNthIntVec(2, curr_meth_id, valid_int_vecs, meth_int_vecs); // i -- time-group pointer
                off = u[0];
                grpIndex = off + 1;
                if (grpIndex < v.size())
                {
                    if (v[grpIndex] == t)
                    {
                        u[0] = grpIndex;
                        setNthIntVec(2, curr_meth_id, valid_int_vecs, meth_int_vecs, u);
                    }
                }
                return m.block(rows * off, 0, rows, cols);

            case METH_ROWS_TIMES_ROWS:
                m = getNthMat(0, curr_meth_id, valid_vars, meth_mats);
                m1 = getNthMat(1, curr_meth_id, valid_vars, meth_mats);
                u = getNthIntVec(0, curr_meth_id, valid_int_vecs, meth_int_vecs);
                v = getNthIntVec(0, curr_meth_id, valid_int_vecs, meth_int_vecs);

                if (u.size() != v.size()) {
                    return m2;
                }
                if (m.cols() != m1.cols()) {
                    return m2;
                }
                m2 = matrix<Type>::Zero(u.size(), m.cols());
                m3 = matrix<Type>::Zero(v.size(), m1.cols());
                for (unsigned int i = 0; i < u.size(); i++) {
                    m2.row(i) = m.row(u[i]);
                    m3.row(i) = m1.row(v[i]);
                }
                return m2.cwiseProduct(m3);

            default:
                return m;
            }
        case -1: // literals
            m = matrix<Type>::Zero(1, 1);
            m.coeffRef(0, 0) = valid_literals[table_x[row]];
            return m;
        case 0: // matrices
            m = valid_vars.m_matrices[table_x[row]];
            return m;
        default: // functions
            int n = table_n[row];
            ArgList<Type> args(n);
            vector<int> index2mats(n);
            vector<int> index2what(n);
            for (int i = 0; i < n; i++) {
                if (table_n[table_i[row] + i] == -3)
                {
                    // -3 in the 'number of arguments' column of the
                    // parse table means 'integer vector'
                    args.set(i, valid_int_vecs[table_x[table_i[row] + i]]);
                }
                else
                {
                    // otherwise, recursively descend into the parse tree
                    // to pick out the arguments
                    args.set(i, EvalExpr(hist, t, valid_vars, table_i[row] + i));
                }

                // Check here if index2mats actually points at
                // a matrix and not a function. Later on if index2mats
                // is used one should check if it is -1, indicating that
                // it is not pointing at a named matrix and therefore
                // should fail.
                // TODO: named matrix indexing should really be a class
                // or something.
                //
                // index2what = 0 (for a matrix), 1 (for an int vec), -1 (for something else)
                if (table_n[table_i[row] + i] == 0)
                {
                    index2mats[i] = table_x[table_i[row] + i];
                    index2what[i] = 0; // pointing at matrix
                }
                else if (table_n[table_i[row] + i] == -3)
                {
                    index2mats[i] = table_x[table_i[row] + i];
                    index2what[i] = 1; // pointing at integer vector
                }
                else
                {
                    index2mats[i] = -1;
                    index2what[i] = -1;
                }
                if (GetErrorCode())
                    return m;
            }

            if (is_int_in(table_x[row] + 1, mp_math)) {
                if (args.all_matrices() == 0) {
                    MP2_ERR(205, "All arguments to math functions must be numeric matrices, but at least one is an integer", table_x[row] + 1);
                    return m;
                }
            }
            if (is_int_in(table_x[row] + 1, mp_history)) {
                if (!mats_save_hist[index2mats[0]]) {
                    MP2_ERR(205, "All arguments to functions that act on the simulation history must have a first argument that is a non-empty matrix with saved history", table_x[row] + 1);
                    return m;
                }
            }
            
            // Elementwise Binary Operations (e.g., + - * / ^)
            // Check dimensions compatibility. If needed, 
            // expand one operand to make its dimensions 
            // compatible with the other
            if (is_int_in(table_x[row] + 1, mp_elementwise_binop) && table_n[row] == 2) {
                args = args.recycle_for_bin_op();
                err_code = args.get_error_code();
                switch (err_code) {
                case 201:
                    MP2_ERR(201, "The two operands do not have the same number of columns", table_x[row] + 1);
                    return m;
                case 202:
                    MP2_ERR(202, "The two operands do not have the same number of rows", table_x[row] + 1);
                    return m;
                case 203:
                    MP2_ERR(203, "The two operands do not have the same number of columns or rows", table_x[row] + 1);
                    return m;
                }
            }
            else if (table_x[row] + 1 == MP2_MATRIX_MULTIPLY) { // %*% matrix multiplication
                if (args[0].cols() != args[1].rows()) {
                    MP2_ERR(204, "The two operands are not compatible to do matrix multiplication", table_x[row] + 1);
                    return m;
                }
            }

            if (GetErrorCode())
                return m; // early return

            // #' Functions Available in the Simulation Engine
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
            // #' To produce a dynamical simulation that iteratively evaluates
            // #' expression involving these functions, use 
            // #' \code{\link{simple_sims}}.
            // #'
            // #' ```
            // #' simple_sims(
            // #'   iteration_exprs = list(x ~ x - 0.9 * x),
            // #'   time_steps = 5,
            // #'   mats = list(x = 1)
            // #' )
            // #' ```
            // #'
            // #' Here, `x - 0.9 * x` is assigned to `x` at each of five 
            // #' iterations of a simulation loop.
            // #' 
            // #' If these expressions involve matrices with more than one 
            // #' element, You can control which elements in the evaluation 
            // #' of the right hand side go to which elements on the left 
            // #' hand side. This technique involves using either square
            // #' brackets or the `c` function on the left hand side. For 
            // #' more information on assignment, please see the section
            // #' on Assignment below.
            // #'
            switch (table_x[row] + 1) {

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
            // #' * `x` : Any matrix with dimensions compatible with `y`.
            // #' * `y` : Any matrix with dimensions compatible with `x`.
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
                Rcpp::Rcout << args.get_as_mat(0) << " + " << args.get_as_mat(1) << " = " << args.get_as_mat(0) + args.get_as_mat(1) << std::endl
                          << std::endl;
                #endif
                if (table_n[row] == 1) {
                    return args.get_as_mat(0); // unary +  (in case someone puts a plus sign at the beginning of an expression)
                } else {
                    return args.get_as_mat(0) + args.get_as_mat(1); // binary +
                }
            case MP2_SUBTRACT: // -
                #ifdef MP_VERBOSE
                if (table_n[row] == 1)
                    Rcpp::Rcout << "Unary - " << args.get_as_mat(0) << std::endl
                              << std::endl;
                else
                    Rcpp::Rcout << args.get_as_mat(0) << " - " << args.get_as_mat(1) << " = " << args.get_as_mat(0) - args.get_as_mat(1) << std::endl
                              << std::endl;
                #endif
                if (table_n[row] == 1) {
                    return -args.get_as_mat(0); // unary -
                } else {
                    return args.get_as_mat(0) - args.get_as_mat(1); // binary -
                }
            case MP2_MULTIPLY: // *
                #ifdef MP_VERBOSE
                Rcpp::Rcout << args.get_as_mat(0) << " .* " << args.get_as_mat(1) << " = " << args.get_as_mat(0).cwiseProduct(args.get_as_mat(1)) << std::endl
                          << std::endl;
                #endif
                return args.get_as_mat(0).cwiseProduct(args.get_as_mat(1));
            case MP2_DIVIDE: // /
                #ifdef MP_VERBOSE
                Rcpp::Rcout << args.get_as_mat(0) << " ./ " << args.get_as_mat(1) << " = " << args.get_as_mat(0).array() / args.get_as_mat(1).array() << std::endl
                          << std::endl;
                #endif
                return args.get_as_mat(0).cwiseQuotient(args.get_as_mat(1));
            case MP2_POWER: // ^
                #ifdef MP_VERBOSE
                Rcpp::Rcout << args.get_as_mat(0) << " ^ " << args.get_as_mat(1) << " = " << pow(args.get_as_mat(0).array(), args.get_as_mat(1).coeff(0, 0)).matrix() << std::endl
                          << std::endl;
                #endif
                return pow(args.get_as_mat(0).array(), args.get_as_mat(1).array()).matrix();

            // #' ## Elementwise Math
            // #'
            // #' ### Functions
            // #'
            // #' * `log(x)` : Natural logarithm.
            // #' * `exp(x)` : Exponential function.
            // #' * `cos(x)` : Cosine function.
            // #' * `sin(x)` : Sine function.
            // #' * `sqrt(x)` : Squareroot function.
            // #' * `invlogit(x)` : Inverse logit function, 
            // #' `1/(1 + exp(-x))`.
            // #' * `logit(x)` : Logit function, `log(x/(1-x))`.
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : Any numeric matrix.
            // #'
            // #' ### Return
            // #'
            // #' * A matrix with the same dimensions as `x`, containing
            // #' the results of applying the function to each element of `x`.
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

            case MP2_SIN:
                return args[0].array().sin().matrix();
              
            case MP2_SQRT:
                return sqrt(args[0].array()).matrix();
              
            case MP2_INVLOGIT:
                return (1 / (1 + (-args[0].array()).exp())).matrix();

            case MP2_LOGIT:
                return (-(1 / args[0].array() - 1).log()).matrix();
                  
            // #' ## Proportions
            // #'
            // #' ### Functions
            // #'
            // #' * `proportions(x, limit, eps)`
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : Any matrix
            // #' * `limit` : numeric value to return elementwise from `proportions` if `sum(x) < eps`
            // #' * `eps` : numeric tolerance for `sum(x)`
            // #'
            // #' ### Return
            // #'
            // #' * matrix of `x / sum(x)` or `rep(limit, length(x))` if 
            // #' `sum(x) < eps`.
            // #'
            // #' ### Examples
            // #'
            // #' ```
            // #' engine_eval(~ proportions(y, 0.5, 1e-8), y = c(2, 0.5))
            // #' ```
            // #' 
            case MP2_PROPORTIONS:
                m = args.get_as_mat(0);
                m1 = matrix<Type>::Zero(1, 1);
                m1.coeffRef(0, 0) = 1;
                if (m.size() == 1) return m1;
                limit = args.get_as_mat(1).coeff(0, 0);
                eps = args.get_as_mat(2).coeff(0, 0);
                sum = m.sum();
                m2 = matrix<Type>::Zero(args.rows(0), args.cols(0));
                for (int i = 0; i < m2.rows(); i++) {
                    for (int j = 0; j < m2.cols(); j++) {
                        m2.coeffRef(i, j) = CppAD::CondExpLt(sum, eps, limit, m.coeff(i, j) / sum);
                    }
                }
                return m2;
              


            // #' ## Integer Sequences
            // #'
            // #' ### Functions
            // #'
            // #' * `from:to` : Inclusive and ordered sequence of
            // #' integers between two bounds.
            // #' * `seq(from, length, by)` : Ordered sequence of
            // #' integers with equal spacing between adjacent
            // #' values.
            // #'
            // #' ### Arguments
            // #'
            // #' * `from` : Scalar integer giving the first integer
            // #' in the sequence.
            // #' * `to` : Scalar integer giving the last integer in
            // #' the sequence.
            // #' * `length` : Number of integers in the sequence.
            // #' * `by` : Scalar giving the difference
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
                from = args.get_as_int(0);
                to = args.get_as_int(1);
                if (from > to) {
                    MP2_ERR(MP2_COLON, "Lower bound greater than upper bound in : operation", MP2_COLON);
                    return m;
                }
                m = matrix<Type>::Zero(to - from + 1, 1);
                for (int i = from; i <= to; i++)
                    m.coeffRef(i - from, 0) = i;
                #ifdef MP_VERBOSE
                Rcpp::Rcout << from << ":" << to << " = " << m << std::endl
                          << std::endl;
                #endif
                return m;

            case MP2_SEQUENCE: // seq

                // #' The `seq` function is a little different from the
                // #' base R default, \code{\link[base]{seq}}, in that it
                // #' allows the user precise control over the length of
                // #' the output through the `length` argument. The
                // #' base R function gives the user this option, but not
                // #' as the default.
                // #'
                int length;
                from = args.get_as_int(0);
                length = args.get_as_int(1);
                by = args[2].coeff(0, 0);
                if (length <= 0) {
                    MP2_ERR(MP2_SEQUENCE, "Sequence length is less than or equal to zero in seq operation", MP2_SEQUENCE);
                    return m;
                }
                m = matrix<Type>::Zero(length, 1);
                for (int i = 0; i < length; i++)
                    m.coeffRef(i, 0) = from + i * by;
                #ifdef MP_VERBOSE
                Rcpp::Rcout << "seq(" << from << ", " << length << ", " << by << ") = "
                          << m << std::endl
                          << std::endl;
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
            // #' * `rep(x, times)` : Replicate a column vector a
            // #' number of times, by repeatedly stacking it on top of
            // #' itself.
            // #' * `recycle(x, rows, cols)` : Recycle rows and columns of
            // #' `x` to produce a matrix with `rows` rows and `cols` columns.
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : A scalar-valued variable to repeat.
            // #' * `times` : A scalar-valued integer variable giving
            // #' the number of times to repeat `x`.
            // #' * `rows` : Number of rows in the output of `recycle`.
            // #' * `cols` : Number of columns in the output of `recycle`.
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
            // #' engine_eval(~ recycle(  1:3,  3, 4))
            // #' engine_eval(~ recycle(t(1:4), 3, 4))
            // #' ```
            // #'
            case MP2_REPLICATE: // rep
                X = args[0];
                times = args.get_as_int(1);
                return mp2_rep(X, times);
            
            case MP2_RECYCLE:
                // rows = CppAD::Integer(args[1].coeff(0,0));
                // cols = CppAD::Integer(args[2].coeff(0,0));
                rows = args.get_as_int(1);
                cols = args.get_as_int(2);
                v1.push_back(0);
                args = args.recycle_to_shape(v1, rows, cols);
                err_code = args.get_error_code();
                m = args[0];
                if (err_code != 0) {
                    MP2_ERR(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", MP2_RECYCLE);
                    return m;
                }
                return m;

            // #' ## Matrix Multiplication
            // #'
            // #' ### Functions
            // #'
            // #' * `x %*% y` : Standard matrix multiplication.
            // #' * `x %x% y` : Kronecker product
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : A matrix. For the standard product, `x`
            // #' must have as many columns as `y` has rows.
            // #' * `y` : A matrix. For standard product, `y`
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
            case MP2_MATRIX_MULTIPLY: // %*%
                return args[0] * args[1];

            case MP2_KRONECKER: // %x%
                rows = args[0].rows() * args[1].rows();
                cols = args[0].cols() * args[1].cols();
                m = matrix<Type>::Zero(rows, cols);
                for (int i = 0; i < args[0].rows(); i++) {
                    for (int j = 0; j < args[0].cols(); j++) {
                        m.block(i * args[1].rows(), j * args[1].cols(), args[1].rows(), args[1].cols()) = args[0].coeff(i, j) * args[1];
                    }
                }
                return m;

            // #' ## Parenthesis
            // #'
            // #' The order of operations can be enforced in the usual
            // #' way with round parentheses, \code{\link{(}}.
            // #'
            case MP2_ROUND_BRACKET: // (
                return args[0];

            // #' ## Reshaping and Combining Matrices
            // #'
            // #' ### Functions
            // #'
            // #' * `c(...)` : Stack columns of arguments into a
            // #' single column vector.
            // #' * `cbind(...)` : Create a matrix containing all of
            // #' the columns of a group of matrices with the same
            // #' number of rows.
            // #' * `rbind(...)` : Create a matrix containing all of
            // #' the rows of a group of matrices with the same number
            // #' of columns.
            // #' * `matrix(x, rows, cols)` : Reshape a matrix to have
            // #' `rows` rows and `cols` columns. The input `x` must
            // #' have `rows * cols` elements.
            // #' * `t(x)` : Standard matrix transpose.
            // #'
            // #' ### Arguments
            // #'
            // #' * `...` : Any number of dimensionally consistent
            // #' matrices. The definition of dimensionally consistent
            // #' depends on the function.
            // #' * `x` : Can be any matrix for `t`, but for `matrix`
            // #' it must have `rows * cols` elements.
            // #' * `rows` : Scalar integer giving the number of
            // #' rows in the output.
            // #' * `cols` : Scalar integer giving the number of
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
                // Rcpp::Rcout << "in c(...)" << std::endl;
                size = 0;
                for (int i = 0; i < n; i++)
                {
                    size += args[i].rows() * args[i].cols();
                }
                m = matrix<Type>::Zero(size, 1);
                off = 0;
                for (int i = 0; i < n; i++)
                {
                    cols = args[i].cols();
                    for (int j = 0; j < cols; j++)
                    {
                        rows = args[i].rows();
                        // Rcpp::Rcout << "number of rows in c(...): " << rows << std::endl;
                        if (rows != 0)
                        { // avoid adding empty matrices
                            // Rcpp::Rcout << "adding rows" << std::endl;
                            m.block(off, 0, rows, 1) = args[i].col(j);
                            off += rows;
                        }
                    }
                }

                #ifdef MP_VERBOSE
                Rcpp::Rcout << "c(" << args[0] << ", ...," << args[n - 1] << ") = " << m << std::endl
                          << std::endl;
                #endif
                return m;

            // #' Column and row vectors of the same length can be
            // #' combined using the \code{\link{cbind}} and
            // #' \code{\link{rbind}} functions respectively
            // #'
            case MP2_CBIND:
                rows = args[0].rows();
                // Rcpp::Rcout << "rows: " << rows << std::endl;
                // Rcpp::Rcout << "n: " << n << std::endl;
                int cols_per_arg;
                int totcols, colmarker;
                totcols = 0;
                colmarker = 0;
                for (int j = 0; j < n; j++) {
                    totcols += args[j].cols();
                }
                m = matrix<Type>::Zero(rows, totcols);
                for (int i = 0; i < n; i++) {
                    if (args[i].rows() == rows) {
                        cols_per_arg = args[i].cols();
                        for (int k = 0; k < cols_per_arg; k++) {
                            m.col(colmarker + k) = args[i].col(k);
                        }
                        colmarker += cols_per_arg;
                    }
                    else {
                        MP2_ERR(MP2_CBIND, "Inconsistent size in cbind function", MP2_CBIND);
                        return m;
                    }
                }
                return m;
                
                
            case MP2_RBIND:
                cols = args[0].cols();
                // Rcpp::Rcout << "cols: " << cols << std::endl;
                // Rcpp::Rcout << "n: " << n << std::endl;
                int rows_per_arg;
                int totrows, rowmarker;
                totrows = 0;
                rowmarker = 0;
                for (int j = 0; j < n; j++)
                {
                    totrows += args[j].rows();
                }
                m = matrix<Type>::Zero(totrows, cols);
                for (int i = 0; i < n; i++)
                {
                    if (args[i].cols() == cols)
                    {
                        rows_per_arg = args[i].rows();
                        for (int k = 0; k < rows_per_arg; k++)
                        {
                            m.row(rowmarker + k) = args[i].row(k);
                        }
                        rowmarker += rows_per_arg;
                    }
                    else
                    {
                        MP2_ERR(MP2_RBIND, "Inconsistent size in rbind function", MP2_RBIND);
                        return m;
                    }
                }
                return m;
                
                
            // #' The `matrix` function can be used to redefine the
            // #' numbers of rows and columns to use for arranging
            // #' the values of a matrix. It works similarly to
            // #' the base R \code{\link[base]{matrix}} function in that it
            // #' takes the same arguments.
            // #' On the other hand, this function differs substantially
            // #' from the base R version in that it must be filled
            // #' by column and there is no `byrow` option.
            // #'
            case MP2_MATRIX: // matrix
                if (n > 3) {
                    MP2_ERR(MP2_MATRIX, "Too many arguments provided to function. Note this function differs from the base R version in the arguments it accepts.", MP2_MATRIX);
                }
                rows = args.get_as_int(1);
                cols = args.get_as_int(2);
                size_out = rows * cols;
                m = args[0];
                size_in = m.rows() * m.cols();
                if (size_in == size_out) {
                    m.resize(rows, cols);
                } else if (size_in < size_out) {
                    if (size_out % size_in) {
                        MP2_ERR(MP2_MATRIX, "The size of the input matrix is not compatible with the requested shape of the output matrix.", MP2_MATRIX);
                        return m1;
                    }
                    m = mp2_rep(m, size_out / size_in);
                    m.resize(rows, cols);
                } else {
                    MP2_ERR(MP2_MATRIX, "The size of the input must be less than or equal to that of the output.", MP2_MATRIX);
                }
                return m;

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
            // #' * `to_diag(x)` : Create a diagonal matrix by setting
            // #' the diagonal to a column vector, `x`.
            // #' * `from_diag(x)` : Extract the diagonal from a
            // #' matrix, `x`, and return the diagonal as a column
            // #' vector.
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : Any matrix (for `from_diag`) or a
            // #' column vector (for `to_diag`). It is common to assume
            // #' that `x` is square for `from_diag` but this is
            // #' not required.
            // #'
            // #' ### Return
            // #'
            // #' * `to_diag(x)` : Diagonal matrix with `x` on the
            // #' diagonal.
            // #' * `from_diag(x)` : Column vector containing the
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
                for (int i = 0; i < rows; i++)
                    m.coeffRef(i, i) = args[0].coeff(i, 0);
                return m;

            case MP2_FROM_DIAG: // from_diag
                m = args[0].diagonal();
                return m;
                

            // #' ## Summarizing Matrix Values
            // #'
            // #' ### Functions
            // #'
            // #' * `sum(...)` : Sum all of the elements of all of the
            // #' matrices passed to `...`.
            // #' * `col_sums(x)` : Row vector containing the sums
            // #' of each column.
            // #' * `row_sums(x)` : Column vector containing the sums
            // #' of each row.
            // #' * `group_sums(x, f, n)` : Column vector containing the
            // #' sums of groups of elements in `x`. The groups are
            // #' determined by the integers in `f` and the order of
            // #' the sums in the output is determined by these
            // #' integers.
            // #' * `mean(x)` : Arthmetic average of all elements in matrix
            // #' `x`.
            // #' * `sd(x)` : Sample standard deviation of all elements in
            // #' matrix `x`.
            // #'
            // #' ### Arguments
            // #'
            // #' * `...` : Any number of matrices of any shape.
            // #' * `x` : A matrix of any dimensions, except for
            // #' `group_sums` that expects `x` to be a column vector.
            // #' * `f` : A column vector the same length as `x`
            // #' containing integers between `0` and `m-1`, given `m`
            // #' unique groups. Elements of `f` refer to the indices
            // #' of `x` that will be grouped and summed.
            // #' * `n` : A column vector of length `m`. If `f` does
            // #' not contain group `k` in `[0, m-1]`, `group_sums` skips
            // #' this group and the output at index `k+1` is `n[k+1]`.
            // #'
            // #' ### Return
            // #'
            // #' * A matrix containing sums of subsets of the inputs.
            // #'
            case MP2_SUM: // sum(...)
                m = matrix<Type>::Zero(1, 1);
                sum = 0.0;
                for (int i = 0; i < n; i++)
                    sum += args[i].sum();
                m.coeffRef(0, 0) = sum;
                return m;

            // #' ### Details
            // #'
            // #' The `row_sums` and `col_sums` are similar to the base R
            // #' \code{\link{rowSums}} and \code{\link{colSums}} functions,
            // #' but with slightly different behaviour. In particular, the 
            // #' `row_sums` function returns a column vector and the 
            // #' `col_sums` function returns a row vector. If a specific shape 
            // #' is required then the transpose \code{\link{t}} function must 
            // #' be explicitly used.
            // #'
            case MP2_ROWSUMS: // row_sums(x)
                m = args[0].rowwise().sum().matrix();
                return m;

            case MP2_COLSUMS: // col_sums(x)
                m = args[0].colwise().sum().matrix();
                return m;

            case MP2_GROUPSUMS: // group_sums(x)
                v1 = args.get_as_int_vec(1);
                err_code = args.check_indices(2, v1, {0});
                if (err_code) {
                    MP2_ERR(MP2_GROUPSUMS, "Group indexes are out of range.", MP2_GROUPSUMS);
                    return m;
                }
                m = args[0];
                if (m.cols() != 1) {
                    MP2_ERR(MP2_GROUPSUMS, "Group sums are only allowed for column vectors.", MP2_GROUPSUMS);
                }
                rows = args.rows(2); // get number of rows in the 3rd argument
                m1 = matrix<Type>::Zero(rows, 1);
                if (v1.size() != m.rows()) {
                    MP2_ERR(MP2_GROUPSUMS, "Number of rows in x must equal the number of indices in f in group_sums(x, f, n).", MP2_GROUPSUMS);
                    return m;
                }
                for (int i = 0; i < m.rows(); i++) {
                    m1.coeffRef(v1[i], 0) += m.coeff(i, 0);
                }
                return m1;
            
            case MP2_MEAN: // mean(x)
                if (n != 1) {
                    MP2_ERR(MP2_MEAN, "The mean function can only take a single matrix.", MP2_MEAN);
                    return m;
                }
                m = matrix<Type>::Zero(1, 1);
                sum = args.get_as_mat(0).mean();
                m.coeffRef(0, 0) = sum;
                return m;
            
            case MP2_SD: // sd(x)
                if (n != 1) {
                    MP2_ERR(MP2_SD, "The sd function can only take a single matrix.", MP2_SD);
                    return m;
                }
                m = args.get_as_mat(0);
                m1 = matrix<Type>::Zero(1, 1);
                sum = sqrt(((m.array() - m.mean()).square().sum() / (m.size() - 1)));
                m1.coeffRef(0, 0) = sum;
                return m1;

            // #' ### Examples
            // #'
            // #' ```
            // #' x = 1
            // #' y = 1:3
            // #' A = matrix(1:12, 4, 3)
            // #' engine_eval(~ sum(y), y = y)
            // #' engine_eval(~ sum(x, y, A), x = x, y = y, A = A)
            // #' engine_eval(~ col_sums(A), A = A)
            // #' engine_eval(~ row_sums(A), A = A)
            // #' engine_eval(~ group_sums(x, f, n), x = 1:10, f = rep(0:3, 1:4), n = c(1:4))
            // #' ```
            // #'

            // #' ## Sweeping Matrix Elements
            // #'
            // #' ### Functions
            // #'
            // #' * `cumsum(x)` : Return a matrix with columns containing the
            // #' cumulative sum of the columns in `x`.
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : A matrix.
            // #'
            // #' ### Return
            // #' 
            // #' A matrix the same size as `x` but with columns containing the
            // #' cumulative sum of the columns in `x`.
            case MP2_CUMSUM:
                rows = args.rows(0);
                cols = args.cols(0);
                m = matrix<Type>::Zero(rows, cols);
                m1 = args[0];
                for (int j = 0; j < cols; j++) {
                    m.coeffRef(0, j) = m1.coeff(0, j);
                    for (int i = 1; i < rows; i++) {
                        m.coeffRef(i, j) = m.coeff(i - 1, j) + m1.coeff(i, j);
                    }
                }
                return m;
            
            // #' ## Extracting Matrix Elements
            // #'
            // #' ### Functions
            // #'
            // #' * `x[i,j]` : Return a matrix containing a subset
            // #' of the rows and columns of `x`.
            // #' * `block(x,i,j,n,m)` : Return a matrix containing a
            // #' contiguous subset of rows and columns of `x`
            // #' \url{https://eigen.tuxfamily.org/dox/group__TutorialBlockOperations.html}.
            // #' * `last(x)` : The last element of a matrix (i.e., the
            // #' lower-right element).
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : Any matrix.
            // #' * `i` : An integer column vector (for `[`) or
            // #' integer scalar (for `block`) containing the indices
            // #' of the rows to extract (for `[`) or the index of the
            // #' first row to extract (for `block`).
            // #' * `j` : An integer column vector (for `[`) or
            // #' integer scalar (for `block`) containing the indices
            // #' of the columns to extract (for `[`) or the index of
            // #' the first column to extract (for `block`). If `j` is missing
            // #' in a call to `[`, it is assumed to be `j = 0` although
            // #' we might change this default to be the vector of all column
            // #' indices.
            // #' * `n` : Number of rows in the block to return.
            // #' * `m` : Number of columns in the block to return.
            // #'
            // #' ### Return
            // #'
            // #' * A matrix containing a subset of the rows and columns
            // #' in `x`.
            // #'
            // #' ### Details
            // #'
            // #' Note that zero-based indexing is used
            // #' so the first row/column gets index, `0`, etc. The `block`
            // #' function is expected to be more efficient than `[` when
            // #' the elements to be extracted are contiguous.
            // #'
            // #' ### Examples
            // #'
            // #' ```
            // #' engine_eval(~ A[c(3, 1, 2), 2], A = matrix(1:12, 4, 3))
            // #' engine_eval(~ block(x,i,j,n,m), x = matrix(1:12, 4, 3), i=1, j=1, n=2, m=2)
            // #' engine_eval(~ last(A), A = matrix(1:12, 4, 3))
            // #' ```
            // #'
            case MP2_SQUARE_BRACKET: // [
                #ifdef MP_VERBOSE
                Rcpp::Rcout << "square bracket" << std::endl
                          << std::endl;
                #endif

                int nrow;
                int ncol;

                v1 = args.get_as_int_vec(1);
                nrow = v1.size();
                if (n == 2) {
                    v2.push_back(0);
                    ncol = 1;
                } else {
                    v2 = args.get_as_int_vec(2);
                    ncol = v2.size();
                }
                err_code = args.check_indices(0, v1, v2);
                if (err_code) {
                    MP2_ERR(MP2_SQUARE_BRACKET, "Illegal index to square bracket", MP2_SQUARE_BRACKET);
                    return m;
                }
                m = args[0];
                m1 = matrix<Type>::Zero(nrow, ncol);
                for (int i = 0; i < nrow; i++) {
                    for (int j = 0; j < ncol; j++) {
                        m1.coeffRef(i, j) = m.coeff(v1[i], v2[j]);
                    }
                }
                return m1;

            case MP2_BLOCK: // block
                rowIndex = args.get_as_int(1);
                colIndex = args.get_as_int(2);
                rows = args.get_as_int(3);
                cols = args.get_as_int(4);

                v1 = {rowIndex};
                v2 = {colIndex};
                v3 = {rowIndex + rows - 1};
                v4 = {colIndex + cols - 1};
                err_code = args.check_indices(0, v1, v2);
                if (err_code){
                    MP2_ERR(MP2_BLOCK, "Illegal starting index to block", MP2_BLOCK);
                    return m;
                }
                err_code = args.check_indices(0, v3, v4);
                if (err_code){
                    MP2_ERR(MP2_BLOCK, "Illegal index to block, requesting more elements than available in input", MP2_BLOCK);
                    return m;
                }
                return args[0].block(rowIndex, colIndex, rows, cols);
            
            case MP2_LAST: // last(x)
                return args.get_last_element(0);
                

            // #' ## Accessing Past Values in the Simulation History
            // #'
            // #' For matrices with their simulation history saved,
            // #' it is possible to bind the rows or columns of past
            // #' versions of such matrices into a single matrix.
            // #'
            // #' ### Functions
            // #'
            // #' * `rbind_lag(x, lag, t_min)` : Bind the rows of versions of
            // #' `x` that were recorded at the end of all
            // #' simulation iterations corresponding to time lags given
            // #' by integers in `lag`.
            // #' * `rbind_time(x, t, t_min)` : Bind the rows of versions of
            // #' `x` that were recorded at the end of all
            // #' simulation iterations corresponding to integers in
            // #' `t`.
            // #' * `cbind_lag(x, lag, t_min)` : Bind the columns of versions of
            // #' `x` that were recorded at the end of all
            // #' simulation iterations corresponding to time lags given
            // #' by integers in `lag`. (TODO -- cbind_lag is not developed yet)
            // #' * `cbind_time(x, t, t_min)` : Bind the columns of versions of
            // #' `x` that were recorded at the end of all
            // #' simulation iterations corresponding to integers in
            // #' `t`. (TODO -- cbind_lag is not developed yet)
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : Any matrix with saved history such that the
            // #' number of columns (for `rbind_*`) or rows (for
            // #' `cbind_*`) does not change throughout the simulation.
            // #' * `lag` : Integer vector giving numbers
            // #' of time steps before the current step to obtain
            // #' past values of `x`.
            // #' * `t` : Integer vector giving time steps
            // #' at which to obtain past values of `x`.
            // #' * `t_min` : Integer giving the minimum time step
            // #' that is allowed to be accessed. All time-steps in `t`
            // #' or implied by `lag` that are before `t_min` are ignored.
            // #'
            // #' ### Return
            // #'
            // #' * A matrix containing values of `x` from past times.
            // #'
            case MP2_RBIND_LAG:
                doing_lag = 1;
            case MP2_RBIND_TIME:
                int int_func;
                if (doing_lag) {
                    int_func = MP2_RBIND_LAG;
                } else {
                    int_func = MP2_RBIND_TIME;
                }
                if ((t == 1) & (!doing_lag))
                    return m; // have not built up any previous iterations yet, so returning empty matrix
                if (t == 0) {
                    MP2_ERR(154, "The simulation loop has not yet begun and so rbind_time (or rbind_lag) cannot be used", int_func);
                    return m;
                }
                matIndex = index2mats[0]; // m

                if ((matIndex < 0) | (index2what[0] != 0)) {
                    MP2_ERR(MP2_RBIND_TIME, "Can only rbind_time (or rbind_lag) named matrices not expressions of matrices and not integer vectors", int_func);
                    // Rcpp::Rcout << "return 2 " << std::endl;
                    return m;
                }

                if ((n == 1) & (!doing_lag)) {
                    // std::vector<int> timeIndex(t - 1);
                    for (int i = 1; i < t_max + 1; i++) {
                        timeIndex.push_back(i);
                    }
                }
                else if ((n == 1) & (doing_lag)) {
                    timeIndex.push_back(t - 1);
                }
                else {
                    timeIndex = args.get_as_int_vec(1);
                    //Rcpp::Rcout << "t: " << t << std::endl;
                    //Rcpp::Rcout << "timeIndex.size: " << timeIndex.size() << std::endl;
                    //printIntVectorWithLabel(timeIndex, "default time index vector");
                    if (doing_lag) {
                        for (unsigned int i = 0; i < timeIndex.size(); i++) {
                            timeIndex[i] = t - timeIndex[i];
                            if (timeIndex[i] < 0) {
                                MP2_ERR(MP2_RBIND_LAG, "Lag functionality is conceptually flawed at the moment for lags greater than 1. All other lags are currently not allowed.", int_func);
                                // what we need to do is include an argument for a matrix
                                // (usually a column vector) of initial values that take
                                // us back into negative time steps. need to do the same
                                // for convolution and anything else that looks backwards.
                                return m;
                            }
                        }
                        // timeIndex = -timeIndex;
                        // timeIndex += t;
                    }
                }
                if (timeIndex.size() == 0) return m; // return empty matrix if no time indices are provided

                int lowerTimeBound;
                if (table_n[row] == 3) {
                    lowerTimeBound = args.get_as_int(2);
                    if (lowerTimeBound < 0) {
                        MP2_ERR(MP2_RBIND_TIME, "Lower time bound (third argument) is less than zero", int_func);
                        return m;
                    }
                    if (lowerTimeBound > t_max) {
                        MP2_ERR(MP2_RBIND_TIME, "Lower time bound (third argument) is greater than the number of time steps", int_func);
                        return m;
                    }
                }
                else if (doing_lag) {
                    lowerTimeBound = 0;
                }
                else {
                    lowerTimeBound = 1;
                }

                // Get the length of legitimate times in rbind_time.
                // Check if the shape of the matrix changes.
                //    Error if yes or assign variables "rows" and "cols" with
                //    the correct values otherwise.
                int rbind_length, nRows, nCols;
                rbind_length = 0; // count of legitimate time steps to select
                //Rcpp::Rcout << "lowerTimeBound " << lowerTimeBound << std::endl
                for (unsigned int i = 0; i < timeIndex.size(); i++)
                {
                    rowIndex = timeIndex[i];
                    if (rowIndex < t && rowIndex >= lowerTimeBound)
                    {
                        nRows = hist[rowIndex].m_matrices[matIndex].rows();
                        nCols = hist[rowIndex].m_matrices[matIndex].cols();
                    }
                    else if (rowIndex == t)
                    {
                        nRows = valid_vars.m_matrices[matIndex].rows();
                        nCols = valid_vars.m_matrices[matIndex].cols();
                    }
                    else
                        continue;

                    if (nRows == 0 || nCols == 0) // skip empty matrix
                        continue;

                    if (rbind_length == 0)
                    { // first one
                        rows = nRows;
                        cols = nCols;
                    }
                    else
                    {
                        if (rows != nRows || cols != nCols)
                        { // Shall we allow inconsistent rows?
                            MP2_ERR(MP2_RBIND_TIME, "Inconsistent rows or columns in rbind_time (or rbind_lag)", int_func);
                            // Rcpp::Rcout << "return 5 " << std::endl;
                            return args[0];
                        }
                    }

                    rbind_length++;
                }
                // Rcpp::Rcout << "rbind length: " << rbind_length << std::endl;

                if (rbind_length > 0) {
                    m = matrix<Type>::Zero(rbind_length * rows, cols);
                    rbind_length = 0;
                    for (unsigned int i = 0; i < timeIndex.size(); i++) {
                        rowIndex = timeIndex[i];
                        if (rowIndex < t && rowIndex >= lowerTimeBound) {
                            if (hist[rowIndex].m_matrices[matIndex].rows() != 0 &&
                                hist[rowIndex].m_matrices[matIndex].cols() != 0)
                            {
                                m.block(rbind_length * rows, 0, rows, cols) = hist[rowIndex].m_matrices[matIndex];
                                rbind_length++;
                            }
                        }
                        else if (rowIndex == t) {
                            if (valid_vars.m_matrices[matIndex].rows() != 0 &&
                                valid_vars.m_matrices[matIndex].cols() != 0) {
                                m.block(rbind_length * rows, 0, rows, cols) = valid_vars.m_matrices[matIndex];
                                rbind_length++;
                            }
                        }
                    }
                }

                // Rcpp::Rcout << "return 6" << std::endl;
                // Rcpp::Rcout << "m: " << m << std::endl;
                return m; // empty matrix (if colIndex==0) or non-empty one (otherwise)

            // #' ## Time Indexing
            // #'
            // #' Get or update the index of the current or lagged 
            // #' time step or the index of the current time group. 
            // #' A time group is a contiguous set of time steps 
            // #' defined by two change points.
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
            // #' * `time_var(x, change_points)`: An improvement
            // #' to `time_group`. Returns values in `x`
            // #' at time steps in `change_points`, return value
            // #' remains constant between `change_points`.
            // #'
            // #' ### Arguments
            // #'
            // #' * `x`: Column vector representing a time series.
            // #' `time_var` will return the value of `x` corresponding
            // #' to element in `change_points` that contains the
            // #' current time.
            // #' * `lag`: Number of time-steps to look back for
            // #' the time-step to return.
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
            // #'   mats = list(x = empty_matrix)
            // #' )
            // #' sims = simple_sims(
            // #'   iteration_exprs = list(
            // #'     j ~ time_group(j, change_points),
            // #'     time_varying_parameter ~ time_variation_schedule[j]
            // #'   ),
            // #'   mats = list(
            // #'     j = 0,
            // #'     change_points = c(0, 4, 7),
            // #'     time_variation_schedule = c(42, pi, sqrt(2)),
            // #'     time_varying_parameter = empty_matrix
            // #'   ),
            // #'   time_steps = 10,
            // #' )
            // #' set.seed(1L)
            // #' change_points = c(0,2,5)
            // #' x_val = rnorm(length(change_points))
            // #' simple_sims(
            // #'     iteration_exprs = list(x ~ time_var(x_val,change_points))
            // #'   , int_vecs = list(change_points = change_points)
            // #'   , mats = list(x = empty_matrix, x_val=x_val)
            // #'   , time_steps = 10
            // #' )
            // #' ```
            // #'
            case MP2_TIME_STEP: // time_step(lag)
                m = matrix<Type>::Zero(1, 1);
                lag = CppAD::Integer(args[0].coeff(0, 0));
                if (lag < 0)
                {
                    MP2_ERR(MP2_TIME_STEP, "Time lag needs to be non-negative", MP2_TIME_STEP);
                    return m;
                }
                if (t > lag)
                {
                    m.coeffRef(0, 0) = t - lag;
                }
                return m;

            case MP2_TIME_GROUP: // time_group(i, change_points)
                if (index2what[0] != 0) {
                    MP2_ERR(MP2_TIME_GROUP, "First argument needs to be a matrix.", MP2_TIME_GROUP);
                    return m;
                }
                m = args[0];
                off = args.get_as_int(0);
                cp = args.get_as_int_vec(1)[off + 1]; // current pointer
                if (cp == t)
                {
                    m.coeffRef(0, 0) = off + 1;
                }
                return m;

            case MP2_TIME_VAR: // time_var(x, change_points)
                if (t == 0) {
                    MP2_ERR(MP2_TIME_VAR, "Time variation is not allowed before the simulation loop begins.", MP2_TIME_VAR);
                    return m;
                }
                if (t > t_max) {
                    MP2_ERR(MP2_TIME_VAR, "Time variation is not allowed after the simulation loop ends.", MP2_TIME_VAR);
                    return m;
                }

                v = args.get_as_int_vec(1);
                off = args.get_as_int(1);
                if (off < 0) {
                    MP2_ERR(MP2_TIME_VAR, "The first element of the second argument must not be less than zero.", MP2_TIME_VAR);
                    return m;
                }
                if (off >= v.size()) {
                    MP2_ERR(MP2_TIME_VAR, "The first element of the second argument must be less than the number of elements in the first.", MP2_TIME_VAR);
                    return m;
                }
                
                // off is the 'initial offset', which lets the user
                // tells us when (i.e., what time-step) we should start
                // increment.
                // 
                // we have switched this initial offset in the user 
                // interface from zero-based to one-based, which makes
                // much more sense given that time steps are one-based 
                // in macpan2. however we are keeping offsets
                // zero-based in the code, because indexing is
                // zero-based and because of the following minor
                // back-compatibility argument.
                // 
                // the most common case was to set initial offset = 0,
                // meaning we start to increment right away at the
                // first time step. so for back-compatibility we 
                // allow this to mean the same thing as it did before. 
                // in the much less common case of offset > 0
                // (don't think anyone really did this because
                // it wasn't documented) we break back-compatability
                // by shifting user-supplied initial offsets down 
                // by one, which will not matter much for simulations
                // with a 'reasonable' number of simulation steps.
                if (t == 1) {
                  off = off - 1;
                  if (off < 0) off = 0; 
                }

                // first argument can have its rows indexed
                // by the second (curly braces wrap ints in
                // integer vectors so that the function
                // signature is respected. overloading _might_
                // be a better solution. note curly braces
                // used in this way require c++11 i believe.)
                if (off < v.size() - 1) { // might need to increment
                    cp = v[off + 1];
                    if (cp == t) { // yes we need to increment
                        off = off + 1; // so we increment
                        matIndex = index2mats[1];
                        if (index2what[1] == 1) { // int-vec-valued pointer
                            // store the new offset in the zeroth position
                            valid_int_vecs.setNthIntVec(matIndex, off, 0);
                        }
                        else {
                            MP2_ERR(MP2_TIME_VAR, "Time variation pointers need to be length-1 integer vectors.", MP2_TIME_VAR);
                            return m;
                        }
                    }
                    else if (cp < 0) {
                        MP2_ERR(MP2_TIME_VAR, "Negative times are not allowed.", MP2_TIME_VAR);
                        return m;
                    }
                }
                m = matrix<Type>::Zero(args[0].cols(), 1);

                // the row in the input corresponding to the
                // current time, becomes a column vector in
                // the output.
                m.col(0) = args[0].row(off);
                return m;

            case MP2_CONVOLUTION:

                // #' ## Convolution
                // #'
                // #' You can take the convolution of each element in a
                // #' matrix, x, over simulation time using a kernel, k.
                // #'
                // #' ### Functions
                // #'
                // #' * `convolution(x, k)`
                // #'
                // #' ### Arguments
                // #'
                // #' * `x` : The matrix containing elements to be
                // #' convolved.
                // #' * `k` : A column vector giving the convolution kernel.
                // #'
                // #' ### Return
                // #'
                // #' A matrix the same size as `x` but with the
                // #' convolutions, \eqn{y_{ij}}, of each element,
                // #' \eqn{x_{ij}}. The value of \eqn{y_{ij}} at time 
                // #' \eqn{t = 1, ..., T} is given by the following.
                // #'
                // #' \deqn{y_{ij}(t) = \sum_{\tau = 0}^{min(t,m)-1} x_{ij}(t-\tau) k_\tau}
                // #' 
                // #' Where:
                // #' 
                // #' * \eqn{x_{ij}(t)} : value of \eqn{x_{ij}} at time step \eqn{t}.
                // #' * \eqn{y_{ij}(t)} : value of \eqn{y_{ij}} at time step \eqn{t}.
                // #' * \eqn{t = 1, ..., T} : the time step.
                // #' * \eqn{\tau = 0, ..., m - 1} : index of the 
                // #' time lag for a kernel of length \eqn{m}.
                // #' * \eqn{k_\tau} : value of the kernel associated with lag
                // #' \eqn{\tau}.
                // #'
                // #' ### Details
                // #'
                // #' If any empty matrices are encountered when looking
                // #' back in time, they are treated as matrices with all
                // #' zeros. The convolution of a matrix of all positive
                // #' values will be biased low for all time steps less than
                // #' the length of the kernel (i.e., for all time steps 
                // #' such that \eqn{t < m}), and therefore one should only
                // #' compare observed data with a convolution (e.g., when
                // #' calibrating) for time steps less than \eqn{m}.
                // #'
                // #' ### Examples
                // #' 
                // #' ```
                // #' simple_sims(
                // #'   list(
                // #'     x ~ 3 * x * (1 - x),
                // #'     y ~ convolution(x, rep(1/10, 10))
                // #'   ),
                // #'   time_steps = 50,
                // #'   mats = list(x = 0.5, y = empty_matrix)
                // #' )
                // #' ```
                // #' 
                matIndex = index2mats[0]; // m
                if (matIndex == -1) {
                    MP2_ERR(MP2_CONVOLUTION, "Can only convolve named matrices not expressions of matrices", MP2_CONVOLUTION);
                    return args[0];
                }

                #ifdef MP_VERBOSE
                Rcpp::Rcout << "matIndex: " << matIndex << std::endl
                          << std::endl;
                #endif
                length = args[1].rows(); // size of the kernel
                #ifdef MP_VERBOSE
                Rcpp::Rcout << "length: " << length << std::endl
                          << std::endl;
                #endif
                if (length > 0 && args[1].cols() == 1) {
                    #ifdef MP_VERBOSE
                    Rcpp::Rcout << "kernel 1: " << args[1] << std::endl
                              << std::endl;
                    #endif
                    if (t + 1 < length) {
                        length = t + 1;
                        args[1] = args[1].block(0, 0, length, 1);
                    }
                    #ifdef MP_VERBOSE
                    Rcpp::Rcout << "kernel 2: " << args[1] << std::endl
                              << std::endl;
                    #endif

                    rows = args[0].rows();
                    cols = args[0].cols();
                    m = matrix<Type>::Zero(rows, cols);

                    for (int i = 0; i < rows; i++)
                        for (int j = 0; j < cols; j++)
                            m.coeffRef(i, j) = args[1].coeff(0, 0) * valid_vars.m_matrices[matIndex].coeff(i, j);

                    for (int k = 1; k <= length - 1; k++)
                        if (hist[t - k].m_matrices[matIndex].rows() != 0 &&
                            hist[t - k].m_matrices[matIndex].cols() != 0)
                            for (int i = 0; i < rows; i++)
                                for (int j = 0; j < cols; j++)
                                    m.coeffRef(i, j) += args[1].coeff(k, 0) * hist[t - k].m_matrices[matIndex].coeff(i, j);

                    return m;
                }
                else
                {
                    MP2_ERR(MP2_CONVOLUTION, "Either empty or non-column vector used as kernel in convolution", MP2_CONVOLUTION);
                    return m;
                }

            // #' ## Clamp
            // #'
            // #' Smoothly clamp the elements of a matrix so that they
            // #' do not get closer to 0 than a tolerance, `eps`, with
            // #' a default of 1e-12. This `clamp` function is the following 
            // #' modification of the 
            // #' [squareplus function](https://arxiv.org/abs/2112.11687).
            // #'
            // #' \deqn{f(x) = \epsilon_- + \frac{(x - \epsilon_-) + \sqrt{(x - \epsilon_-)^2 + (2\epsilon_0 - \epsilon_-)^2 - \epsilon_-^2}}{2}}
            // #' 
            // #' Where the two parameters are defined as follows.
            // #'
            // #' \deqn{\epsilon_0 = f(0)}
            // #' 
            // #' \deqn{\epsilon_- = \lim_{x \to  -\infty}f(x)}
            // #' 
            // #' This function is differentiable everywhere, monotonically
            // #' increasing, and \eqn{f(x) \approx x} if \eqn{x} is positive
            // #' and not too close to zero. By modifying the parameters, you 
            // #' can control the distance between \eqn{f(x)} and the
            // #' horizontal axis at two 'places' -- \eqn{0} and \eqn{-\infty}.
            // #' [See issue #93](https://github.com/canmod/macpan2/issues/93).
            // #' for more information.
            // #'
            // #' ### Functions
            // #'
            // #' * `clamp(x, eps, limit)`
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : A matrix with elements that should remain positive.
            // #' * `eps` : A small positive number, \eqn{\epsilon_0 = f(0)},
            // #' giving the value of the function when the input is zero.
            // #' The default value is 1e-11
            // #' * `limit` : A small positive number, 
            // #' \deqn{\epsilon_- = \lim_{x \to  -\infty}f(x)}, giving the
            // #' value of the function as the input goes to negative
            // #' infinity. The default is `limit = 1e-12`. This `limit` 
            // #' should be chosen to be less than `eps` to ensure that 
            // #' `clamp` is twice differentiable.
            // #' 
            // #' 
            case MP2_CLAMP:
                eps = 1e-11; // default
                if (n > 1)
                    eps = args[1].coeff(0, 0);
                if (n == 3) {
                    limit = args[2].coeff(0, 0);
                } else {
                    limit = 1e-12; // default
                }
                X = args[0];
                rows = X.rows();
                cols = X.cols();
                m = matrix<Type>::Zero(rows, cols);
                
                // https://github.com/canmod/macpan2/issues/93
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        m.coeffRef(i, j) = limit + (
                            (
                                X.coeff(i, j) - limit + 
                                sqrt(
                                    pow(X.coeff(i, j) - limit, 2.0) + 
                                    pow(2.0 * eps - limit, 2.0) - 
                                    pow(limit, 2.0)
                                )
                            ) / 2.0
                        );
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
            // #' * `dpois(observed, simulated)` : Log of the Poisson density
            // #' based on this [dpois](https://kaskr.github.io/adcomp/group__R__style__distribution.html#gaa1ed15503e1441a381102a8c4c9baaf1)
            // #' TMB function.
            // #' * `dnbinom(observed, simulated, over_dispersion)` :
            // #' Log of the negative binomial density based on this [dnbinom](https://kaskr.github.io/adcomp/group__R__style__distribution.html#ga76266c19046e04b651fce93aa0810351)
            // #' TMB function. To get the variance that this function
            // #' requires we use this expression, \code{simulated + simulated^2/over_dispersion},
            // #' following p.165 in this [book](https://ms.mcmaster.ca/~bolker/emdbook/book.pdf)
            // #' * `dnorm(observed, simulated, standard_deviation)` :
            // #' Log of the normal density based on this [dnorm](https://kaskr.github.io/adcomp/dnorm_8hpp.html)
            // #' TMB function.
            // #' * `dbinom(observed, size, probability)` :
            // #' Log of the binomial density based on the [dbinom](https://kaskr.github.io/adcomp/group__R__style__distribution.html#gaee11f805f02bc1febc6d7bf0487671be)
            // #' TMB function.
            // #'
            // #' ### Arguments
            // #'
            // #' * `observed` : Matrix of observed values
            // #' at which the density is being evaluated.
            // #' * `simulated` : Matrix of distributional means,
            // #' with singleton rows and columns recycled to match
            // #' the numbers of rows and columns in `observed`.
            // #' * `over_dispersion` : Over-dispersion parameter
            // #' given by \code{(simulated/standard_deviation)^2 - simulated)}.
            // #' * `standard_deviation` : Standard deviation parameter.
            // #' * `size` : Number of Bernoulli trials.
            // #' * `probability` : Probability of a successful Bernoulli trial.
            // #'
            case MP2_POISSON_DENSITY:
                if (n < 2) {
                    MP2_ERR(MP2_POISSON_DENSITY, "dpois needs two arguments: matrices with observed and expected values", MP2_POISSON_DENSITY);
                    return m;
                }
                rows = args[0].rows();
                cols = args[0].cols();
                v1.push_back(1);
                args = args.recycle_to_shape(v1, rows, cols);
                err_code = args.get_error_code();
                if (err_code != 0) {
                    MP2_ERR(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", MP2_POISSON_DENSITY);
                    return m;
                }
                m = matrix<Type>::Zero(rows, cols);
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        m.coeffRef(i, j) = dpois(args[0].coeff(i, j), args[1].coeff(i, j), 1);
                    }
                }
                return m;

            case MP2_NEGBIN_DENSITY:
                if (n < 3) {
                    MP2_ERR(MP2_NEGBIN_DENSITY, "dnbinom needs three arguments: matrices with observed values, expected values, and dispersion parameters", MP2_NEGBIN_DENSITY);
                    return m;
                }
                rows = args[0].rows();
                cols = args[0].cols();
                v1.push_back(1);
                v1.push_back(2);
                args = args.recycle_to_shape(v1, rows, cols);
                err_code = args.get_error_code();
                if (err_code != 0) {
                    MP2_ERR(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", MP2_NEGBIN_DENSITY);
                    return m;
                }
                //   var ~ variance
                //   mu ~ mean
                //   k ~ overdispersion parameter = sp[this->spi[0]]
                m = matrix<Type>::Zero(rows, cols);
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        // p.165: https://ms.mcmaster.ca/~bolker/emdbook/book.pdf
                        // mu ~ mean -- args[1]
                        // k ~ overdispersion -- args[2].coeff(i,j)
                        // var = mu + mu^2/k
                        var = args[1].coeff(i, j) + ((args[1].coeff(i, j) * args[1].coeff(i, j)) / args[2].coeff(i, j));
                        m.coeffRef(i, j) = dnbinom2(args[0].coeff(i, j), args[1].coeff(i, j), var, 1);
                    }
                }
                return m;

            case MP2_NORMAL_DENSITY:
                if (n < 3) {
                    MP2_ERR(MP2_NORMAL_DENSITY, "dnorm needs three arguments: matrices with observed values, expected values, and standard deviation parameters", MP2_NORMAL_DENSITY);
                    return m;
                }
                rows = args[0].rows();
                cols = args[0].cols();
                v1.push_back(1);
                v1.push_back(2);
                args = args.recycle_to_shape(v1, rows, cols);
                err_code = args.get_error_code();
                if (err_code != 0) {
                    MP2_ERR(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", MP2_NORMAL_DENSITY);
                    return m;
                }
                m = matrix<Type>::Zero(rows, cols);
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        m.coeffRef(i, j) = dnorm(args[0].coeff(i, j), args[1].coeff(i, j), args[2].coeff(i, j), 1);
                    }
                }
                return m;
                
      	    case MP2_BINOM_DENSITY:
        	      if (n < 3) {
            		    MP2_ERR(MP2_BINOM_DENSITY, "dbinom needs three arguments: matrices with observed values, numbers of trials, and probabilities", MP2_BINOM_DENSITY);
            		    return m;
                }
          	    rows = args[0].rows();
          	    cols = args[0].cols();
          	    v1.push_back(1);
          	    v1.push_back(2);
          	    args = args.recycle_to_shape(v1, rows, cols);
          	    err_code = args.get_error_code();
          	    if (err_code != 0) {
          		      MP2_ERR(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", MP2_BINOM_DENSITY);
          		      return m;
                }
          	    m = matrix<Type>::Zero(rows, cols);
          	    for (int i = 0; i < rows; i++) {
          		      for (int j = 0; j < cols; j++) {
          		            // https://kaskr.github.io/adcomp/group__R__style__distribution.html#gaee11f805f02bc1febc6d7bf0487671be
          		            m.coeffRef(i, j) = dbinom(args[0].coeff(i, j), args[1].coeff(i, j), args[2].coeff(i, j), 1);
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
            // #' * `rpois(mean)` : Pseudo-random Poisson distributed
            // #' values.
            // #' * `rnbinom(mean, over_dispersion)` : Pseudo-random
            // #' negative binomially distributed values.
            // #' * `rnorm(mean, standard_deviation)` : Pseudo-random
            // #' normal values.
            // #' * `rbinom(size, prob)` : Pseudo-random binomial values.
            // #' * `reulermultinom(size, rate, dt)` : Pseudo-random
            // #' [Euler-multinomial](https://kingaa.github.io/manuals/pomp/html/eulermultinom.html)
            // #' values.
            // #'
            // #' ### Arguments
            // #'
            // #' * `mean` : Matrix of means about which to simulate
            // #' pseudo-random variation.
            // #' * `over_dispersion` : Matrix of over-dispersion parameters
            // #' given by \code{(simulated/standard_deviation)^2 - simulated)}.
            // #' * `standard_deviation` : Matrix of standard deviation
            // #' parameters.
            // #' * `size` : Matrix of numbers of trials.
            // #' * `prob` : Matrix of probabilities of success.
            // #' * `rate` : Matrix of rates, used to compute the probabilities
            // #' in a multinomial distribution. The probability associated with
            // #' the `i`th rate, `r_i`, is (1 - exp(-sum(r * dt))) * (r_i / r),
            // #' where `r` is the sum of the rates. This is not a typical
            // #' multinomial distribution in that if you sum these
            // #' probabilities up you do not get `1` but rather 
            // #' `(1 - exp(-sum(r * dt)))`. See details below for more
            // #' on the Euler-multinomial distribution
            // #' * `dt` : Optional parameter specifying the length of the time
            // #' step. See details below for more on the Euler-multinomial
            // #' distribution.
            // #'
            // #' ### Details
            // #' 
            // #' The Euler-multinomial distribution is used to model how
            // #' many individuals move from one compartment to a set of other
            // #' compartments in a single time step of length `dt`. The rate of 
            // #' moving to each of these compartments is characterized by the 
            // #' associated element in the `rate` matrix. The reason why the
            // #' probabilities do not sum to `1`, is that not all individuals
            // #' have to change compartments in a time step.
            // #' 
            case MP2_POISSON_SIM:
                rows = args[0].rows();
                cols = args[0].cols();
                m = matrix<Type>::Zero(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        m.coeffRef(i, j) = rpois(args[0].coeff(i, j));
                    }
                }
                return m;

            case MP2_NEGBIN_SIM:
                if (n < 2)
                {
                    MP2_ERR(MP2_NEGBIN_SIM, "rnbinom needs two arguments: matrices with means and dispersion parameters", MP2_NEGBIN_SIM);
                    return m;
                }
                eps = 1e-8;
                rows = args[0].rows();
                cols = args[0].cols();
                v1.push_back(1);
                args = args.recycle_to_shape(v1, rows, cols);
                err_code = args.get_error_code();
                if (err_code != 0)
                {
                    MP2_ERR(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", MP2_NEGBIN_SIM);
                    return m;
                }
                m = matrix<Type>::Zero(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        var = args[0].coeff(i, j) + ((args[0].coeff(i, j) * args[0].coeff(i, j)) / args[1].coeff(i, j));
                        if (var < eps)
                            // more numerically stable to just set the simulations
                            // to the mean when the var is low
                            m.coeffRef(i, j) = args[0].coeff(i, j);
                        else
                            m.coeffRef(i, j) = rnbinom2(args[0].coeff(i, j), var);
                    }
                }
                return m;

            case MP2_NORMAL_SIM:
                if (n < 2)
                {
                    MP2_ERR(MP2_NORMAL_SIM, "rnorm needs two arguments: matrices with means and standard deviations", MP2_NORMAL_SIM);
                    return m;
                }
                rows = args[0].rows();
                cols = args[0].cols();
                v1.push_back(1);
                args = args.recycle_to_shape(v1, rows, cols);
                err_code = args.get_error_code();
                // err_code = RecycleInPlace(args[1], rows, cols);
                if (err_code != 0)
                {
                    MP2_ERR(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", MP2_NORMAL_SIM);
                    return m;
                }
                m = matrix<Type>::Zero(rows, cols);
                for (int i = 0; i < rows; i++)
                {
                    for (int j = 0; j < cols; j++)
                    {
                        m.coeffRef(i, j) = rnorm(args[0].coeff(i, j), args[1].coeff(i, j));
                    }
                }
                return m;
            
            case MP2_BINOM_SIM:
                // rbinom(size, prob)
                if (n != 2) {
                    MP2_ERR(MP2_BINOM_SIM, "rbinom needs two arguments: matrices with size and probability", MP2_BINOM_SIM);
                    return m;
                }
                rows = args[0].rows();
                cols = args[0].cols();
                v1.push_back(1);
                args = args.recycle_to_shape(v1, rows, cols);
                err_code = args.get_error_code();
                if (err_code != 0) {
                    MP2_ERR(MP2_BINOM_SIM, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", MP2_BINOM_SIM);
                    return m;
                }
                m = matrix<Type>::Zero(rows, cols);
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        m.coeffRef(i, j) = mp2_rbinom(args[0].coeff(i, j), args[1].coeff(i, j));
                    }
                }
                return m;
            
            case MP2_EULER_MULTINOM_SIM:
                // reulermultinom(size, rate, dt)
                if (n == 3) {
                    delta_t = args[2].coeff(0, 0);
                } else {
                    delta_t = 1.0;
                }
                if ((args[0].rows() != 1) || (args[0].cols() != 1)) {
                    //Rcpp::Rcout << "++++++" << std::endl;
                    //Rcpp::Rcout << args[0] << std::endl;
                    MP2_ERR(MP2_EULER_MULTINOM_SIM, "The first 'size' argument must be scalar.", MP2_EULER_MULTINOM_SIM);
                    return m;
                }
                if (args[1].cols() != 1) {
                    //Rcpp::Rcout << "------" << std::endl;
                    //Rcpp::Rcout << args[1] << std::endl;
                    MP2_ERR(MP2_EULER_MULTINOM_SIM, "The second 'rate' argument must be a column vector.", MP2_EULER_MULTINOM_SIM);
                }
                sum = args[1].sum();
                //Rcpp::Rcout << "sum of rates: " << sum << std::endl;
                m = matrix<Type>::Zero(args[1].rows(), 1);  // multinomial probabilities
                for (int i = 0; i < args[1].rows(); i++) {
                    m.coeffRef(i, 0) = args[1].coeff(i, 0) * delta_t; // not yet multinomial probabilities in m, but there will be
                }
                p0 = exp(-m.sum()); // probability of staying
                //Rcpp::Rcout << "prob(staying): " << p0 << std::endl;
                p0 = (1 - p0) / sum; // transform the prob(staying) into the 'rate multiplier'
                //Rcpp::Rcout << "rate multiplier: " << p0 << std::endl;
                for (int i = 0; i < args[1].rows(); i++) {
                    m.coeffRef(i, 0) = p0 * args[1].coeff(i, 0); // now we actually fill m with multinomial probabilities
                }
                //Rcpp::Rcout << "multinomial probabilities: " << m << std::endl;
                
                m1 = matrix<Type>::Zero(args[1].rows(), 1);  // multinomial outcomes
                
                // rounded size, called left_over because it will be
                // updated as we loop through the categories of the
                // multinomial distribution
                left_over = mp2_round(args[0].coeff(0, 0));
                
                remaining_prop = 1.0;
                for (int i = 0; i < m1.rows(); i++) {
                    //m1.coeffRef(i, 0) = ((left_over > 0.0) && ((m.coeff(i, 0) / remaining_prop) > 0.0)) ? 1.0 * rbinom(left_over, m.coeff(i, 0) / remaining_prop) : 0.0;
                    m1.coeffRef(i, 0) = mp2_rbinom(left_over, m.coeff(i, 0) / remaining_prop); // 0/0 could be an issue
                    left_over -= m1.coeff(i, 0);
                    remaining_prop -= m.coeff(i, 0);
                }
                // m1.coeffRef(m1.rows() - 1, 0) = left_over;
                return m1;
            
            
            // #' ## Cumulative Distribution Functions
            // #'
            // #' Lower-tail cumulative distribution functions.
            // #' 
            // #' ### Functions
            // #'
            // #' * `pgamma(q, shape, scale)` : Cumulative distribution function
            // #' of the Gamma distribution. This is a lite wrapper for the
            // #' [pgamma function in TMB](https://kaskr.github.io/adcomp/group__R__style__distribution.html#ga3bd06a324f89b21694aac26bfe1aef45).
            // #' * `pnorm(q, mean, sd)` : Cumulative distribution function of
            // #' the normal distribution. This is a lite wrapper for the 
            // #' [pnorm function in TMB](https://kaskr.github.io/adcomp/group__R__style__distribution.html#ga2a3cc5a06500670aeaf6eb911a1094d9).
            // #' 
            // #' ### Arguments
            // #'
            // #' * `q` : Matrix of Quantiles.
            // #' * `shape` : Matrix of shape parameters of the Gamma distribution.
            // #' * `scale` : Matrix of scale parameters of the Gamma distribution.
            // #' * `mean` : Matrix of mean parameters of the normal distribution.
            // #' * `sd` : Matrix of standard deviation parameters of the normal distribution.
            // #'
            case MP2_PGAMMA:
                rows = args[0].rows();
                cols = args[0].cols();
                v1.push_back(1);
                v1.push_back(2);
                args = args.recycle_to_shape(v1, rows, cols);
                err_code = args.get_error_code();
                if (err_code != 0) {
                    MP2_ERR(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", MP2_PGAMMA);
                    return m;
                }
                m1 = args.get_as_mat(0); // q
                m2 = args.get_as_mat(1); // shape
                m3 = args.get_as_mat(2); // scale
                m = matrix<Type>::Zero(rows, cols);
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        m.coeffRef(i, j) = pgamma(m1.coeff(i, j), m2.coeff(i, j), m3.coeff(i, j));
                    }
                }
                return m;
            
            case MP2_PNORM:
                rows = args[0].rows();
                cols = args[0].cols();
                v1.push_back(1);
                v1.push_back(2);
                args = args.recycle_to_shape(v1, rows, cols);
                err_code = args.get_error_code();
                if (err_code != 0) {
                    MP2_ERR(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", MP2_PGAMMA);
                    return m;
                }
                m1 = args.get_as_mat(0); // q
                m2 = args.get_as_mat(1); // mean
                m3 = args.get_as_mat(2); // sd
                m = matrix<Type>::Zero(rows, cols);
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        m.coeffRef(i, j) = pnorm(m1.coeff(i, j), m2.coeff(i, j), m3.coeff(i, j));
                    }
                }
                return m;
            
            // #' ## Rounding
            // #' 
            // #' ### Functions
            // #' 
            // #' round(x) : Round elements of a matrix to the nearest integer.
            // #' 
            // #' ### Arguments
            // #' 
            // #' * `x` : Matrix to be rounded.
            // #'
            // #' ### Details
            // #'
            // #' Be careful if you are using rounding in a model to be calibrated.
            // #' Rounding will break differentiability of the objective function
            // #' if `x` depends, either directly or indirectly, on parameters 
            // #' being calibrated. This will lead to incorrect gradients
            // #' potentially being passed to an optimizer. To be safe, do
            // #' not round in models being calibrated.
            // #' 
            case MP2_ROUND:
                m = mp2_round(args[0]);
                return m;
            
            // #' ## Debugging Instrumentation
            // #' 
            // #' Functions to use when you are trying to figure stuff out.
            // #' 
            // #' ### Functions
            // #'
            // #' * `print(x)` : Print out the value of a matrix.
            // #' * `check_finite(x)` : Stop the simulations and return an
            // #' error if `x` has any non-finite values.
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : Name of a matrix in the model.
            // #'
            // #' ### Return
            // #'
            // #' An \code{\link{empty_matrix}}.
            // #'
            // #' ### Examples
            // #'
            // #' ```
            // #' simple_sims(
            // #'      list(dummy ~ print(x), x ~ x / 2)
            // #'    , time_steps = 10
            // #'    , mats = list(x = 2)
            // #' )
            // #' engine_eval(~ 1/0) ## returns Inf
            // #' engine_eval(~ check_finite(1/0)) ## returns nothing and throws an error
            // #' ```
            // #' 
            case MP2_PRINT:
                Rcpp::Rcout << "printing matrix number " << index2mats[0] << " at time step " << t << " :" << std::endl;
                Rcpp::Rcout << args[0] << std::endl;
                return m;

            case MP2_CHECK_FINITE:
                m = args.get_as_mat(0);
                is_finite_mat = m.array().isFinite().all();
                if (!is_finite_mat) {
                    MP2_ERR(123, "Some elements of this matrix are not finite.", MP2_CHECK_FINITE);
                }
                return m;
            
            // #' ## Assign (deprecated)
            // #'
            // #' Assign values to a subset of the elements in a matrix.
            // #'
            // #' ### Functions
            // #'
            // #' * `assign(x, i, j, v)`
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : Matrix with elements that are to be updated
            // #' by the values in `v`.
            // #' * `i` : Column vector of row indices pointing to
            // #' the elements of `x` to be updated. These indices are
            // #' paired with those in `v`. If the length of
            // #' `i` does not equal that of `v`, then it must have a
            // #' single index that gets paired with every element of
            // #' `v`. Indices are zero-based, `i=0` corresponds to 
            // #' the first row.
            // #' * `j` : Column vector of column indices pointing to
            // #' the elements of `x` to be updated. These indices are
            // #' paired with those in `v`. If the length of
            // #' `j` does not equal that of `v`, then it must have a
            // #' single index that gets paired with every element of
            // #' `v`. Indices are zero-based, `j=0` corresponds to
            // #' the first column.
            // #' * `v` : Column vector of values to replace elements
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
            case MP2_ASSIGN:

                cols = args[1].cols();
                if (cols != 1)
                {
                    MP2_ERR(255, "Assignment index matrices must have a single column", MP2_ASSIGN);
                    return m;
                }
                cols = args[2].cols();
                if (cols != 1)
                {
                    MP2_ERR(255, "Assignment index matrices must have a single column", MP2_ASSIGN);
                    return m;
                }
                cols = args[3].cols();
                if (cols != 1)
                {
                    MP2_ERR(255, "Assignment value matrices must have a single column", MP2_ASSIGN);
                    return m;
                }

                // apparently we still need this check
                v1 = args.get_as_int_vec(1);
                v2 = args.get_as_int_vec(2);
                // printIntVector(v1);
                // printIntVector(v2);
                err_code = args.check_indices(0, v1, v2); // CheckIndices(args[0], args[1], m1);
                // err_code = CheckIndices(args[0], args[1], args[2]);
                if (err_code)
                {
                    MP2_ERR(MP2_ASSIGN, "Illegal index used in assign", MP2_ASSIGN);
                    return m;
                }

                rows = args[3].rows();
                v3.push_back(1);
                v3.push_back(2);
                args = args.recycle_to_shape(v3, rows, cols);
                err_code = args.get_error_code();
                if (err_code != 0) {
                    MP2_ERR(err_code, "cannot recycle rows and/or columns because the input is inconsistent with the recycling request", MP2_ASSIGN);
                    return m;
                }

                for (int k = 0; k < rows; k++) {
                    rowIndex = CppAD::Integer(args[1].coeff(k, 0));
                    colIndex = CppAD::Integer(args[2].coeff(k, 0));
                    matIndex = index2mats[0];
                    if (matIndex == -1) {
                        MP2_ERR(MP2_ASSIGN, "Can only assign to named matrices not expressions of matrices", MP2_ASSIGN);
                        return args[0];
                    }
                    valid_vars.m_matrices[matIndex].coeffRef(rowIndex, colIndex) = args[3].coeff(k, 0);
                }
                return m2; // empty matrix

            // #' ## Unpack (deprecated)
            // #'
            // #' Unpack elements of a matrix into smaller matrices.
            // #'
            // #' ### Functions
            // #'
            // #' * `unpack(x, ...)`
            // #'
            // #' ### Arguments
            // #'
            // #' * `x` : Matrix with elements to be distributed to
            // #' the matrices passed through `...`.
            // #' * `...` : Matrices with elements to be replaced by
            // #' the values of elements in `x` in column-major order.
            // #' These matrices must be named matrices and not
            // #' computed on the fly using expressions. Note that even
            // #' subsetting (e.g. `unpack(x, y[0], y[3])`) counts as
            // #' an expression. This use-case would require the
            // #' \code{\link{assign}} function
            // #' `assign(y, c(0, 3), 0, x)`.
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
            case MP2_UNPACK:

                // matIndex = index2mats[0]; // m
                // valid_vars.m_matrices[matIndex]

                m = args[0];
                size = m.rows() * m.cols();
                m.resize(size, 1);

                start = 0;
                for (int i = 1; i < n; i++)
                {
                    sz = args[i].rows() * args[i].cols();
                    if (size >= sz)
                    {
                        m1 = m.block(start, 0, sz, 1);
                        m1.resize(args[i].rows(), args[i].cols());
                        // Rcpp::Rcout << "MATRIX " << valid_vars.m_matrices[index2mats[i]] << std::endl << std::endl;
                        matIndex = index2mats[i];
                        if (matIndex == -1)
                        {
                            MP2_ERR(MP2_ASSIGN, "Can only unpack into named matrices not expressions of matrices", MP2_UNPACK);
                            return args[0];
                        }
                        valid_vars.m_matrices[matIndex] = m1;
                        // args[i] = m1;
                        size -= sz;
                        start += sz;
                    }
                    else
                        break;
                }
                return m2; // empty matrix
            
            default:
                MP2_ERR(255, "invalid operator in arithmetic expression", -99);
                return m;
            }
        } // switch (table_n[row])
    };

private:
    unsigned char error_code;
    int expr_row;
    int func_int;
    int time_int;
    std::vector<int> arg_rows;
    std::vector<int> arg_cols;
    std::vector<int> arg_type_ints;
    char error_message[256];
};

template <class Type>
class MatAssigner {
private:
    vector<int> table_x;
    vector<int> table_n;
    vector<int> table_i;
    ListOfIntVecs valid_int_vecs;
    vector<Type> valid_literals;

public:
    MatAssigner(
        vector<int> &table_x_,
        vector<int> &table_n_,
        vector<int> &table_i_,
        ListOfIntVecs &valid_int_vecs_,
        vector<Type> &valid_literals_) {
        table_x = table_x_;
        table_n = table_n_;
        table_i = table_i_;
        valid_int_vecs = valid_int_vecs_;
        valid_literals = valid_literals_;
    };

    void matAssign(
        matrix<Type> assignment_value,
        ListOfMatrices<Type> &valid_vars, // current list of values of each matrix
        int row = 0                       // current expression parse table row being evaluated
    ) {
        int n = table_n[row];
        int x = table_x[row];
        int x1;
        int x2;
        int sz;
        int nr;
        int nc;
        int size;
        int start;
        matrix<Type> m;
        matrix<Type> m1;
        std::vector<int> v1;
        std::vector<int> v2;
        int err_code;
        // Rcpp::Rcout << "---- assignment ----" << std::endl;
        // Rcpp::Rcout << "n: " << n << std::endl;
        // Rcpp::Rcout << "x: " << n << std::endl;
        switch (n) {
            case 0:
                valid_vars.m_matrices[x] = assignment_value;
                return;
            case -1:
                Rf_error("trying to assign to a literal, which is not allowed");
            case -2:
                Rf_error("trying to assign to an engine method, which is not allowed");
            case -3:
                Rf_error("trying to assign to an integer vector, which is not allowed");
        }
        // if we make it here we have a function on the left-hand-side.
        // i.e. the number of arguments n > 0 (ignoring n < -3, which should 
        // never get here because it would violate the spec). so now we need 
        // to switch on the particular function being used. at most one 
        // function can be used on the left-hand-side, and only particular ones 
        // can be used as the switch statement shows
        // 
        // #' ## Assignment
        // #'
        // #' The left-hand-side of formulas sent to the simulation engine
        // #' determine assignment works.
        // #' 
        // #' ### Functions
        // #'
        // #' * `y ~ x` : Assign `x` to `y`.
        // #' * `y[i] ~ x` : Assign the first column of `x` to those rows
        // #' in the first column of `y` that are indexed by `i`.
        // #' * `y[i, j] ~ x` : Assign each element, `x[k, l]`, in `x`,
        // #' to element, `y[i[k], j[l]]`, in `y`.
        // #' * `c(...) ~ x` : Assign the elements of the columns of `x`
        // #' (stacked on top of each other) to the matrices in `...` in the 
        // #' order in which they appear. If the number of columns is `x` 
        // #' equals the number of matrices in `...`, and if these matrices
        // #' are vectors (i.e., have only a single column or a single row),
        // #' then the columns of `x` become assigned to the vectors in `...`.
        // #' 
        // #' ### Arguments
        // #' 
        // #' * `x` : Matrix containing the result of the expression on the
        // #' right-hand-side.
        // #' * `y` : Matrix with elements that will be assigned the elements
        // #' of `x`.
        // #' * `i` : Integer vector giving zero-based row indexes describing
        // #' the rows in `x` that get the 
        // #' 
        switch (x + 1) {
            case MP2_SQUARE_BRACKET:
                if (n == 3) { // two index vectors
                    if (table_n[table_i[row] + 2] == -1) { // second index vector is a literal
                        v2.push_back(CppAD::Integer(valid_literals[table_x[table_i[row] + 2]]));
                        //Rf_error("indexing on the left-hand-side cannot be done using literals");
                    } else if (table_n[table_i[row] + 2] != -3) { // second index vector is not an integer vector
                        Rf_error("indexing on the left-hand-side needs to be done using integer vectors or literals");
                    }
                } else if (n == 2){ // one index vector
                    v2.push_back(0);  // assume the second index vector is length-1 with a 0 (i.e. points to the first column)
                } else { // too many index vectors
                    Rf_error("only two index arguments allowed to square brackets on the left-hand-side");
                }
                if (table_n[table_i[row] + 1] == -1) { // first index vector is a literal
                    v1.push_back(CppAD::Integer(valid_literals[table_x[table_i[row] + 1]]));
                } else if (table_n[table_i[row] + 1] != -3) { // first index vector is not an integer vector or literal
                    Rf_error("indexing on the left-hand-side needs to be done using integer vectors or literals");
                } else { // first index is an integer vector
                    v1 = valid_int_vecs[table_x[table_i[row] + 1]];
                }
                x1 = table_x[table_i[row]];

                m = valid_vars.m_matrices[x1];
                if (n == 3)
                    v2 = valid_int_vecs[table_x[table_i[row] + 2]];
    
                err_code = CheckIndices(m, v1, v2);
                if (err_code) {
                    Rf_error("assignment indexes are out of range");
                }
    
                for (unsigned int i = 0; i < v1.size(); i++) {
                    for (unsigned int j = 0; j < v2.size(); j++) {
                        m.coeffRef(v1[i], v2[j]) = assignment_value.coeff(i, j);
                    }
                }
                valid_vars.m_matrices[x1] = m;
                return;

            case MP2_COMBINE:
                // vectorize the right-hand-side so that it is a stacked 
                // column vector
                m = assignment_value;
                size = m.rows() * m.cols();
                m.resize(size, 1);
                //Rcpp::Rcout << "assignment: " << m << std::endl;
              
                start = 0;
                for (int i = 0; i < n; i++) {
                    x2 = table_x[table_i[row]+i]; // index (in valid_vars) to ith argument of `c`
                    //Rcpp::Rcout << "recipient: " << valid_vars.m_matrices[x2] << std::endl;
                    sz = valid_vars.m_matrices[x2].rows() * valid_vars.m_matrices[x2].cols();
                    if (sz == 0) sz = size / (n - i);  // heuristic
                    //Rcpp::Rcout << "sz: " << sz << std::endl;
                    //Rcpp::Rcout << "size: " << size << std::endl;
                    //Rcpp::Rcout << "start: " << start << std::endl;
                    if (size >= sz) {
                        m1 = m.block(start, 0, sz, 1);
                        nr = valid_vars.m_matrices[x2].rows();
                        nc = valid_vars.m_matrices[x2].cols();
                        if (nr == 0) nr = sz;
                        if (nc == 0) nc = 1;
                        m1.resize(nr, nc);
                        //Rcpp::Rcout << "this replacement block: " << m1 << std::endl;
                        valid_vars.m_matrices[x2] = m1;
                        //Rcpp::Rcout << "recipient after: " << valid_vars.m_matrices[x2] << std::endl;
                        size -= sz;
                        start += sz;
                    }
                    else
                        break;
                }
                // Rcpp::Rcout << "size at end: " << size << std::endl;
                // Rcpp::Rcout << "size at end: " << size << std::endl;
                return;
        } // switch (x + 1)
        
        Rf_error("square bracket (e.g. x[i, j]) and concatenation (e.g. c(x, y, z)) are the only functions allowed on the left-hand-side");
    };
};

#define REPORT_ERROR                                             \
    {                                                            \
        int error = exprEvaluator.GetErrorCode();                \
        int expr_row = exprEvaluator.GetExprRow();               \
        vector<int> arg_rows = exprEvaluator.GetArgRows();       \
        vector<int> arg_cols = exprEvaluator.GetArgCols();       \
        vector<int> arg_type_ints = exprEvaluator.GetArgTypeInts(); \
        int func_int = exprEvaluator.GetFuncInt();               \
        int time_int = exprEvaluator.GetTimeInt();               \
        const char *err_msg = exprEvaluator.GetErrorMessage();   \
        REPORT(error);                                           \
        REPORT(expr_row);                                        \
        REPORT(func_int);                                        \
        REPORT(time_int);                                        \
        REPORT(arg_rows);                                        \
        REPORT(arg_cols);                                        \
        REPORT(arg_type_ints);                                   \
                                                                 \
        logfile.open(log_file, std::ios_base::app);              \
        logfile << "Error code = " << error << std::endl;        \
        logfile << "Error message = " << err_msg << std::endl;   \
        logfile << "Expression row = " << expr_row << std::endl; \
        logfile << "Function code = " << func_int << std::endl;  \
        logfile << "Time step = " << time_int << std::endl;      \
        logfile.close();                                         \
    }

template <class Type>
vector<ListOfMatrices<Type>> MakeSimulationHistory(
    const int time_steps,
    const vector<int> &mats_save_hist,
    ListOfMatrices<Type> &hist_shape_template)
{

    vector<ListOfMatrices<Type>> simulation_history(time_steps + 2);
    matrix<Type> empty_matrix;
    for (unsigned int i = 0; i < mats_save_hist.size(); i++)
        //Rcpp::Rcout << "matrix: " << size << std::endl;
        if (mats_save_hist[i] == 0)
            hist_shape_template.m_matrices[i] = empty_matrix;

    return simulation_history;
}

// Helper function
template <class Type>
void UpdateSimulationHistory(
    vector<ListOfMatrices<Type>> &hist,
    int t,
    const ListOfMatrices<Type> &mats,
    const vector<int> &mats_save_hist,
    ListOfMatrices<Type> &hist_shape_template)
{
    // matrix<Type> emptyMat;
    // ListOfMatrices<Type> ms(mats);
    // if the history of the matrix is not to be saved,
    // just save a 1-by-1 with a zero instead to save space
    for (unsigned int i = 0; i < mats_save_hist.size(); i++)
        if (mats_save_hist[i] != 0)
            hist_shape_template.m_matrices[i] = mats.m_matrices[i];

    hist[t] = hist_shape_template;
}


// "main" function
template <class Type>
Type objective_function<Type>::operator()()
{
#ifdef MP_VERBOSE
    Rcpp::Rcout << "============== objective_function =============" << std::endl;
#endif

    // Log file path
    DATA_STRING(log_file);
    if (!fileExists(log_file)) log_file = bail_out_log_file;
    std::ofstream logfile;
    logfile.open(log_file);
    logfile << "======== macpan2 log file ========\n";
    logfile.close();
    

    std::setprecision(9); // Set the precision of Rcpp::Rcout

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
    DATA_INTEGER(time_steps);

    // Expressions and parse table
    DATA_IVECTOR(a_table_x);
    DATA_IVECTOR(a_table_n);
    DATA_IVECTOR(a_table_i);
    DATA_IVECTOR(assign_num_a_table_rows);
    DATA_IVECTOR(expr_sim_block);
    DATA_IVECTOR(expr_num_p_table_rows);
    DATA_IVECTOR(eval_schedule)
    DATA_IVECTOR(p_table_x);
    DATA_IVECTOR(p_table_n);
    DATA_IVECTOR(p_table_i);

    // Literals
    DATA_VECTOR(literals);

    // Methods
    DATA_IVECTOR(meth_type_id);
    DATA_IVECTOR(meth_n_mats);
    DATA_IVECTOR(meth_n_int_vecs);
    DATA_IVECTOR(meth_mat_id);
    DATA_IVECTOR(meth_int_vec_id);

    // Constant Integer Vectors
    DATA_IVECTOR(const_int_vec);
    DATA_IVECTOR(const_n_int_vecs);

    // Objective function parse table
    DATA_IVECTOR(o_table_n);
    DATA_IVECTOR(o_table_x);
    DATA_IVECTOR(o_table_i);

    // Flags
    DATA_INTEGER(values_adreport);

    // Rcpp::Rcout << "=======================" << std::endl;

#ifdef MP_VERBOSE
    Rcpp::Rcout << "params = " << params << std::endl;

    Rcpp::Rcout << "random = " << random << std::endl;

    n = mats.m_matrices.size();
    for (int i = 0; i < n; i++)
        Rcpp::Rcout << "mats = " << mats.m_matrices[i] << std::endl;

    Rcpp::Rcout << "p_par_id = " << p_par_id << std::endl;
    Rcpp::Rcout << "p_mat_id = " << p_mat_id << std::endl;
    Rcpp::Rcout << "p_row_id = " << p_row_id << std::endl;
    Rcpp::Rcout << "p_col_id = " << p_col_id << std::endl;

    Rcpp::Rcout << "r_par_id = " << r_par_id << std::endl;
    Rcpp::Rcout << "r_mat_id = " << r_mat_id << std::endl;
    Rcpp::Rcout << "r_row_id = " << r_row_id << std::endl;
    Rcpp::Rcout << "r_col_id = " << r_col_id << std::endl;

    Rcpp::Rcout << "time_steps = " << time_steps << std::endl;

    Rcpp::Rcout << "mats_save_hist = " << mats_save_hist << std::endl;
    Rcpp::Rcout << "mats_return = " << mats_return << std::endl;

    Rcpp::Rcout << "eval_schedule = " << eval_schedule << std::endl;

    // Rcpp::Rcout << "expr_output_id = " << expr_output_id << std::endl;
    Rcpp::Rcout << "expr_sim_block = " << expr_sim_block << std::endl;
    Rcpp::Rcout << "expr_num_p_table_rows = " << expr_num_p_table_rows << std::endl;

    Rcpp::Rcout << "p_table_x = " << p_table_x << std::endl;
    Rcpp::Rcout << "p_table_n = " << p_table_n << std::endl;
    Rcpp::Rcout << "p_table_i = " << p_table_i << std::endl;

    Rcpp::Rcout << "literals = " << literals << std::endl;

    Rcpp::Rcout << "o_table_x = " << o_table_x << std::endl;
    Rcpp::Rcout << "o_table_n = " << o_table_n << std::endl;
    Rcpp::Rcout << "o_table_i = " << o_table_i << std::endl;
#endif

    // 2 Replace some of elements of some matrices with parameters
    n = p_par_id.size();
    for (int i = 0; i < n; i++)
        mats.m_matrices[p_mat_id[i]].coeffRef(p_row_id[i], p_col_id[i]) = params[p_par_id[i]];

    n = r_par_id.size();
    for (int i = 0; i < n; i++)
        mats.m_matrices[r_mat_id[i]].coeffRef(r_row_id[i], r_col_id[i]) = random[r_par_id[i]];

    // Simulation history
    /// each element of this history 'vector' is a list of the matrices
    /// in the model at a particular point in history
    ListOfMatrices<Type> hist_shape_template(mats);
    vector<ListOfMatrices<Type>> simulation_history = MakeSimulationHistory(
        time_steps,
        mats_save_hist,
        hist_shape_template);

    ListOfIntVecs const_int_vecs(const_int_vec, const_n_int_vecs);
    ListOfIntVecs meth_mats(meth_mat_id, meth_n_mats);
    ListOfIntVecs meth_int_vecs(meth_int_vec_id, meth_n_int_vecs);

    ExprEvaluator<Type> exprEvaluator(
        mats_save_hist,
        p_table_x,
        p_table_n,
        p_table_i,
        meth_type_id,
        meth_mats,
        meth_int_vecs,
        const_int_vecs,
        literals);
    ExprEvaluator<Type> objFunEvaluator(
        mats_save_hist, // this seems odd given that objective functions can't access history
        o_table_x,
        o_table_n,
        o_table_i,
        meth_type_id,
        meth_mats,
        meth_int_vecs,
        const_int_vecs,
        literals);
    MatAssigner<Type> matAssigner(
        a_table_x,
        a_table_n,
        a_table_i,
        const_int_vecs,
        literals);

    // 3 Pre-simulation (the 'before' step)
    int expr_index = 0;
    int p_table_row = 0;
    int a_table_row = 0;

    for (int i = 0; i < eval_schedule[0]; i++) { // loop over expressions in the before step
#ifdef MP_VERBOSE
        Rcpp::Rcout << "in pre-simulation --- " << i << std::endl;
        Rcpp::Rcout << "expr_num_p_table_rows[i] " << expr_num_p_table_rows[i] << std::endl;
#endif
        matrix<Type> result;
        if (expr_sim_block[i] == 1) {
            SIMULATE {
                result = exprEvaluator.EvalExpr(
                    simulation_history, 0, mats, p_table_row);
            }
        }
        else
            result = exprEvaluator.EvalExpr(
                simulation_history, 0, mats, p_table_row);

        if (exprEvaluator.GetErrorCode()) {
            REPORT_ERROR
            return 0.0;
        }

        // mats.m_matrices[expr_output_id[expr_index+i]] = result;
        matAssigner.matAssign(result, mats, a_table_row);

        p_table_row += expr_num_p_table_rows[i];
        a_table_row += assign_num_a_table_rows[i];
        
        // if expr is the last one in the next expr loop, then go back
        // to the start of that loop
    } // p_table_row is fine here

    // simulation_history[0] = mats;
    UpdateSimulationHistory(
        simulation_history,
        0,
        mats,
        mats_save_hist,
        hist_shape_template);

    // 4 During simulation (the 'during' step)
    expr_index += eval_schedule[0];

    // p_table_row2 lets us restart the parse table row every time the
    // simulation loop is iterated
    int p_table_row2 = p_table_row;
    int a_table_row2 = a_table_row;
    for (int k = 0; k < time_steps; k++) {
        p_table_row2 = p_table_row;
        a_table_row2 = a_table_row;
#ifdef MP_VERBOSE
        Rcpp::Rcout << "simulation step --- " << k << std::endl;
#endif
        for (int i = 0; i < eval_schedule[1]; i++) { // for each expression in the 'during' list
#ifdef MP_VERBOSE
            Rcpp::Rcout << "Eval expression --- " << i << std::endl;
            Rcpp::Rcout << "expr_num_p_table_rows[i] " << expr_num_p_table_rows[expr_index + i] << std::endl;
#endif
            matrix<Type> result;
            if (expr_sim_block[i] == 1) {
                SIMULATE {
                    result = exprEvaluator.EvalExpr(
                        simulation_history, k + 1, mats, p_table_row2);
                }
            }
            else
                result = exprEvaluator.EvalExpr(
                    simulation_history, k + 1, mats, p_table_row2);

            if (exprEvaluator.GetErrorCode()) {
                REPORT_ERROR
                return 0.0;
            }
            // mats.m_matrices[expr_output_id[expr_index+i]] = result;
            matAssigner.matAssign(result, mats, a_table_row2);

            p_table_row2 += expr_num_p_table_rows[expr_index + i];
            a_table_row2 += assign_num_a_table_rows[expr_index + i];

#ifdef MP_VERBOSE
            int n = mats.m_matrices.size();
            for (int ii = 0; ii < n; ii++)
                Rcpp::Rcout << "mats = " << mats.m_matrices[ii] << std::endl;
#endif
        }
        // simulation_history[k+1] = mats;
        UpdateSimulationHistory(
            simulation_history,
            k + 1,
            mats,
            mats_save_hist,
            hist_shape_template);
    }
    p_table_row = p_table_row2;
    a_table_row = a_table_row2;

    // 5 Post-simulation (the 'after' set of expressions)
    expr_index += eval_schedule[1];

    for (int i = 0; i < eval_schedule[2]; i++) { // loop over the 'after' expressions
#ifdef MP_VERBOSE
        Rcpp::Rcout << "in post-simulation --- " << i << std::endl;
        Rcpp::Rcout << "expr_num_p_table_rows[i] " << expr_num_p_table_rows[expr_index + i] << std::endl;
#endif
        matrix<Type> result;
        if (expr_sim_block[i] == 1) {
            SIMULATE {
                result = exprEvaluator.EvalExpr(
                    simulation_history, time_steps + 1, mats, p_table_row);
            }
        }
        else {
            result = exprEvaluator.EvalExpr(
                simulation_history, time_steps + 1, mats, p_table_row);
        }

        if (exprEvaluator.GetErrorCode()) {
            REPORT_ERROR
            return 0.0;
        }

        // mats.m_matrices[expr_output_id[expr_index+i]] = result;
        matAssigner.matAssign(result, mats, a_table_row);

        p_table_row += expr_num_p_table_rows[expr_index + i];
        a_table_row += assign_num_a_table_rows[expr_index + i];
    }

    // simulation_history[time_steps+1] = mats;
    UpdateSimulationHistory(
        simulation_history,
        time_steps + 1,
        mats,
        mats_save_hist,
        hist_shape_template);

#ifdef MP_VERBOSE
    Rcpp::Rcout << "Simulation history ..." << std::endl;
    int m = simulation_history.size();
    for (int t = 0; t < m; t++) {
        Rcpp::Rcout << "----- t = " << t << std::endl;
        ListOfMatrices<Type> mats = simulation_history[t];
        int n = mats.m_matrices.size();
        for (int i = 0; i < n; i++)
            Rcpp::Rcout << "mats = " << mats.m_matrices[i] << std::endl;
    }
#endif

    // 6 Report a table as history, each row contains
    //   mat_id, time_step, row_id, col_id, value (This is the the order the current
    //   implementation uses)
    //   These indices are all zero-based.

    // int r = 0;
    int table_rows = 0;
    for (unsigned int i = 0; i < mats_return.size(); i++) {
        if (mats_return[i] == 1) {
            if (mats_save_hist[i] == 0) { // Report the last one
                table_rows += mats.m_matrices[i].rows() * mats.m_matrices[i].cols();
            }
            else { // Report the whole simulation history
                int hist_len = time_steps + 2;
                for (int k = 0; k < hist_len; k++)
                    table_rows += simulation_history[k].m_matrices[i].rows() *
                                  simulation_history[k].m_matrices[i].cols();
            }
        }
    }

    matrix<Type> values(table_rows, 5);

    int cur = 0;
    for (unsigned int i = 0; i < mats_return.size(); i++) {
        if (mats_return[i] == 1) {
            if (mats_save_hist[i] == 0) { // Report the last one
                for (int jj = 0; jj < mats.m_matrices[i].cols(); jj++)
                    for (int ii = 0; ii < mats.m_matrices[i].rows(); ii++) {
                        values(cur, 0) = i;
                        values(cur, 1) = time_steps + 1;
                        values(cur, 2) = ii;
                        values(cur, 3) = jj;
                        values(cur, 4) = mats.m_matrices[i].coeff(ii, jj);
                        cur++;
                    }
            }
            else { // Report the whole simulation history
                int hist_len = time_steps + 2;
                for (int k = 0; k < hist_len; k++)
                    for (int jj = 0; jj < simulation_history[k].m_matrices[i].cols(); jj++)
                        for (int ii = 0; ii < simulation_history[k].m_matrices[i].rows(); ii++) {
                            values(cur, 0) = i;
                            values(cur, 1) = k;
                            values(cur, 2) = ii;
                            values(cur, 3) = jj;
                            values(cur, 4) = simulation_history[k].m_matrices[i].coeff(ii, jj);
                            cur++;
                        }
            }
        }
    }

    REPORT(values)
    if (values_adreport == 1) {
        matrix<Type> value_column = values.block(0, 4, values.rows(), 1);
        ADREPORT(value_column)
    }

    // 7 Calc the return of the objective function
    matrix<Type> ret;
    ret = objFunEvaluator.EvalExpr(simulation_history, time_steps + 2, mats, 0);
    if (ret.size() != 1) Rf_error("Objective function did not return a scalar.");

    if (exprEvaluator.GetErrorCode()) {
        REPORT_ERROR;
        return 0.0;
    }

    REPORT_ERROR

#ifdef MP_VERBOSE
    Rcpp::Rcout << "======== end of objective function ========" << std::endl;
#endif
    return ret.coeff(0, 0);
}
