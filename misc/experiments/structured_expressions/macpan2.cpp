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
// Functions we support
enum macpan2_func {
    MP2_ADD = 1,
    MP2_SUBTRACT = 2,
    MP2_MULTIPLY = 3,
    MP2_DIVIDE = 4,
    MP2_POWER = 5,
    MP2_ROUND_BRACKET = 6,
    MP2_COMBINE = 7,
    MP2_MATRIX = 8,
    MP2_MATRIX_MULTIPLY = 9,
    MP2_SUM = 10,
    MP2_REPLICATE = 11,
    MP2_ROWSUMS = 12,
    MP2_COLSUMS = 13,
    MP2_SQUARE_BRACKET = 14,
    MP2_TRANSPOSE = 15
};

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
        const vector<int>& table_x,
        const vector<int>& table_n,
        const vector<int>& table_i,
        const ListOfMatrices<Type>& valid_vars,
        const vector<Type>& valid_literals,
        int row = 0
    )
    {
        matrix<Type> m, m2;
        Type sum, s;
        int rows, cols, rowIndex, colIndex;

        if (GetErrorCode()) return m; // Check if error has already happened at some point of the recursive call.

        switch (table_n[row]) {
            case -1: // literals
                m = matrix<Type>::Zero(1,1);
                m.coeffRef(0,0) = valid_literals[table_x[row]];
                return m;
            case 0: // In current version, there are only scalar variables.
                    // We will need to split the case into 3 cases when vector and matrix variables are introduced.
                //m = matrix<Type>::Zero(1,1);
                //m.coeffRef(0,0) = valid_vars[table_x[row]];
                m = valid_vars.m_matrices[table_x[row]];
                return m;
            default:
                int n = table_n[row];
                vector<matrix<Type> > r(n);
                for (int i=0; i<n; i++) {
                    r[i] = EvalExpr(table_x, table_n, table_i, valid_vars, valid_literals, table_i[row]+i);
                    if (GetErrorCode()) return m;
                }

                // Check dimensions compatibility. If needed, expand one operand to make its dimensions compatible with the other
                if (table_x[row]+1<6 && table_n[row]==2) { // elementwise operations + - * / ^ 
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
                            else
                                SetError(1, "The two operands do not have the same number of columns");
                                //Rf_error("The two operands do not have the same number of columns");

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
                            else
                                SetError(2, "The two operands do not have the same number of rows");
                                // Rf_error("The two operands do not have the same number of rows");
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
                                SetError(3, "The two operands do not have the same number of columns or rows");
                                //Rf_error("The dimensions of the two operands are not equal to each other");
                            }
                        }
                    }
                }
                else if (table_x[row]+1==9) { // %*% matrix multiplication
                    if (r[0].cols()!=r[1].rows())
                        SetError(4, "The two operands are not compatible to do matrix multiplication");
                        //Rf_error("The two operands are not compatible to do matrix multiplication");
                }

                if (GetErrorCode()) return m; // early return

                switch(table_x[row]+1) {
                    case MP2_ADD: // +
                        #ifdef MP_VERBOSE
                            std::cout << r[0] << " + " << r[1] << " = " << r[0]+r[1] << std::endl << std::endl;
                        #endif
                        return r[0]+r[1];
                    case MP2_SUBTRACT: // -
                        #ifdef MP_VERBOSE
                            std::cout << r[0] << " - " << r[1] << " = " << r[0]-r[1] << std::endl << std::endl;
                        #endif
                        if (table_n[row]==1)
                            return -r[0];
                        else
                            return r[0]-r[1];
                    case MP2_MULTIPLY: // *
                        #ifdef MP_VERBOSE
                            std::cout << r[0] << " .* " << r[1] << " = " << r[0].array()*r[1].array() << std::endl << std::endl;
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
                    case MP2_ROUND_BRACKET: // (
                        return r[0];
                    case MP2_COMBINE: // c
                        m = matrix<Type>::Zero(n,1);
                        for (int i=0; i<n; i++)
                            m.coeffRef(i,0) = r[i].coeff(0,0);
                        #ifdef MP_VERBOSE
                            std::cout << "c(" << r[0] << ", ...," << r[n-1] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;
                    case MP2_MATRIX: // matrix
                        m = r[0];

                        rows = CppAD::Integer(r[1].coeff(0,0));
                        cols = CppAD::Integer(r[2].coeff(0,0));

                        m.resize(rows, cols);

                        // m2 = m.transpose(); // m = m.transpose() doesn't work !!!
                        m2 = m;

                        #ifdef MP_VERBOSE
                            std::cout << "matrix(" << r[0] << ") reshaped into [" << rows << ", " << cols << "] = " \
                                      << m2 << std::endl << std::endl;
                        #endif

                        return m2;

                    case MP2_MATRIX_MULTIPLY: // %*%
                        #ifdef MP_VERBOSE
                            std::cout << r[0] << " %*% " << r[1] << " = " << r[0]*r[1] << std::endl << std::endl;
                        #endif
                        return r[0]*r[1];

                    case MP2_SUM: // sum
                        m = matrix<Type>::Zero(1,1);
                        sum = 0.0;
                        for (int i=0; i<n; i++)
                            sum += r[i].sum();
                        m.coeffRef(0,0) = sum;

                        #ifdef MP_VERBOSE
                            std::cout << "sum(" << r[0] << ", ..., " << r[n-1] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;
                    case MP2_REPLICATE: // rep
                        rows = CppAD::Integer(r[1].coeff(0,0));
                        m = matrix<Type>::Constant(rows,1, r[0].coeff(0,0));
                        //for (int i=0; i<rows; i++)
                        //    m.coeffRef(i,0) = r[0].coeff(0,0);

                        #ifdef MP_VERBOSE
                            std::cout << "rep(" << r[0] << ", " << r[1] << ") = " << m << std::endl << std::endl;
                        #endif
                        return m;
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
                    case MP2_SQUARE_BRACKET: // [
                        m = matrix<Type>::Zero(1,1);
                        rowIndex = CppAD::Integer(r[1].coeff(0,0));
                        colIndex = CppAD::Integer(r[2].coeff(0,0));
                        m.coeffRef(0,0) = r[0].coeff(rowIndex, colIndex);
                        return m;
                    case MP2_TRANSPOSE: // t or transpose
                        m = r[0].transpose(); 
                        return m;
                    default:
                        SetError(5, "invalid operator in arithmatic expression");
                        //Rf_error("invalid operator in arithmatic expression");
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

    PARAMETER_VECTOR(params);

    PARAMETER_VECTOR(random);

    // Matrices
    DATA_STRUCT(mats, ListOfMatrices);

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

    DATA_IVECTOR(mats_save_hist);
    DATA_IVECTOR(mats_return);

    // Expressions
    DATA_IVECTOR(eval_schedule)

    DATA_IVECTOR(expr_output_count);
    DATA_IVECTOR(expr_output_id);
    DATA_IVECTOR(expr_sim_block);
    DATA_IVECTOR(expr_num_p_table_rows);

    // Parse Tables
    DATA_IVECTOR(p_table_x);
    DATA_IVECTOR(p_table_n);
    DATA_IVECTOR(p_table_i);

    // Literals
    DATA_VECTOR(literals);

    // Objective function return
    //DATA_IVECTOR(o_table_n)
    //DATA_IVECTOR(o_table_x)
    //DATA_IVECTOR(o_table_i)

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

    std::cout << "expr_output_count = " << expr_output_count << std::endl;
    std::cout << "expr_output_id = " << expr_output_id << std::endl;
    std::cout << "expr_sim_block = " << expr_sim_block << std::endl;
    std::cout << "expr_num_p_table_rows = " << expr_num_p_table_rows << std::endl;

    std::cout << "p_table_x = " << p_table_x << std::endl;
    std::cout << "p_table_n = " << p_table_n << std::endl;
    std::cout << "p_table_i = " << p_table_i << std::endl;
    
    std::cout << "literals = " << literals << std::endl;
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
    vector<ListOfMatrices<Type> > simulation_history(time_steps+2);

    // 3 Pre-simulation
    int expr_index = 0;
    int p_table_row = 0;

    for (int i=0; i<eval_schedule[0]; i++) {
        #ifdef MP_VERBOSE
        std::cout << "in pre-simulation --- " << i << std::endl;
        #endif
        matrix<Type> result;
        if (expr_sim_block[i]==1) {
            SIMULATE {
                result  = exprEvaluator.EvalExpr(
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
        
    simulation_history[0] = mats;

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
            #endif
            matrix<Type> result;
            if (expr_sim_block[i]==1) {
                SIMULATE {
                    result = exprEvaluator.EvalExpr(
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

            p_table_row2 += expr_num_p_table_rows[i];

            #ifdef MP_VERBOSE
            int n = mats.m_matrices.size();
            for (int ii = 0; ii < n; ii++)
                std::cout << "mats = " << mats.m_matrices[ii] << std::endl;
            #endif
        }
        simulation_history[k+1] = mats;
    }
    p_table_row = p_table_row2;

    // 5 Post-simulation
    expr_index += eval_schedule[1];

    for (int i=0; i<eval_schedule[2]; i++) {
        #ifdef MP_VERBOSE
        std::cout << "in post-simulation --- " << i << std::endl;
        #endif
        matrix<Type> result;
        if (expr_sim_block[i]==1) {
            SIMULATE {
                result = exprEvaluator.EvalExpr(
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

    simulation_history[time_steps+1] = mats;

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

    // 6 Report a vector of matrices, each of which is
    //   either the post-simulation matrices or the entire simulation history of matrices
    vector<matrix<Type> > mats_returned(mats_return.sum());

    int r = 0;
    for (int i=0; i<mats_return.size(); i++) {
        if (mats_return[i]==1) {
            if (mats_save_hist[i]==0) { // Report the last one
                mats_returned[r++] = mats.m_matrices[i];
            }
            else { // Report the whole simulation history
                int hist_len = time_steps+2;
                int nRows = mats.m_matrices[i].rows();
                int nCols = mats.m_matrices[i].cols();
                matrix<Type> hist(nRows, hist_len*nCols);
                for (int k=0; k<hist_len; k++)
                    hist.block(0, k*nCols, nRows, nCols) = simulation_history[k].m_matrices[i];
                mats_returned[r++] = hist;
            }
        }
    }

    REPORT(mats_returned)

    // 7 Calc the return of the objective function
    REPORT_ERROR

    return 0.0;

  /*
  DATA_IVECTOR(parse_table_x);
  DATA_IVECTOR(parse_table_n);
  DATA_IVECTOR(parse_table_i);
  DATA_VECTOR(valid_literals);
//  DATA_VECTOR(valid_vars);
  DATA_STRUCT(valid_vars, ListOfMatrices);
  PARAMETER_VECTOR(params);

  // DATA_STRUCT(valid_vars);
  // PARAMETER_VECTOR(params);
  // DATA_IVECTOR(param_var_id);
  // DATA_IVECTOR(param_row_id);
  // DATA_IVECTOR(param_col_id);

  // for (i in 1:length(params)) {
  //   valid_vars[param_var_id[i-1]][param_row_id[i-1], param_col_id[i-1]] = params[i]
  // }



  ExprEvaluator<Type> exprEvaluator;
  matrix<Type> result = exprEvaluator.EvalExpr(
    parse_table_x,
    parse_table_n,
    parse_table_i,
    valid_vars,
    valid_literals
  );

  int error_code = exprEvaluator.GetErrorCode();
  REPORT(error_code);
  REPORT(result);
  if (error_code)
      return 0.0;
  else
      return result.sum();
  */
}
