#ifndef functions
#define functions

#include<fstream>
#include<string>
#include<vector>

// -------------------------------------------------

struct Element{ // Element of a 2D matrix, give by its row and col

	int row; // Array index, from 0 to N-1, for an NxN matrix | { row, ...
	int col; // Array index, from 0 to N-1, for an NxN matrix | ... col }

	// No need for a constructor; struct is simple and so it will understand {. , .}

};

// -------------------------------------------------

// Core function for XMAS search
void finding_xmas(std::ofstream &debug_file, const std::string &filepath, const std::string &filename);

// Core function X shaped MAS search
void finding_x_shaped_mas(std::ofstream &debug_file, const std::string &filepath, const std::string &filename);

// Reads file content and stores it into a matrix
std::vector<std::string> load_file_to_matrix(std::ifstream &input_file, const std::string &filename);

// Finds all X characters in a 2D string matrix
std::vector<Element> find_char_locations(const std::vector<std::string> &file_mat, const int &n_rows, const int &n_cols, const char &target_c, int &n_char);

// Checking a direction (d_row, d_col) of a matrix element start_X, which contains the character 'X', for the string "MAS", to see if "XMAS" is formed. Returns true if it is
bool check_MAS(std::ofstream &debug_file, const std::vector<std::string> &file_mat, const int &n_rows, const int &n_cols, const Element &start_X, int &d_row, int &d_col, std::vector<std::string> &virtual_mat, int &n_xmas);

// Go over all X locations and find how many XMAS exist
int count_total_xmas(std::ofstream &debug_file, const std::vector<std::string> &file_mat, const std::vector<Element> &X_elements, const int &n_rows, const int &n_cols, std::vector<std::string> &virtual_mat);

// Go over all A locations and find how many X-shaped MAS exist
int count_total_x_shaped_mas(std::ofstream &debug_file, const std::vector<std::string> &file_mat, const std::vector<Element> &A_elements, const int &n_rows, const int &n_cols, std::vector<std::string> &virtual_mat);

#endif
