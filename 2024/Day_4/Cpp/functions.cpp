#include "functions.hpp"
#include <iostream>
using namespace std;

// -----------------------------------------------

std::vector<std::string> load_file_to_matrix(std::ifstream &input_file, const std::string &filename){

	std::vector<std::string> file_mat;
	std::string file_line;

	// --- --- ---

	// Checking if file is open
	/*if( !input_file.is_open() ){
		std::cerr << "Error: Could not open file "+filename << endl;
		return file_mat;
	}*/

	// Reading file
	while( std::getline(input_file, file_line) ){
		file_mat.push_back(file_line);
	}

	return file_mat;

}

// -----------------------------------------------

std::vector<Element> find_char_locations(const std::vector<std::string> &file_mat, const int &n_rows, const int &n_cols, const char &target_c, int &n_char){

	std::vector<Element> char_elements;

	// --- --- ---

	for(int i_row = 0; i_row < n_rows; ++i_row){ // Row-major order; ideal for C++
		for(int i_col = 0; i_col < n_cols; ++i_col){

			if( file_mat[i_row][i_col] == target_c ){ // Ticks are used because X is one character

				char_elements.push_back({i_row, i_col});

			}

		}
	}

	n_char = char_elements.size(); // Number of found X characters in the matrix

	return char_elements;

}

// -----------------------------------------------

bool check_MAS(std::ofstream &debug_file, const std::vector<std::string> &file_mat, const int &n_rows, const int &n_cols, const Element &start_X, int &d_row, int &d_col, std::vector<std::string> &virtual_mat, int &n_xmas){

	std::string target = "MAS"; // String we want to check for after X, in a given (d_row, d_col) direction
	int row_check, col_check, target_length;

	// --- --- ---

	target_length = 3; // Length of "MAS"; Avoids having to do target.length() for a fixed 3 letter word 

	debug_file << ">> Direction check: d_row = " << d_row << ", d_col = " << d_col << endl;

	for(int i_letter = 0; i_letter < target_length; ++i_letter){ // i_letter = 0 <> 'M'; i_letter = 1 <> 'A'; i_letter = 2 <> 'S'

		row_check = start_X.row + d_row*(i_letter+1); // Next row from start_X.row in one step of d_row direction
		col_check = start_X.col + d_col*(i_letter+1); // Next col from start_X.col in one step of d_col direction

		debug_file << ">> Next row is therefore: row = " << row_check << ", col = " << col_check << endl;

		// Checking boundary conditions

		// Row boundaries: row_check < 0 <> Top wall ; row_check > n_rows-1 <> Bottom wall
		if( row_check < 0 || row_check > n_rows-1 ){

			debug_file << ">> Top / Bottom wall has been hit; XMAS cannot be formed" << endl;
			return false; // Wall was hit; No XMAS can be formed

		}

		// Column boundaries: col_check < 0 <> Left wall ; col_check > n_cols-1 <> Right wall
		if( col_check < 0 || col_check > n_cols-1 ){

			debug_file << ">> Left / Right wall has been hit; XMAS cannot be formed" << endl;
			return false; // Wall was hit; No XMAS can be formed

		}

		// Letter check, in order, which is dictated by iterative i_letter; We are checking if letter is NOT the one we want
		if( file_mat[row_check][col_check] != target[i_letter] ){

			debug_file << ">> Letter was supposed to be " << target[i_letter] << ". Instead, it's " << file_mat[row_check][col_check] << ". XMAS cannot be formed" << endl;
			return false; // Letter was not the appropriate one; No XMAS can be formed

		}

		// If it survives all three if cases, then we hit i_letter target character
		debug_file << ">> Letter is " << file_mat[row_check][col_check] << " | Target is " << target[i_letter] << endl;

	}

	n_xmas = n_xmas + 1; // XMAS count is updated
	debug_file << ">> XMAS has been formed! Update cumulative XMAS count = " << n_xmas << endl;

	// Visualizing found XMAS, through virtual_mat
	virtual_mat[start_X.row + d_row][start_X.col + d_col] = 'M';
	virtual_mat[start_X.row + 2*d_row][start_X.col + 2*d_col] = 'A';
	virtual_mat[start_X.row + 3*d_row][start_X.col + 3*d_col] = 'S';

	debug_file << ">> Visualizing in current matrix:" << endl;
	for( const std::string &row : virtual_mat ){
		debug_file << row << endl;
	}
	debug_file << endl;

	return true; // Only happens if it survives the loop, which means a XMAS was formed

}

// -----------------------------------------------

int count_total_xmas(std::ofstream &debug_file, const std::vector<std::string> &file_mat, const std::vector<Element> &X_elements, const int &n_rows, const int &n_cols, std::vector<std::string> &virtual_mat){

	int n_xmas;

	// --- --- ---

	n_xmas = 0; // Initialize number of XMAS found

	/*
	Possible directions from X (consider X is d_row = 0 and d_col = 0):
		123
		8X4
		765
	1 :: Going top-left  	--> d_row = -1, d_col = -1;
	2 :: Going top 		 	--> d_row = -1, d_col = 0;
	3 :: Going top-right 	--> d_row = -1, d_col = 1;
	4 :: Going right 		--> d_row = 0, d_col = 1;
	5 :: Going bottom-right --> d_row = 1, d_col = 1;
	6 :: Going bottom 		--> d_row = 1, d_col = 0;
	7 :: Going bottom-left 	--> d_row = 1, d_col = -1;
	8 :: Going left 		--> d_row = 0, d_col = -1;
	*/

	for(const Element &start_X : X_elements){ // Go over each X on the matrix

		debug_file << "Going to X location = (" << start_X.row << ", " << start_X.col << ")" << endl;

		for(int d_row = -1; d_row < 2; ++d_row){ // d_row = -1 <> Go up a row; d_row = 0 <> Stay in the same row; d_row = 1 <> Go down a row
			for(int d_col = -1; d_col < 2; ++d_col){ // d_col = -1 <> Go up a col; d_col = 0 <> Stay in the same col; d_col = 1 <> Go down a col

				if( d_row == 0 && d_col == 0 ){ continue; } // That's the X spot

				check_MAS(debug_file, file_mat, n_rows, n_cols, start_X, d_row, d_col, virtual_mat, n_xmas);

			}
		}

		debug_file << endl;

	}

	return n_xmas;

}

// -----------------------------------------------

int count_total_x_shaped_mas(std::ofstream &debug_file, const std::vector<std::string> &file_mat, const std::vector<Element> &A_elements, const int &n_rows, const int &n_cols, std::vector<std::string> &virtual_mat){

	int n_x_shaped_mas, up_row_check, down_row_check, left_col_check, right_col_check;
	bool x_shaped_mas_found;

	// --- --- ---

	n_x_shaped_mas = 0; // Initialize number of X-shaped MAS found

	/*
	Possible X shaped MAS (consider A is d_row = 0 and d_col = 0):
		1) M.M		2) M.S		3) S.M		4) S.S
		   .A.		   .A.		   .A.		   .A.
		   S.S		   M.S		   S.M		   M.M
	1 :: M at d_row = -1 + (d_col = -1 && d_col = 1) | S at d_row = 1 + (d_col = -1 && d_col = 1)
	2 :: M at (d_row = -1 && d_row = 1) + d_col = -1 | S at (d_row = -1 && d_row = 1) + d_col = 1
	3 :: M at (d_row = -1 && d_row = 1) + d_col = 1  | S at (d_row = -1 && d_row = 1) + d_col = -1
	4 :: M at d_row = 1 + (d_col = -1 && d_col = 1)  | S at d_row = -1 + (d_col = -1 && d_col = 1)
	*/

	for(const Element &start_A : A_elements){ // Go over each X on the matrix

		debug_file << "Going to A location = (" << start_A.row << ", " << start_A.col << ")" << endl;

		// X-shaped row and column positions
		up_row_check = start_A.row - 1;
		down_row_check = start_A.row + 1;
		left_col_check = start_A.col - 1;
		right_col_check = start_A.col + 1;

		debug_file << ">> X-shape positions to check: Up row = " << up_row_check << " | Down row = " << down_row_check << " | Left col = " << left_col_check << " | Right col = " << right_col_check << endl;

		// --- --- ---

		x_shaped_mas_found = false; // Initialize if x-shaped mas was found to false. For virtual_mat visualization

		if( up_row_check < 0 || down_row_check > n_rows-1 ){ // Row hits boundary

			debug_file << ">> Row hits boundary. No X-shaped MAS can be formed" << endl;
			debug_file << endl;
			continue; // Go to the next A location

		} else if( left_col_check < 0 || right_col_check > n_cols-1 ){ // Column hits boundary

			debug_file << ">> Column hits boundary. No X-shaped MAS can be formed" << endl;
			debug_file << endl;
			continue; // Go to the next A location

		} else { // No boundaries are hit

			// --- --- ---

			if( file_mat[up_row_check][left_col_check] == 'M' ){ // Checking if top-left is an M

				debug_file << ">> Top-left is M" << endl;

				if( file_mat[down_row_check][right_col_check] == 'S' ){ // If top-left is M, need to assure down-right is S

					debug_file << ">> Bottom-right is S" << endl;

					// Only Case 1 or 2 are possible now

					if( file_mat[up_row_check][right_col_check] == 'M' && file_mat[down_row_check][left_col_check] == 'S' ){ // Case 1 check

						n_x_shaped_mas = n_x_shaped_mas + 1;
						debug_file << ">> Forms Case 1 X-shaped MAS. Updating count = " << n_x_shaped_mas << endl;
						debug_file << endl;
						
						x_shaped_mas_found = true;

						virtual_mat[start_A.row][start_A.col] = 'A';
						virtual_mat[up_row_check][left_col_check] = 'M';
						virtual_mat[up_row_check][right_col_check] = 'M';
						virtual_mat[down_row_check][left_col_check] = 'S';
						virtual_mat[down_row_check][right_col_check] = 'S';

					} else if( file_mat[up_row_check][right_col_check] == 'S' && file_mat[down_row_check][left_col_check] == 'M' ){ // Case 2 check

						n_x_shaped_mas = n_x_shaped_mas + 1;
						debug_file << ">> Forms Case 2 X-shaped MAS. Updating count = " << n_x_shaped_mas << endl;
						debug_file << endl;

						x_shaped_mas_found = true;

						virtual_mat[start_A.row][start_A.col] = 'A';
						virtual_mat[up_row_check][left_col_check] = 'M';
						virtual_mat[up_row_check][right_col_check] = 'S';
						virtual_mat[down_row_check][left_col_check] = 'M';
						virtual_mat[down_row_check][right_col_check] = 'S';

					} else { // Fails both case checks

						debug_file << ">> Fails both Case 1 and Case 2: Top-right char = " << file_mat[up_row_check][right_col_check] << " | Bottom-left char = " << file_mat[down_row_check][left_col_check] << ". No X-shaped MAS can be formed" << endl;
						debug_file << endl;
						continue; // Go to the next A location

					}

					// --- --- ---

				} else {

					debug_file << ">> Top-left is M but bottom-right is not S, it's " << file_mat[down_row_check][right_col_check] << " instead. No X-shaped MAS can be formed" << endl;
					debug_file << endl;
					continue; // Go to the next A location

				}

				// --- --- ---

			} else if( file_mat[up_row_check][left_col_check] == 'S' ){ // Checking if top-left is an S

				debug_file << ">> Top-left is S" << endl;

				if( file_mat[down_row_check][right_col_check] == 'M' ){ // If top-left is S, need to assure down-right is M

					debug_file << ">> Bottom-right is M" << endl;

					// Only Case 3 or 4 are possible now

					if( file_mat[up_row_check][right_col_check] == 'M' && file_mat[down_row_check][left_col_check] == 'S' ){ // Case 3 check

						n_x_shaped_mas = n_x_shaped_mas + 1;
						debug_file << ">> Forms Case 3 X-shaped MAS. Updating count = " << n_x_shaped_mas << endl;
						debug_file << endl;

						x_shaped_mas_found = true;

						virtual_mat[start_A.row][start_A.col] = 'A';
						virtual_mat[up_row_check][left_col_check] = 'S';
						virtual_mat[up_row_check][right_col_check] = 'M';
						virtual_mat[down_row_check][left_col_check] = 'S';
						virtual_mat[down_row_check][right_col_check] = 'M';

					} else if( file_mat[up_row_check][right_col_check] == 'S' && file_mat[down_row_check][left_col_check] == 'M' ){ // Case 4 check

						n_x_shaped_mas = n_x_shaped_mas + 1;
						debug_file << ">> Forms Case 4 X-shaped MAS. Updating count = " << n_x_shaped_mas << endl;
						debug_file << endl;

						x_shaped_mas_found = true;

						virtual_mat[start_A.row][start_A.col] = 'A';
						virtual_mat[up_row_check][left_col_check] = 'S';
						virtual_mat[up_row_check][right_col_check] = 'S';
						virtual_mat[down_row_check][left_col_check] = 'M';
						virtual_mat[down_row_check][right_col_check] = 'M';

					} else { // Fails both case checks

						debug_file << ">> Fails both Case 3 and Case 4: Top-right char = " << file_mat[up_row_check][right_col_check] << " | Bottom-left char = " << file_mat[down_row_check][left_col_check] << ". No X-shaped MAS can be formed" << endl;
						debug_file << endl;
						continue; // Go to the next A location

					}

					// --- --- ---

				} else {

					debug_file << ">> Top-left is S but bottom-right is not M, it's " << file_mat[down_row_check][right_col_check] << " instead. No X-shaped MAS can be formed" << endl;
					debug_file << endl;
					continue; // Go to the next A location

				}

				// --- --- ---

			} else {

				debug_file << ">> Top-left is neither M nor S, but " << file_mat[up_row_check][left_col_check] << " instead. No X-shaped MAS can be formed" << endl;
				debug_file << endl;
				continue; // Go to the next A location

			}

			// --- --- ---

		}

		// --- --- ---

		// Visualization
		if(x_shaped_mas_found){

			debug_file << ">> Visualizing in current matrix:" << endl;
			for( const std::string &row : virtual_mat ){
				debug_file << row << endl;
			}
			debug_file << endl;

		}

		// --- --- ---

	}

	return n_x_shaped_mas;

}

// -----------------------------------------------

void finding_xmas(std::ofstream &debug_file, const std::string &filepath, const std::string &filename){

	int n_rows, n_cols, n_X, n_xmas;

	std::ifstream input_file;
	std::vector<std::string> file_mat, virtual_mat;
	std::vector<Element> X_elements;

	// --- --- ---

	//std::cout << "Hello World!" << endl;
	//std::cout << "The filename of the input file is "+filename << endl;

	// --- --- ---

	// Opening input file
	input_file.open(filepath + filename);
	file_mat = load_file_to_matrix(input_file, filename);

	// Getting number of rows and of matrices 
	n_rows = file_mat.size();
	n_cols = file_mat[0].length();

	// DEBUG
	std::cout << "File has " << n_rows << " rows and " << n_cols << " columns" << endl;
	debug_file << "File has " << n_rows << " rows and " << n_cols << " columns" << endl;
	debug_file << endl;
	debug_file << "Reproducing file content:" << endl;
	for( const std::string &row_string : file_mat ){
		debug_file << row_string << endl;
	}
	debug_file << endl;

	// --- --- ---

	// Finding all X letters in the matrix
	X_elements = find_char_locations(file_mat, n_rows, n_cols, 'X', n_X); // n_X is output too

	// DEBUG | Also, intializing virtual_mat, as a 2D matrix with only the X and the rest as dots
	std::cout << "In total, " << n_X << " X's have been found" << endl;
	debug_file << "In total, " << n_X << " X's have been found" << endl;

	virtual_mat.assign(n_rows, std::string(n_cols, '.'));
	for( const Element &X_el : X_elements ){
		virtual_mat[X_el.row][X_el.col] = 'X';
	}

	debug_file << "Reproducing file content, with only the X letters:" << endl;
	for( const std::string &row_string : file_mat ){
		for( char c : row_string ){
			debug_file << (c == 'X' ? 'X' : '.'); // (If [Condition] ? means then [Output if True] : [Output if False])
		}
		debug_file << endl;
	}
	debug_file << endl;

	// --- --- ---

	// Go to each X location, find XMAS, and count all of the XMAS found
	n_xmas = count_total_xmas(debug_file, file_mat, X_elements, n_rows, n_cols, virtual_mat);

	// DEBUG
	std::cout << "In total, " << n_xmas << " XMAS have been found" << endl;
	debug_file << "In total, " << n_xmas << " XMAS have been found" << endl;
	debug_file << endl;

	// --- --- ---

}

// -----------------------------------------------

void finding_x_shaped_mas(std::ofstream &debug_file, const std::string &filepath, const std::string &filename){

	int n_rows, n_cols, n_A, n_x_shaped_mas;

	std::ifstream input_file;
	std::vector<std::string> file_mat, virtual_mat;
	std::vector<Element> A_elements;

	// --- --- ---

	// Opening input file
	input_file.open(filepath + filename);
	file_mat = load_file_to_matrix(input_file, filename);

	// Getting number of rows and of matrices 
	n_rows = file_mat.size();
	n_cols = file_mat[0].length();

	// DEBUG
	std::cout << "File has " << n_rows << " rows and " << n_cols << " columns" << endl;
	debug_file << "File has " << n_rows << " rows and " << n_cols << " columns" << endl;
	debug_file << endl;
	debug_file << "Reproducing file content:" << endl;
	for( const std::string &row_string : file_mat ){
		debug_file << row_string << endl;
	}
	debug_file << endl;

	// --- --- ---

	// Finding all A letters in the matrix
	A_elements = find_char_locations(file_mat, n_rows, n_cols, 'A', n_A); // n_A is output too

	// DEBUG | Also, intializing virtual_mat, as a 2D matrix with only the A and the rest as dots
	std::cout << "In total, " << n_A << " A's have been found" << endl;
	debug_file << "In total, " << n_A << " A's have been found" << endl;

	virtual_mat.assign(n_rows, std::string(n_cols, '.'));

	debug_file << "Reproducing file content, with only the A letters:" << endl;
	for( const std::string &row_string : file_mat ){
		for( char c : row_string ){
			debug_file << (c == 'A' ? 'A' : '.'); // (If [Condition] ? means then [Output if True] : [Output if False])
		}
		debug_file << endl;
	}
	debug_file << endl;

	// --- --- ---

	// Go to each A location, find X shapped MAS, and count all of the X shapped MAS found
	n_x_shaped_mas = count_total_x_shaped_mas(debug_file, file_mat, A_elements, n_rows, n_cols, virtual_mat);

	// DEBUG
	std::cout << "In total, " << n_x_shaped_mas << " X shaped MAS have been found" << endl;
	debug_file << "In total, " << n_x_shaped_mas << " X shaped MAS have been found" << endl;
	debug_file << endl;

	// --- --- ---

}
