#include<iostream>
#include<fstream>
#include<string>
#include "functions.hpp"
using namespace std;

// -------------------------------------------------

int main(){

	int user_choice;
	char file_choice;

	std::string filepath, filename;
	std::ofstream debug_file;

	// --- --- ---

	// Filepath of all files in this code
	filepath = "";

	// Chosing which word search to tackle | The user choses
	std::cout << "Select which word search to do" << endl;
	std::cout << "[1] Word search for XMAS" << endl;
	std::cout << "[2] Search fos MAS in X-shapes" << endl;
	std::cout << "Enter choice (1/2): ";

	std::cin >> user_choice;

	if( user_choice == 1 ){

		std::cout << "XMAS word search selected" << endl;
		std::cout << endl;

		// Opening debug file
		filename = "XMAS_search_debug.txt";
		debug_file.open(filepath + filename);

		// Reading input file | The user choses which input file they want
		std::cout << "Select which file to use as input" << endl;
		std::cout << "[e] Example input file :: 2024_day4_input_example.txt" << endl;
		std::cout << "[r] Regular input file :: 2024_day4_input.txt" << endl;
		std::cout << "Enter choice (e/r): ";

		std::cin >> file_choice;

		if( file_choice == 'r' || file_choice == 'R' ){

			filename = "2024_day4_input.txt";
			std::cout << "Regular input file has been chosen" << endl;

		} else if( file_choice == 'e' || file_choice == 'E' ){
			
			filename = "2024_day4_input_example.txt";
			std::cout << "Example input file has been chosen" << endl;

		} else{

			std::cout << "Choice is not valid. Please try again" << endl;
			return 0; // Early return

		}

		finding_xmas(debug_file, filepath, filename);

	} else if( user_choice == 2 ){

		std::cout << "X-shaped MAS search selected" << endl;
		std::cout << endl;

		// Opening debug file
		filename = "X_shaped_MAS_search_debug.txt";
		debug_file.open(filepath + filename);

		// Reading input file | The user choses which input file they want
		std::cout << "Select which file to use as input" << endl;
		std::cout << "[e] Example input file :: 2024_day4_input_example.txt" << endl;
		std::cout << "[r] Regular input file :: 2024_day4_input.txt" << endl;
		std::cout << "Enter choice (e/r): ";

		std::cin >> file_choice;

		if( file_choice == 'r' || file_choice == 'R' ){

			filename = "2024_day4_input.txt";
			std::cout << "Regular input file has been chosen" << endl;

		} else if( file_choice == 'e' || file_choice == 'E' ){
			
			filename = "2024_day4_input_example.txt";
			std::cout << "Example input file has been chosen" << endl;

		} else{

			std::cout << "Choice is not valid. Please try again" << endl;
			return 0; // Early return

		}

		finding_x_shaped_mas(debug_file, filepath, filename);

	} else{

		std::cout << "Word search choice not valid. Please try again" << endl;
		return 0; // Early return

	}

	return 0; // Successful return
}
