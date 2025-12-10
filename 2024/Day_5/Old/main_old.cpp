#include<iostream>
#include<fstream>
#include<string>
#include "functions_old.hpp"
using namespace std;

// -------------------------------------------------

int main(){

	/*
	We have two things:
	- Page ordering rules: X|Y -> if X and Y are to be printed, X must come before Y
	- Pages to be produced by each update
	We have to:
	- Find the updates in the correct order
	- Obtain the middle page numbers from those updates and sum them
	*/

	char file_choice;

	std::string filepath, filename;
	std::ofstream debug_file;

	// --- --- ---

	// Filepath of all files in this code
	filepath = "";

	// Opening debug file
	filename = "pages_debug.txt";
	debug_file.open(filepath + filename);

	// Reading input file | The user choses which input file they want
	std::cout << "Select which file to use as input" << endl;
	std::cout << "[e] Example input file :: 2024_day5_input_example.txt" << endl;
	std::cout << "[r] Regular input file :: 2024_day5_input.txt" << endl;
	std::cout << "Enter choice (e/r): ";

	std::cin >> file_choice;

	if( file_choice == 'r' || file_choice == 'R' ){

		filename = "2024_day5_input.txt";
		std::cout << "Regular input file has been chosen" << endl;

	} else if( file_choice == 'e' || file_choice == 'E' ){
		
		filename = "2024_day5_input_example.txt";
		std::cout << "Example input file has been chosen" << endl;

	} else{

		std::cout << "Choice is not valid. Please try again" << endl;
		return 0; // Early return

	}

	summ_mid_pages_correct_updates(debug_file, filepath, filename);

	return 0;
	
}
