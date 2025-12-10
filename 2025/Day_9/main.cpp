#include<iostream>
#include<fstream>
#include<string>
#include "functions.hpp"
using namespace std;

// -------------------------------------------------

int main(){

	char file_choice;
	std::ofstream debug_file;

	// --- --- ---

	// Filepath of all files in this code
	std::string filepath = "";

	// Opening debug file
	std::string filename = "tiles_debug.txt";
	debug_file.open(filepath + filename);

	// Reading input file | The user choses which input file they want
	std::cout << "Select which file to use as input" << endl;
	std::cout << "[e] Example input file :: 2025_day9_input_example.txt" << endl;
	std::cout << "[r] Regular input file :: 2025_day9_input.txt" << endl;
	std::cout << "Enter choice (e/r): ";

	std::cin >> file_choice;

	if( file_choice == 'r' || file_choice == 'R' ){

		filename = "2025_day9_input.txt";
		std::cout << "Regular input file has been chosen" << endl;

	} else if( file_choice == 'e' || file_choice == 'E' ){
		
		filename = "2025_day9_input_example.txt";
		std::cout << "Example input file has been chosen" << endl;

	} else{

		std::cout << "Choice is not valid. Please try again" << endl;
		return 0; // Early return

	}

	main_function(debug_file, filepath, filename);

	return 0;
	
}
