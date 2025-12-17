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
	std::string filename = "IDs_debug.txt";
	debug_file.open(filepath + filename);

	// Reading input file | The user choses which input file they want
	std::cout << "Select which file to use as input" << endl;
	std::cout << "[e] Example input file :: 2025_day2_input_example.txt" << endl;
	std::cout << "[r] Regular input file :: 2025_day2_input.txt" << endl;
	std::cout << "Enter choice (e/r): ";

	std::cin >> file_choice;

	if( file_choice == 'r' || file_choice == 'R' ){

		filename = "2025_day2_input.txt";
		std::cout << "Regular input file has been chosen" << endl;

	} else if( file_choice == 'e' || file_choice == 'E' ){
		
		filename = "2025_day2_input_example.txt";
		std::cout << "Example input file has been chosen" << endl;

	} else{

		std::cout << "Choice is not valid. Please try again" << endl;
		return 0; // Early return

	}

	int user_choice;

	// Reading input file | The user choses which input file they want
	std::cout << "Select which invalid IDs to look for" << endl;
	std::cout << "[1] Regular :: Even-digit IDs broken into two equal halves" << endl;
	std::cout << "[2] Special :: Any-digit IDs broken into any numbered sequence of the same number" << endl;
	std::cout << "Enter choice (1/2): ";

	std::cin >> user_choice;

	if( user_choice == 1 ){

		std::cout << "Regular invalid IDs were chosen" << endl;
		regular_invalid_IDs(debug_file, filepath, filename);

	} else if( user_choice == 2 ){
		
		std::cout << "Special invalid IDs were chosen" << endl;
		special_invalid_IDs(debug_file, filepath, filename);

	} else{

		std::cout << "Choice of invalid ID search is not valid. Please try again" << endl;
		return 0; // Early return

	}

	return 0;
	
}
