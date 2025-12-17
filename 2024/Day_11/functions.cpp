#include "functions.hpp"
#include <iostream>
#include <sstream>
#include <cmath> // For trunc, log10 and pow
using namespace std;

// -----------------------------------------------

std::vector<long long> get_stones(std::ifstream &input_file){

	std::vector<long long> stones;
	std::string line, line_seg;

	std::getline(input_file, line);

	std::stringstream line_stream(line);

	while( std::getline(line_stream, line_seg, ' ') ){
		stones.push_back( std::stoi(line_seg) );
	}

	return stones;

}

// -----------------------------------------------

long evaluate_stones(std::ofstream &debug_file, std::vector<long long> &stones){

	long n_stones;

	std::vector<long long> new_stones;

	for(const long long &stone : stones){

		debug_file << "\t" << "Evaluating stone " << stone << endl;

		// Rule 1) If 0 --> Replace with 1
		if(stone == 0){

			debug_file << "\t" << "Stone is 0 | Replace it with 1 | New stone arrangement in progress: ";

			new_stones.push_back(1); // Replacing stone with 1

			for(size_t i = 0; i < new_stones.size(); ++i){
				debug_file << new_stones[i] << " ";
			}
			debug_file << endl;

			continue; // Move on to the next stone
		}
		
		// Rule 2) The even digits rule

		int stone_digits = std::trunc(std::log10(stone))+1;
		int stone_midpoint = stone_digits/2;

		if(stone_digits%2 == 0){ // If number of digits of stone is even

			debug_file << "\t" << "Stone has an even number of digits = " << stone_digits << endl;
			debug_file << "\t" << "The midway digit is " << stone_midpoint << endl;
		
			std::string stone_str = std::to_string(stone); // Convert stone into a string

			//std::cout << stone_digits << " " << stone_midpoint << endl;
			//std::cout << stone_str << endl;
			//std::cout << stone_str.substr(0, stone_midpoint) << endl;
			//std::cout << stone_str.substr(stone_midpoint, stone_digits) << endl;

			long long stone_left_half = std::stoll( stone_str.substr(0, stone_midpoint) );
			long long stone_right_half = std::stoll( stone_str.substr(stone_midpoint, stone_digits) );

			debug_file << "\t" << "Left half of stone is " << stone_left_half << endl;
			debug_file << "\t" << "Right half of stone is " << stone_right_half << endl;
			debug_file << "\t" << "New stone arrangement in progress: ";

			new_stones.push_back(stone_left_half);
			new_stones.push_back(stone_right_half);

			for(size_t i = 0; i < new_stones.size(); ++i){
				debug_file << new_stones[i] << " ";
			}
			debug_file << endl;

			continue; // Move on to the next stone

		}

		// Rule 3) Stone multiplied by 2024

		debug_file << "\t" << "Stone is neither 0 nor has an even number of digits (" << stone_digits << ")" << endl;
		debug_file << "\t" << "New stone arrangement in progress: ";

		new_stones.push_back(stone*2024); // Replacing stone with 1

		for(size_t i = 0; i < new_stones.size(); ++i){
			debug_file << new_stones[i] << " ";
		}
		debug_file << endl;

		debug_file << endl;

	}

	// At the end, replace stones with new_stones
	stones = new_stones;
	n_stones = stones.size();

	return n_stones;

}

// -----------------------------------------------

void add_stone(std::vector<long long> &unique_stones, std::vector<long long> &stone_count, long long stone, long long count_to_add){

	// This function searches unique_stones for stone
	// If stone already exists, it adds count_to_add to stone_count for that stone
	// If stone doesn't exist yet, it push_back both stone and count_to_add to the respective vectors

	for(size_t i = 0; i < unique_stones.size(); ++i){

		if(unique_stones[i] == stone){
			stone_count[i] = stone_count[i] + count_to_add;
			return; // No need to continue searching, we can just leave the function
		}

	}

	// We only get to this point, outside the for loop, if stone was not found inside unique_stones
	unique_stones.push_back(stone);
	stone_count.push_back(count_to_add);

}

// -----------------------------------------------

void optimized_evaluate_stones(std::ofstream &debug_file, std::vector<long long> &unique_stones, std::vector<long long> &stone_count){

	std::vector<long long> next_stones;
	std::vector<long long> next_count;

	long long stone_left_half, stone_right_half, stone_digits, stone_midpoint;

	for(size_t i_stone = 0; i_stone < unique_stones.size(); ++i_stone){

		long long stone = unique_stones[i_stone];
		long long stone_amount = stone_count[i_stone];

		//debug_file << "\t" << "Evaluating stone " << stone << endl;

		// Rule 1) If 0 --> Replace with 1
		if(stone == 0){

			//debug_file << "\t" << "Stone is 0" << endl;

			/*
			We replace the 0 stone by a 1 stone by doing the following
			We are starting from scratch with a vector that will replace unique_stones at the end of the i_stone loop: the next_stones vector
			All 0 stones will undergo this transformation
			So, to next_count, we add 1 as a stone number and the quantity of 0 stones will now be the quantity of 1 stones
			If 1 wasn't in next_stones yet, it now is
			If 1 was already in next_stones, its count is updated
			*/
			add_stone(next_stones, next_count, 1, stone_amount);

			//debug_file << "\t" << "Stone is transformed into 1" << endl;

			
			/*debug_file << "\t" << "Current (stone | count) arranjement: ";
			for(size_t i_stone = 0; i_stone < next_stones.size(); ++i_stone){
				debug_file << next_stones[i_stone] << " | " << next_count[i_stone] << " ; ";
			}
			debug_file << endl;
			debug_file << endl;*/

			continue; // Move on to the next stone
		}

		// Rule 2) Even digits - work in progress
		stone_digits = std::trunc(std::log10(stone))+1;
		stone_midpoint = stone_digits/2;

		if(stone_digits%2 == 0){
			
			//debug_file << "\t" << "Stone has even digits" << endl;

			/*
			We replace the 0 stone by a 1 stone by doing the following
			We are starting from scratch with a vector that will replace unique_stones at the end of the i_stone loop: the next_stones vector
			All 0 stones will undergo this transformation
			So, to next_count, we add 1 as a stone number and the quantity of 0 stones will now be the quantity of 1 stones
			If 1 wasn't in next_stones yet, it now is
			If 1 was already in next_stones, its count is updated
			*/
			
			long long divisor = lround(std::pow(10, stone_midpoint));

			stone_left_half = stone/divisor;
			stone_right_half = stone%divisor;

			//debug_file << "\t" << "Stone is split into " << stone_left_half << " and " << stone_right_half << endl;

			add_stone(next_stones, next_count, stone_left_half, stone_amount);
			add_stone(next_stones, next_count, stone_right_half, stone_amount);

			/*debug_file << "\t" << "Current (stone | count) arranjement: ";
			for(size_t i_stone = 0; i_stone < next_stones.size(); ++i_stone){
				debug_file << next_stones[i_stone] << " | " << next_count[i_stone] << " ; ";
			}
			debug_file << endl;
			debug_file << endl;*/

			continue; // Move on to the next stone

		}

		// Rule 3) Stone multiplied by 2024

		//debug_file << "\t" << "Stone is neither 0 nor has even digits" << endl;

		/*
		Similar to rule 1 but instead of replacing 0 with 1, we replace stone with stone*2024
		*/
		add_stone(next_stones, next_count, stone*2024, stone_amount);

		/*debug_file << "\t" << "Current (stone | count) arranjement: ";
		for(size_t i_stone = 0; i_stone < next_stones.size(); ++i_stone){
			debug_file << next_stones[i_stone] << " | " << next_count[i_stone] << " ; ";
		}
		debug_file << endl;
		debug_file << endl;*/

		
	}

	// Update unique_stones and stone_count to be the next versions we were working on
	unique_stones = next_stones;
	stone_count = next_count;

}

// -----------------------------------------------

void number_of_stones(std::ofstream &debug_file, const std::string &filepath, const std::string &filename){

	// Opening input file
	std::ifstream input_file;
	input_file.open(filepath + filename);

	/*
	Summary of task:
	- Stones arranges in a straight line. Think of it as a vector, one is each entry of the vector
	- Rules the stones' numbers follow, in priority order:

		1) If 0 --> Replace with 1

		2) If number of digits is even --> Stone splits into two: a left stone and a right stone
		We will consider the spliting like this:
			(*) Left stone stays in the vector entry of original stone
			(*) Right stone moves to the entry + 1 spot, forcing all other stones after it to move +1 entries
		Left stone retains the first half of the number
		Right stone retains the second half of the number:
			(*) Regarding this half, it's possible it has leading zeros
			(*) Leading zeros are ignored, up until the first non-zero digit
			(*) If second half is all zeros, then that means that the right stone's number is 0
		
		3) If none of the above rules apply, replace number by itself times 2024

	- These rules are applied to the stone's vector each time a blink happens - a loop iteration that goes over the vector essentially
	- Number of iterations: 25
	- File structure: One line, with integers, separated by spaces. The integers are the stones numbers
	*/

	int user_choice;

	// Reading input file | The user choses which input file they want
	std::cout << "Select which method to use" << endl;
	std::cout << "[1] Not optimized: storing all stones" << endl;
	std::cout << "[2] Optimized" << endl;
	std::cout << "Enter choice (1/2): ";

	std::cin >> user_choice;

	if( user_choice == 1 ){ // Not optimized

		// First, let's read the file, and store the first stones iteration in a vector
		// This vector will be modified according to the stone rules, per iteration

		std::vector<long long> stones = get_stones(input_file);

		debug_file << "Stones initial arrangement" << endl;
		for(const long long &stone : stones){
			debug_file << stone << " ";
		}
		debug_file << endl;
		debug_file << endl;

		int n_blinks = 25;
		long n_stones;

		for(int i_blink = 0; i_blink < n_blinks; ++i_blink){

			debug_file << "Blink number " << i_blink+1 << endl;
			debug_file << endl;
			std::cout << "Blink number " << i_blink+1 << endl;

			// This is the straightfoward procedure, not optimized for the exponential growth
			n_stones = evaluate_stones(debug_file, stones);
		
			debug_file << endl;

		}

		debug_file << "Total number of stones after " << n_blinks << " blinks is " << n_stones << endl;
		std::cout << "Total number of stones after " << n_blinks << " blinks is " << n_stones << endl;

	} else if( user_choice == 2 ){ // Optimized

		// Here is the approach:
		// - Instead of saving every stone at each iteration step, we will be concerned with two things
		// - Store each unique stone number and how many of these unique stone numbers exist
		// At the end I'm only interested in the total number of stones
		// I will have two vectors
		// - unique_stones :: Stores the unique stone numbers that exist
		// - stone_count :: Stores the count of each unique stone
		// The indices of these stones will relate the two vectors with eachother

		// It's long long because I don't know how large a stone can get
		std::vector<long long> unique_stones;

		unique_stones = get_stones(input_file); // We assume the starting stones are all unique

		// We initialize the count as 1 for each stone
		std::vector<long long> stone_count(unique_stones.size(), 1);

		debug_file << "Stones initial arrangement by stone number and the count of each stone number (stone | count): ";
		for(size_t i_stone = 0; i_stone < unique_stones.size(); ++i_stone){
			debug_file << unique_stones[i_stone] << " | " << stone_count[i_stone] << " ; ";
		}
		debug_file << endl;
		debug_file << endl;

		int n_blinks = 75;
		long long n_stones;

		for(int i_blink = 0; i_blink < n_blinks; ++i_blink){

			debug_file << "Blink number " << i_blink+1 << endl;
			debug_file << endl;
			std::cout << "Blink number " << i_blink+1 << endl;

			// This is the straightfoward procedure, not optimized for the exponential growth
			optimized_evaluate_stones(debug_file, unique_stones, stone_count);

			// Sum all counts in stone_count
			n_stones = 0;
			for(const long long &count : stone_count){
				n_stones = n_stones + count;
			}

			debug_file << "Total number of stones after " << i_blink+1 << " blinks is " << n_stones << endl;
			std::cout << "Total number of stones after " << i_blink+1 << " blinks is " << n_stones << endl;

			debug_file << endl;
			debug_file << "-----------------------------------------" << endl;
			debug_file << endl;
			std::cout << endl;
			std::cout << "-----------------------------------------" << endl;
			std::cout << endl;

		}

	} else { // user_choice invalid

		std::cout << "Method choice is not valid. Please try again" << endl;

	}

}
