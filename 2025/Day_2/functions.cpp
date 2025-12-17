#include "functions.hpp"
#include <iostream>
#include <sstream>
#include <cmath>
using namespace std;

// -----------------------------------------------

void get_ranges(std::ifstream &input_file, std::vector<long long> &start_IDs, std::vector<long long> &end_IDs){

	std::string full_line, range, ID_string;

	// Get all of the file content: it's one single line
	std::getline(input_file, full_line);

	// Separate the commas, get all the [start ID]-[end ID] ranges
	std::stringstream full_stream(full_line);

	// Go over ranges as strings

	int i_range = 0;

	while( std::getline(full_stream, range, ',') ){

		std::stringstream range_stream(range);

		int which_range = 0; // 0 is left (start), 1 is right (end)

		while( std::getline(range_stream, ID_string, '-') ){

			if(which_range == 0){
				start_IDs.push_back( std::stoll(ID_string) );
			} else if(which_range == 1){
				end_IDs.push_back( std::stoll(ID_string) );
			}

			which_range = which_range + 1;

		}

		i_range = i_range + 1;

	}

}

// -----------------------------------------------

int get_idx_of_min_value_in_vec(const std::vector<long long> &vec, const int &N){

	int min_idx = 0;
	long long current_val, min_val;

	min_val = vec[min_idx];

	for(int i = 1; i < N; ++i){

		current_val = vec[i];

		if( current_val < min_val ){

			min_idx = i;
			min_val = current_val;

		}

	}

	return min_idx;

}

// -----------------------------------------------

int get_idx_of_max_value_in_vec(const std::vector<long long> &vec, const int &N){

	int max_idx = 0;
	long long current_val, max_val;

	max_val = vec[max_idx];

	for(int i = 1; i < N; ++i){

		current_val = vec[i];

		if( current_val > max_val ){

			max_idx = i;
			max_val = current_val;

		}

	}

	return max_idx;

}

// -----------------------------------------------

std::vector<std::vector<int>> get_digits_vector(const std::vector<long long> &start_IDs, const std::vector<long long> &end_IDs, const int &n_ranges){

	std::vector<std::vector<int>> ranges_per_digits(n_ranges);

	for(int i_range = 0; i_range < n_ranges; ++i_range){

		// Get minimum and maximum ID for the range i_range
		long long min_ID = start_IDs[i_range];
		long long max_ID = end_IDs[i_range];

		// Get the number of digits for the min and max IDs
		int min_digits = std::trunc( std::log10(min_ID) )+1;
		int max_digits = std::trunc( std::log10(max_ID) )+1;

		// Push back the range index, i_range, into ranges_per_digits
		ranges_per_digits[min_digits].push_back(i_range);
		if( max_digits != min_digits ) ranges_per_digits[max_digits].push_back(i_range); // Avoids double counting

	}

	return ranges_per_digits;

}

// -----------------------------------------------

bool contains_ID(const std::vector<long long> &unique_invalid_IDs, const long long &candidate){

	// This function verifies if the unique_invalid_IDs vector already contains the given candidate for invalid ID
	// By default, we assume it does not
	// The if evaluates if it has; and if so, returns true, indicating page is already in unique_invalid_IDs
	// -- Exactly the same functions as the pages exercise from Day 5 2024!

	for( int unique_invalid_ID : unique_invalid_IDs ){ if( unique_invalid_ID == candidate ) return true; }

	return false;
}

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

void special_invalid_IDs(std::ofstream &debug_file, const std::string &filepath, const std::string &filename){

	// Opening input file
	std::ifstream input_file;
	input_file.open(filepath + filename);

	/*
	Reading input file
	- Consists of a single line, with multiple ranges, separated by commas
	- Each range is given like this: [start ID]-[end ID], i.e, start and end IDs separated by -
	- Make two vectors: start_IDs and end_IDs, with sizes = n_ranges
	- An index from 0 to n_ranges-1 uniquely identifies a range, e.g:
	- (*) i_range = 2 --> Range from start_IDs[2] to end_IDs[2]
	*/

	std::vector<long long> start_IDs, end_IDs;
	get_ranges(input_file, start_IDs, end_IDs);

	int n_ranges = start_IDs.size();

	debug_file << "Ranges (there are " << n_ranges << " in total):" << endl;

	for(int i_range = 0; i_range < n_ranges; ++i_range){

		debug_file << "(*) Index " << i_range << " | Start ID = " << start_IDs[i_range] << " | End ID = " << end_IDs[i_range] << endl;

	}

	debug_file << endl;

	// Building the vector with the vector of ranges worth searching for, for each number of digits
	std::vector<std::vector<int>> ranges_per_digits = get_digits_vector(start_IDs, end_IDs, n_ranges);

	/*
	Calculate:
	- Lowest start ID
	- Highest end ID
	*/

	int min_idx_ID = get_idx_of_min_value_in_vec(start_IDs, n_ranges);
	long long min_ID = start_IDs[min_idx_ID];

	int max_idx_ID = get_idx_of_max_value_in_vec(end_IDs, n_ranges);
	long long max_ID = end_IDs[max_idx_ID];

	debug_file << "Global minimum ID = " << min_ID << endl;
	debug_file << "Global maximum ID = " << max_ID << endl;
	debug_file << endl;

	// Calculate the number of digits of the min and max IDs

	int n_min_digits = std::trunc( std::log10(min_ID) )+1;
	int n_max_digits = std::trunc( std::log10(max_ID) )+1;

	debug_file << "Number of digits of the global minimum ID = " << n_min_digits << endl;
	debug_file << "Number of digits of the global maximum ID = " << n_max_digits << endl;
	debug_file << endl;

	debug_file << "Ranges that contain each number of digits:" << endl;
	for(int digits = n_min_digits; digits <= n_max_digits; ++digits){

		debug_file << "(*) " << digits << "-digit numbers, present in ranges: ";

		for(const int &i_range : ranges_per_digits[digits]){

			debug_file << i_range << " (" << start_IDs[i_range] << "-" << end_IDs[i_range] << ") | "; 

		}

		debug_file << endl;

	}
	debug_file << endl;

	// Make a loop that goes through all digits (if the minimum number of digits is 1, add one - 1-digit numbers cannont be invalid IDs)

	// Initializing number of invalid IDs and sum of invalid IDs
	int n_invalid = 0;
	long long invalid_sum = 0;

	// Unique invalid IDs vector
	std::vector<long long> unique_invalid_IDs;

	if( n_min_digits == 1) n_min_digits = n_min_digits + 1;

	for(int N = n_min_digits; N <= n_max_digits; ++N){

		debug_file << "Number of digits, N = " << N << endl;
		debug_file << endl;

		// Make a loop of possible factors of N (numbers that divide N and yield 0 remainder, excluding N itself)
		// 1 will always be possible of course
		for( int d = 1; d < N; ++d ){

			int n_invalid_local = 0;

			if( N%d == 0 ){ // Other possible d's (excluding N itself because it only repeats itself in a number with N-digits once)

				int k = N/d;

				// Make a loop that goes through all seeds, which are ...
				// ... all the numbers with d-digits

				long long min_seed = lround( std::pow(10, d-1) );
				long long max_seed = lround( std::pow(10, d)-1 );

				debug_file << "\t" << "Dividing a " << N << "-digit number into " << k << " seperate " << d << "-digit numbers. Candidate seeds = [" << min_seed << ", " << max_seed << "]" << endl;

				for(long long seed = min_seed; seed <= max_seed; ++seed){

					// Calculate invalid ID candidate
					//long long candidate = seed*lround( std::pow(10, d) ) + seed; // This only works for candidates with even digits

					/*
					
					How to build candidate. Let's rephrase the question.
					How to calculate a number, call it X, with N digits, from a number s, with d digits, that is repeated (s, that is) k times?

					Start with candidate = s
					Then, s*10 + candidate = s*10 + s = ss. Define it as the new candidate :: candidate = ss
					Then, s*10^2 + candidate = s*100 + ss = sss. Define it as the new candidate :: candidate = sss
					...
					Finally, s*10^(k-1) + candidate = s*10^(k-1) + s*10^(k-2) + ... + s*10 + s = sss...s (s repeated k times)

					If s has 1-digit (d = 1), then s repeated k times means the final number has k-digits (N = k) [E.g.: 6-digit number partitioned into 1-digit numbers does have d = 1 with k = 6 => N = k = 6]
					If s has 2-digit (d = 2), then s repeated k times means the final number has k*2-digits (N = k*2) [E.g.: 6-digit number partitioned into 2-digit numbers does have d = 2 with k = 3 => N = 2*k = 6]
					If s has 3-digit (d = 3), then s repeated k times means the final number has k*3-digits (N = k*3) [E.g.: 6-digit number partitioned into 3-digit numbers does have d = 3 with k = 2 => N = 2*k = 6]
					...

					*/

					long long candidate = seed;

					//debug_file << "\t" << "(*) Candidate calculation | Starts of as seed = " << candidate << endl;

					for(int i_k = 1; i_k <= k-1; ++i_k){

						candidate = seed*lround( std::pow(10, i_k*d) ) + candidate; // The power of 10 needs to go in exponent steps of i_k*d ("block number" times the number of digits of the seed)

						//debug_file << "\t" << "\t" << "<*> Next k block (i_k = " << i_k << ") | seed to the 10 power = " << seed*lround( std::pow(10, i_k*d) ) << " | Updated candidate = " << candidate << endl;

					}

					//debug_file << "\t" << "(*) seed = " << seed << " | candidate = " << candidate << " | N = " << N << " | d = " << d << " | k = " << k << endl;

					/*

					We could do the same search for candidate as before:
					- Loop through all the ranges
					- Do an if that sees if candidate is inside that range
					
					Or we could try a new approach:
					- First, build a vector (vec) that, for an index i, reprensenting number of digits (i.e, value of d), contains a vector with all range indices worth searching for
					- Then we go through the values in the vector inside vec[d]

					*/

					// Go over the elements in the vector contained in ranges_per_digits[N] (elements are range indices)
					// ranges_per_digits[N] are the ranges that contains numbers with N digits
					// Re-call that our candidate is an N-digit number (seed is a d-digit number)
					for(const int &i_range : ranges_per_digits[N]){

						//debug_file << "\t" << "(*) Looking at range index " << i_range << " (" << start_IDs[i_range] << "-" << end_IDs[i_range] << ")" << endl;

						// We need to avoid double countined the same invalid ID
						// For that, we keep track of a vector of all unique invalid IDs
						// When a candidate passes the range check, we then check to see if it is on this list
						// And if it is, we ignore it, since it was already counted
						if(candidate >= start_IDs[i_range] && candidate <= end_IDs[i_range]){

							if( contains_ID(unique_invalid_IDs, candidate) ){ // If candidate is already on the vector of unique valid IDs, we ignore it

								debug_file << "\t" << "(*) Ignore candidate " << candidate << " (built with seed = " << seed << "). It is in a range (index = " << i_range << ", corresponding to " << start_IDs[i_range] << "-" << end_IDs[i_range] << ") but it is not unique" << endl;

							} else {

								n_invalid = n_invalid + 1;
								n_invalid_local = n_invalid_local + 1;
								invalid_sum = invalid_sum + candidate;
								unique_invalid_IDs.push_back(candidate);

								debug_file << "\t" << "(*) Candidate " << candidate << " (built with seed = " << seed << ") is in a range (index = " << i_range << ", corresponding to " << start_IDs[i_range] << "-" << end_IDs[i_range] << ") and is unique | We add it to the list of unique invalid IDs" << endl;

							}
							//debug_file << "\t" << "(*) Candidate belongs in this range | Number of invalid IDs with " << d << "-digits = " << n_invalid_local << " | Total number of invalid IDs = " << n_invalid << " | Sum of invalid IDs = " << invalid_sum << endl;

						}/* else {

							debug_file << "\t" << "(*) Candidate does not belong in this range. Next range." << endl;

						}*/

					}

				}

				debug_file << "\t" << "For " << d << "-digit partitions, " << n_invalid_local << " invalid IDs were confirmed | Updated count = " << n_invalid << " | Updated sum = " << invalid_sum << endl;
				debug_file << endl;

			} // End of possible d-digit numbers for N-digit invalid ID

		} // End of going over digits

		debug_file << endl;

	}

	debug_file << "Results | Number of invalid IDs = " << n_invalid << " | Invalid IDs sum = " << invalid_sum << endl;
	std::cout << "Results | Number of invalid IDs = " << n_invalid << " | Invalid IDs sum = " << invalid_sum << endl;

}

// -----------------------------------------------

void regular_invalid_IDs(std::ofstream &debug_file, const std::string &filepath, const std::string &filename){

	// Opening input file
	std::ifstream input_file;
	input_file.open(filepath + filename);

	/*
	Reading input file
	- Consists of a single line, with multiple ranges, separated by commas
	- Each range is given like this: [start ID]-[end ID], i.e, start and end IDs separated by -
	- Make two vectors: start_IDs and end_IDs, with sizes = n_ranges
	- An index from 0 to n_ranges-1 uniquely identifies a range, e.g:
	- (*) i_range = 2 --> Range from start_IDs[2] to end_IDs[2]
	*/

	std::vector<long long> start_IDs, end_IDs;
	get_ranges(input_file, start_IDs, end_IDs);

	int n_ranges = start_IDs.size();

	debug_file << "Ranges (there are " << n_ranges << " in total):" << endl;

	for(int i_range = 0; i_range < n_ranges; ++i_range){

		debug_file << "(*) Index " << i_range << " | Start ID = " << start_IDs[i_range] << " | End ID = " << end_IDs[i_range] << endl;

	}

	debug_file << endl;

	/*
	Calculate:
	- Lowest start ID
	- Highest end ID
	*/

	int min_idx_ID = get_idx_of_min_value_in_vec(start_IDs, n_ranges);
	long long min_ID = start_IDs[min_idx_ID];

	int max_idx_ID = get_idx_of_max_value_in_vec(end_IDs, n_ranges);
	long long max_ID = end_IDs[max_idx_ID];

	debug_file << "Global minimum ID = " << min_ID << endl;
	debug_file << "Global maximum ID = " << max_ID << endl;
	debug_file << endl;

	// Calculate the number of digits of the min and max IDs

	int n_min_digits = std::trunc( std::log10(min_ID) )+1;
	int n_max_digits = std::trunc( std::log10(max_ID) )+1;

	debug_file << "Number of digits of the global minimum ID = " << n_min_digits << endl;
	debug_file << "Number of digits of the global maximum ID = " << n_max_digits << endl;
	debug_file << endl;

	// Define the start and end values of the h loop
	// Involves checking if the number of digits we just calculated are even or not

	int start_h, end_h;

	if( n_min_digits%2 == 0 ){ // Number of digits of min ID is even

		start_h = n_min_digits/2;

	} else { // Number of digits of min ID is odd

		start_h = (n_min_digits + 1)/2;

	}

	debug_file << "Starting value for h loop = " << start_h << endl;

	if( n_max_digits%2 == 0 ){ // Number of digits of max ID is even

		end_h = n_max_digits/2;

	} else { // Number of digits of max ID is odd

		end_h = (n_max_digits - 1)/2;

	}

	debug_file << "Ending value for h loop = " << end_h << endl;
	debug_file << endl;

	// Initializing number of invalid IDs and sum of invalid IDs
	int n_invalid = 0;
	long long invalid_sum = 0;

	// Perform the h loop | h is called half-length, because it represents half of the length of a candidate to invalid ID
	for(int h = start_h; h <=end_h; ++h){

		debug_file << "Building seeds with " << h << "-digits, corresponding to invalid ID candidates of " << h*2 << "-digits" << endl;

		/*

		An invalid ID is a number N with
		- Even number of digits, i.e, N_digits%2 == 0 (because it needs to be broken in half)
		- Constructed from a number called a seed, or, s
		- The seed s has N_digits/2 digits
		- N = s*10^(N_digits/2) + s (seed s is therefore half of N, i.e, N as a string is literally [s][s])
		- s will go from 10^(h-1) to 10^h-1. Examples:
		- (*) h = 1 :: min_s = 10^(1-1) = 1, max_s = 10^1-1 = 9 | I.e, all 1-digit numbers
		- (*) h = 2 :: min_s = 10^(2-1) = 10, max_s = 10^2-1 = 99 | I.e, all 2-digit numbers
		- (*) h = 5 :: min_s = 10^(5-1) = 10000, max_s = 10^5-1 = 99999 | I.e, all 5-digit numbers
		Conclusions and summaries:
		- h determines the number of digits of the seed. We go over all those numbers as possible seeds
		- h = N_digits/2, meaning, with ...
		- (*) h = 1 and all its seeds we are looking at all possible Ns with 2-digits
		- (*) h = 2 and all its seeds we are looking at all possible Ns with 4-digits
		- (*) h = 5 and all its seeds we are looking at all possible Ns with 10-digits

		With this method, we construct every possible number, from start_h-digits to end_h-digits, ...
		... that has an even number of digits and can be separated into two equal halves.

		Then what's left to do, is search the ID ranges and see how many of these numbers fall inside ...
		... the ranges. That will be the number of invalid IDs

		*/

		// Calculate min and max seed values
		long long min_seed = lround( std::pow(10, h-1) );
		long long max_seed = lround( std::pow(10, h)-1 );

		int n_invalid_local = 0;

		// Go over all possible seeds
		for(long long seed = min_seed; seed <= max_seed; ++seed){

			// Calculate invalid ID candidate
			long long candidate = seed*lround( std::pow(10, h) ) + seed;

			// See if candidate falls inside any of the ranges
			for(int i_range = 0; i_range < n_ranges; ++i_range){

				if( candidate >= start_IDs[i_range] && candidate <= end_IDs[i_range] ){

					n_invalid = n_invalid + 1;
					n_invalid_local = n_invalid_local + 1;
					invalid_sum = invalid_sum + candidate;

					debug_file << "(*) seed = " << seed << " | Successful invalid ID candidate = " << candidate << " | Falls inside range index = " << i_range << " (" << start_IDs[i_range] << "-" << end_IDs[i_range] << ")" << endl;
				
				}

			}

		}

		debug_file << "For thif half-length, " << n_invalid_local << " invalid IDs were confirmed | Updated count = " << n_invalid << " | Updated sum = " << invalid_sum << endl;
		debug_file << endl;

	}

	debug_file << "Results | Number of invalid IDs = " << n_invalid << " | Invalid IDs sum = " << invalid_sum << endl;
	std::cout << "Results | Number of invalid IDs = " << n_invalid << " | Invalid IDs sum = " << invalid_sum << endl;

}
