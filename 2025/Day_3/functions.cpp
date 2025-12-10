#include "functions.hpp"
#include <iostream>
#include <cmath>
using namespace std;

// -----------------------------------------------

void read_joltage(std::ofstream &debug_file, std::ifstream &input_file){

	std::string line;

	debug_file << "Going over battery banks" << endl;
	debug_file << endl;

	int i_bank = 1;
	long long first_battery = 0;
	long long second_battery = 0;
	long long max_joltage = 0;
	long long sum_max_joltage = 0;

	int user_choice;
	std::cout << "Select which maximum joltages to look for" << endl;
	std::cout << "[1] 2-digit joltages" << endl;
	std::cout << "[2] 12-digit joltages" << endl;
	std::cout << "Enter choice (1/2): ";
	std::cin >> user_choice;

	int n_digits = 12; // For user_choice == 2

	while( std::getline(input_file, line) ){

		/*
		Line is a battery bank. Each digit is a battery
		For a given bank, we turn on two batteries. We see the number they make when putting the two digits (in order) together
		The largest joltage of the bank will be the two batteries that, when on, make the largest number possible
		The total output joltage will be the sum of the largest joltage from all banks (all lines)

		Challenge : Try not to use vector
		*/

		size_t N = line.size(); // Number of batteries in the bank. line = battery bank

		int *digits = new int[N]; // Allocatable array called digits. digits has been allocated as an integer array with size N
		/*
		digits is actually a pointer, pointing to an integer array with N entries
		Also, although a pointer to a single integer would work like this:
		int *p; Pointer to an integer
		*p = 10; Setting the value of the integer the pointer is pointing to to 10. We are dereferencing the pointer to get the value it points to
		We obtain the same by doing digits[i] = 10. This sets the value of the array entry the pointer is pointing to to 10. [i] automatically does the dereferencing
		*/

		int i = 0; // Index for digits

		debug_file << "(*) Battery bank number " << i_bank << " | Number of batteries in bank = " << N << endl;

		for(const char &c : line){ // Goes over each character in the line string

			/*
			How to transform a character (which we know contains a number from 0 to 9) into an integer
			- '0' to '9' are contiguous characters in ASCII
			- This means that subtracting any of these characters by '0' will yield the integer value they contain. Neat trick!
			*/

			digits[i] = c - '0';
			i = i + 1; // Advance digits index

		}

		debug_file << "(*) Batteries = ";
		for(size_t j = 0; j < N; j++){
			debug_file << digits[j] << " ";
		}
		debug_file << endl;

		// Forming all possible 2(or 12)-digit numbers and finding the largest one
		if(user_choice == 1){ // 2-digit joltages

			max_joltage = finding_largest_joltage_two_digits(digits, N, first_battery, second_battery);
			debug_file << "(*) Maximum bank joltage = " << max_joltage << " | Made possible by turning on batteries " << first_battery << " and " << second_battery << endl;

		} else if (user_choice == 2) { // 12-digit joltages

			//max_joltage = finding_largest_joltage_n_digits_brute_force(debug_file, digits, N, n_digits);
			max_joltage = finding_largest_joltage_n_digits_optimal(debug_file, digits, N, n_digits); // WIP
			debug_file << "(*) Maximum bank joltage = " << max_joltage << endl;
		}

		delete[] digits; // Deallocate digits array

		sum_max_joltage = sum_max_joltage + max_joltage; // Sum of all max joltages

		i_bank = i_bank + 1; // Advance battery bank index

		debug_file << endl;

		std::cout << "Bank " << i_bank << endl;
		std::cout << endl;

	}

	debug_file << "Sum of all max joltages = " << sum_max_joltage << endl;
	std::cout << "Sum of all max joltages = " << sum_max_joltage << endl;

}

// -----------------------------------------------

long long finding_largest_joltage_two_digits(int *arr, const size_t &N, long long &first_battery, long long &second_battery){

	/*
	More about passing a pointer as a function argument:
	- The pointer passed to arr is passed by value. For our purposes, digits is what is going to occupy the arr argument
	- However, because the value of a pointer is a memory address, the effect is similar to passing by reference
	- The system makes a copy of the memory address held by digits and gives it to arr
	- The result is that digits and arr become two different pointer variables, but that point to the same memory address, that of the new int[N] array
	- If we modify arr[i] we are then modifying digits[i] directly as well, similar to passing a vector by reference

	We could still do int *&arr, and this would:
	- Be a reference to a pointer, allowing us to change the address itself by, for exampling, resizing the array, deallocate and re-allocate it inside this function, or make the pointer point to null
	
	We could also do const int *arr:
	- This would allow us to read the arr[i] value, but not change them
	*/

	long long first_digit, second_digit;
	long long max_joltage = 0;

	for(size_t first_digit_index = 0; first_digit_index < N-1; ++first_digit_index){ // Goes from the 1st to the penultimum digit

		first_digit = arr[first_digit_index];

		for(size_t second_digit_index = first_digit_index+1; second_digit_index < N; ++second_digit_index){ // Goes from the first_digit_index+1 to the last digit

			second_digit = arr[second_digit_index];

			int joltage = first_digit*10 + second_digit;

			if( joltage > max_joltage ){

				max_joltage = joltage;
				first_battery = first_digit;
				second_battery = second_digit;

			}

		}

	}

	return max_joltage;

	/*
	Apart from the approach I did here, which was:
	- Start from i = 0 and go to i = N-2. The i element is the first digit being considered
	- Then, go from j = i+1 to j = N-1. That is the second digit being considered
	- Make the number. If it's larger than the number that was last recorded to be largest, then this number is the new largest (start with largest number = 0 so that the if can be activated with no problems)

	I thought of another approach:
	- Find the maximum integer in the array. That is the first digit. Let's say the index of this element is i.
	- Then, find ther maximum integer in the array slice from i to N-1. That's the second integer
	*/

}

// -----------------------------------------------

long long finding_largest_joltage_n_digits_optimal(std::ofstream &debug_file, int *arr, const size_t &N, const size_t &n_digits){

	long long max_joltage = 0;

	int *final_digits = new int[n_digits];

	int top = -1; // To always guarantee we get the largest possible number for all indices in final_digits; -1 means no digit in final_digits is up for debate if it can or cannot be substituted right now

	/*
	Explanation with an example. Consider arr = [4, 5, 9, 1] (N = 4) and n_digits = 3 | We can immediatly see that the right answer has to be 591
	--- ---- ---
	- i = 0 --> arr[0] = 4 being analyzed
	- top = -1, meaning final_digits is empty; does not enter while loop
	- top = -1 < n_digits-1 = 2 | top is increased to 0 and 4 is put into final_digits[0]
	--- ---- ---
	- i = 1 --> arr[1] = 5 being analyzed
	- While loop:
	- (*) top = 0 meaning we are studying possibility of substituting final_digits[0]
	- (*) final_digits[0] <>> current_int --> 4 < 5 is true so if possible we do want to exchange 4 with 5
	- (*) (top + (N - i)) >= n_digits --> (0 + (4 - 1)) >= 3 --> 3 >= 3 | (top + (N - i)) are the digits leftover to analyze, n_digits is how many we need in the number
	- (*) Conditions are met to go in the loop, so now top is reduced to -1. Because of this, loop does not continue
	- If block:
	- (*) top = -1 < n_digits-1 = 2 | True once again, top is increased to 0 and 5 is put into final_digits[0], effectively substituting 4
	--- ---- ---
	- i = 2 --> arr[2] = 9 being analyzed
	- While loop:
	- (*) top = 0 meaning we are studying possibility of substituting final_digits[0]
	- (*) final_digits[0] < current_int --> 5 < 9 is true so if possible we do want to exchange 9 with 5
	- (*) (top + (N - i)) >= n_digits --> (0 + (4 - 2)) >= 3 --> 2 >= 3 | Number requires 3 digits and we only have 2 left to analyze, so no more substitutions possible
	- If block:
	- (*) top = 0 < n_digits-1 = 2 | True once again, top is increased to 1 and 9 is put into final_digits[1]
	--- ---- ---
	- i = 3 --> arr[3] = 1 being analyzed
	- While loop:
	- (*) top = 1 meaning we are studying possibility of substituting final_digits[1]
	- (*) final_digits[1] < current_int --> 1 < 9 is false, so we keep going
	- If block:
	- (*) top = 1 < n_digits-1 = 2 | True once again, top is increased to 2 and 1 is put into final_digits[2]
	End of loop sice we arrive at N = 4. Final number is final_digits[0] = 5, final_digits[1] = 9, final_digits[2] = 1,
	*/

	for(size_t i = 0; i < N; i++){ // Go over batteries

		int current_int = arr[i]; // Current battery (integer from 0 to 9)

		/*
		Decides if current battery is better than battery occupying current index in final_digits (given by top)
		- top > -1 :: To check if there is a digit in final_digits worth trying to improve is possible
		- final_digits[top] < current_int :: Digit in top spot is less than current digit being analyzed (which is current_int)
		- (top + (N - i)) >= n_digits :: Are there enough digits leftover to analyze? If there aren't, we can't substitute with current_int, we keep going
		*/
		while(top > -1 && final_digits[top] < current_int && (top + (N - i)) >= n_digits){
			top = top - 1;
		}

		if( top < (int)n_digits-1 ){
			top = top + 1;
			final_digits[top] = current_int;
		}

	}

	for(size_t k = 0; k < n_digits; ++k){
		max_joltage = max_joltage + (long long)std::pow(10, n_digits-1-k)*(long long)final_digits[k];
	}

	delete[] final_digits;

	return max_joltage;

}

// -----------------------------------------------

long long finding_largest_joltage_n_digits_brute_force(std::ofstream &debug_file, int *arr, const size_t &N, const size_t &n_digits){

	long long *final_digits = new long long[n_digits]; // Stores the n_digits we use to make the number
	size_t *final_indices = new size_t[n_digits]; // Stores the indices of the n_digits to make a number

	/*
	Solution: find out all unique sets of n_digits integers from the N in arr
	For each set, I make the n_digits-digit integer number, and calculate the maximum as I did with the 2-digit integers
	*/

	// From cmath, we get the Gamma function, and :: Gamma(n+1) = n!
	long long n_permutations = (long long)( tgamma(N+1)/( tgamma(n_digits+1)*tgamma(N-n_digits+1) ) );

	debug_file << "(*) Number of unique " << n_digits << " integer sets from the " << N << " in the bank = " << n_permutations << endl;

	long long i_permutation = 1; // Which permutation we are on
	long long max_joltage = 0;

	// Build first permutation
	for(size_t k = 0 ; k < n_digits; ++k){
		final_indices[k] = k;
	}

	while( i_permutation <= n_permutations ){

		// Build set of n_digits integers
		int i = 0;
		for(size_t t = 0; t < N; ++t){ // Goes over all N indices
			if( contains_element(final_indices, n_digits, t) ){
				final_digits[i] = arr[t]; // We add it to the set, i.e, to final_digits
				i = i+1;
			}
		}

		// See if current set yields the largest n_digits integer
		long long joltage = 0;		
		for(size_t k = 0; k < n_digits; ++k){
			joltage = joltage + (long long)std::pow(10, k)*final_digits[n_digits-1-k];
		}
		if( joltage > max_joltage ){
			max_joltage = joltage;
		}

		// Advance to the next permutation
		i_permutation = i_permutation + 1;

		// Build a permutation
		build_permutation(final_indices, N, n_digits); // WIP

	}

	delete[] final_digits;

	return max_joltage;

}

// -----------------------------------------------

bool contains_element(const size_t *arr, const size_t &N, const size_t &val){

	for(size_t ele = 0; ele < N; ++ele){
		if(arr[ele] == val) return true;
	}

	return false;
}

// -----------------------------------------------

void build_permutation(size_t *final_indices, const size_t &N_arr, const size_t &N_set){

	// Finding the rightmost element that has not yet reached its maximum possible value
	// For index i, its maximum possible index is N_arr - N_set + i

	size_t i = N_set - 1;
	while( i>=0 && final_indices[i] == N_arr - N_set + i ){
		i = i - 1;
	}

	if(i < 0){
		return;
	}

	final_indices[i] = final_indices[i] + 1;

	for(size_t j = i+1; j < N_set; j++){
		final_indices[j] = final_indices[j-1] + 1;
	}

}

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

void main_function(std::ofstream &debug_file, const std::string &filepath, const std::string &filename){

	// Opening input file
	std::ifstream input_file;
	input_file.open(filepath + filename);

	// Reading input file - joltage math will be done as the file is read
	read_joltage(debug_file, input_file);

}
