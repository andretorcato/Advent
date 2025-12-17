#include "functions.hpp"
#include <iostream>
using namespace std;

// -----------------------------------------------

int rotate_dial(std::ofstream &debug_file, int &dial, const std::string &rotation_direction, int &rotation_length, int &n_zero_clicks){

	int new_dial;

	/*
	rotation_length needs to be analyzed before calculating new_dial
	Examples:
	- 50 --[ R49 ]--> 99
	- 50 --[ R50 ]--> 0
	- 50 --[ R60 ]--> 10
	- 50 --[ R99 = R50 + R49 ]--> 49
	- 50 --[ R100 ]--> 50 | R100 does not change dial
	- 50 --[ L50 ]--> 0
	- 50 --[ L60 ]--> 90
	- 50 --[ L99 = L50 + L49 ]--> 51
	- 50 --[ L100 ]--> 50 | Same for going backwards
	This means that, for any rotation largar than 2-digits, we can ignore all of its upper digits
	Only the 10^0 and 10^1 digits matter for the rotation
	Example:
	- 50 --[ R84 = R50 + R34 ]--> 34
	- 50 --[ R584357326439532984 = R5843573264395329*100 + R84 ] <==> [ R84 ]--> 34
	I can obtain the last 2-digits of a number x that has 3-digits or more by doing x%100
	*/

	// First, analyze rotation_length and set it to its two-digit form that actually matters

	if(rotation_length > 100){

		int passes_by_zero = rotation_length/100;

		rotation_length = rotation_length%100;

		debug_file << "Rotation length is larger than 2-digits. Real length = " << rotation_length << endl;
		debug_file << "Other digits contribute to full rotations, resulting in a number of passes by 0 = " << passes_by_zero << endl;

		n_zero_clicks = n_zero_clicks + passes_by_zero;

		debug_file << "Updated count of passes by zero = " << n_zero_clicks << endl;

	}

	// Then, execute rotation, keeping going over 99 or below 0 with a rotation_length that is only 2-digits

	if( rotation_direction == "L" ){ // Rotation direction = L

		new_dial = dial - rotation_length;

		if( new_dial < 0 ){ // Passes by 0 once, but we need to make sure new_dial is not 0. If it is, we don't count it, to not duplicate counts with n_zeros

			new_dial = 100+new_dial;

			if(new_dial != 0 && dial != 0){ // If the dial doesn't start nor end at 0, to avoid double counting with n_zeros

				n_zero_clicks = n_zero_clicks + 1;

				debug_file << "Dial passed by 0 and didn't finish on 0, so +1 in the number of passes by 0" << endl;
				debug_file << "Updated count of passes by zero = " << n_zero_clicks << endl;

			}

		}

	} else { // Rotation direction = R

		new_dial = dial + rotation_length;

		if( new_dial > 99 ){ // Passes by 0 once, but we need to make sure new_dial is not 0. If it is, we don't count it, to not duplicate counts with n_zeros

			new_dial = new_dial-100;

			if(new_dial != 0 && dial != 0){ // If the dial doesn't start nor end at 0, to avoid double counting with n_zeros

				n_zero_clicks = n_zero_clicks + 1;

				debug_file << "Dial passed by 0 and didn't finish on 0, so +1 in the number of passes by 0" << endl;
				debug_file << "Updated count of passes by zero = " << n_zero_clicks << endl;

			}

		}

	}

	return new_dial;

}

// -----------------------------------------------

void main_function(std::ofstream &debug_file, const std::string &filepath, const std::string &filename){

	// Opening input file
	std::ifstream input_file;
	input_file.open(filepath + filename);

	// Go over each line of the file

	std::string line;
	int i_rotation = 0;
	int dial = 50;
	int n_zeros = 0; // Number of times dial ends up at 0
	int n_zero_clicks = 0; // Number of times dial passes by 0 during a rotation
	int prev_dial;

	while( std::getline(input_file, line) ){

		//debug_file << "Rotation number " << i_rotation+1 << " is "+line.substr(0, 1)+line.substr(1, line.size()) << endl;
		//std::cout << "Rotation number " << i_rotation+1 << " is "+line.substr(0, 1)+line.substr(1, line.size()) << endl;

		// Rotation direction = line.substr(0, 1) | It's either L or R
		std::string rotation_direction = line.substr(0, 1);

		// Rotation length = line.substr(1, line.size()) | It's a one-digit or two-digit integer number
		int rotation_length = std::stoi( line.substr(1, line.size()) );

		debug_file << "Rotation number " << i_rotation+1 << " :: Direction = "+rotation_direction+" | Length = " << rotation_length << endl;

		prev_dial = dial;
		dial = rotate_dial(debug_file, dial, rotation_direction, rotation_length, n_zero_clicks);

		debug_file << "Dial goes from " << prev_dial << " to " << dial << endl;

		// Checking if dial is 0
		if(dial == 0){
			n_zeros = n_zeros + 1;
			debug_file << "Dial fell on 0 | Update count to " << n_zeros << endl;
		}

		debug_file << endl;

		i_rotation = i_rotation+1; // Next rotation

	} // End of going over lines

	debug_file << "1 :: Number of times dial hit zero = " << n_zeros << endl;
	std::cout << "1 :: Number of times dial hit zero = " << n_zeros << endl;
	debug_file << "2 :: Number of times dial passed by zero = " << n_zero_clicks << endl;
	std::cout << "2 :: Number of times dial passed by zero = " << n_zero_clicks << endl;
	debug_file << "Therefore :: Total number of times dial passed by or hit zero = " << n_zeros + n_zero_clicks << endl;
	std::cout << "Therefore :: Total number of times dial passed by or hit zero = " << n_zeros + n_zero_clicks << endl;

}

/*
Notes:

- Safe has a dial with numbers on it and an arrow that points to a number at a time
- Numbers go from 0 to 99
- When it reaches a number, it makes a click sound

- Puzzle input: sequence of rotations, one per line (format of the file below)
- Rotation = (L || R)(Distance)
- L: Rotation to the left, i.e, towards the lower numbers
- R: Rotation to the right, i.e, towards the higher numbers
- Distance: How many clicks the dial should be rotated
- Example: If the dial starts at 11, a rotation R8 would bring it to 19, whereas a rotation L7 would bring it to 4
- Periodic boundary conditions: L1 from 0 yields 99, and R1 from 99 yields 0

- Dial starts pointed at 50
- Puzzle answer = Number of times the dial reaches 0 at the end of a rotation (not just pass by 0, but end a rotation on it)
- A file looks something like this (example input file):
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
- Interpreting:
L68 ----> From 50 to 82
L30 ----> From 82 to 52
R48 ----> From 52 to 0 (This would get added to the cumulative sum of times the dial points to 0)
L5 -----> From 0 to 95
etc...
*/
