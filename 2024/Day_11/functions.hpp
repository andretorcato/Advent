#ifndef functions
#define functions

#include<fstream>
#include<string>
#include<vector>

// -------------------------------------------------

std::vector<long long> get_stones(std::ifstream &input_file);

long evaluate_stones(std::ofstream &debug_file, std::vector<long long> &stones);

void number_of_stones(std::ofstream &debug_file, const std::string &filepath, const std::string &filename);

#endif
