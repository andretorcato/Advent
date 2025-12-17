#ifndef functions
#define functions

#include<fstream>
#include<string>
#include<vector>

// -------------------------------------------------

void get_ranges(std::ifstream &input_file, std::vector<long long> &start_IDs, std::vector<long long> &end_IDs);

int get_idx_of_min_value_in_vec(const std::vector<long long> &vec, const int &N);

int get_idx_of_max_value_in_vec(const std::vector<long long> &vec, const int &N);

void regular_invalid_IDs(std::ofstream &debug_file, const std::string &filepath, const std::string &filename);

bool contains_ID(const std::vector<long long> &unique_invalid_IDs, const long long &candidate);

std::vector<std::vector<int>> get_digits_vector(const std::vector<long long> &start_IDs, const std::vector<long long> &end_IDs, const int &n_ranges);

void special_invalid_IDs(std::ofstream &debug_file, const std::string &filepath, const std::string &filename);

#endif
