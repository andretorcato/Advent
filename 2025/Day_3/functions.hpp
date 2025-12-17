#ifndef functions
#define functions

#include<fstream>
#include<string>

// -------------------------------------------------

void main_function(std::ofstream &debug_file, const std::string &filepath, const std::string &filename);

void read_joltage(std::ofstream &debug_file, std::ifstream &input_file);

long long finding_largest_joltage_two_digits(int *arr, const size_t &N, long long &first_battery, long long &second_battery);

long long finding_largest_joltage_n_digits_brute_force(std::ofstream &debug_file, int *arr, const size_t &N, const size_t &n_digits);

bool contains_element(const size_t *arr, const size_t &N, const size_t &val);

void build_permutation(size_t *final_indices, const size_t &N_arr, const size_t &N_set);

long long finding_largest_joltage_n_digits_optimal(std::ofstream &debug_file, int *arr, const size_t &N, const size_t &n_digits);

#endif
