#ifndef functions
#define functions

#include<fstream>
#include<string>
#include<vector>

// -------------------------------------------------

int rotate_dial(std::ofstream &debug_file, int &dial, const std::string &rotation_direction, int &rotation_length, int &n_zero_clicks);

void main_function(std::ofstream &debug_file, const std::string &filepath, const std::string &filename);

#endif
