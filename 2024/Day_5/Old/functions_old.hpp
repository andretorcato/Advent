#ifndef functions_old
#define functions_old

#include<fstream>
#include<string>
#include<vector>

// -------------------------------------------------

struct Rule{ // An X|Y rule

	int x; // X
	int y; // Y

	// No need for a constructor; struct is simple and so it will understand {. , .}

};

// -------------------------------------------------

// Core function
void summ_mid_pages_correct_updates(std::ofstream &debug_file, const std::string &filepath, const std::string &filename);

std::vector<Rule> get_rules(std::ifstream &input_file);

int get_page_index(const std::vector<int> &unique_vec, const int &N, const int &value);

std::vector<int> ordering_rules(std::ofstream &debug_file, const std::vector<Rule> &all_rules, const int &n_rules);

bool contains_value(const std::vector<int> &vec, const int &value);

int go_over_updates(std::ofstream &debug_file, std::ifstream &input_file, const std::vector<int> final_order, const int &n_final);

std::vector<int> get_unique_pages(const std::vector<Rule> &all_rules, std::vector<int> &x_pages);

std::vector<std::vector<int>> get_pages_graph(std::ofstream &debug_file, const int &n_pages, const std::vector<Rule> &all_rules, const int &n_rules, const std::vector<int> &pages, std::vector<int> &degrees);

std::vector<int> get_final_order(std::ofstream &debug_file, const std::vector<int> &pages, const int &n_pages, std::vector<int> &degrees, const std::vector<std::vector<int>> &graph);

#endif
