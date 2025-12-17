#ifndef functions
#define functions

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

std::vector<Rule> get_rules(std::ifstream &input_file);
bool contains_page(const std::vector<int> &unique_pages, const int &page);
int get_page_index(const std::vector<int> &unique_pages, const int &n_unique_pages, const int &page);
std::vector<int> get_unique_pages(const std::vector<Rule> &all_rules);
std::vector<Rule> get_rules_subset(std::ofstream &debug_file, const std::vector<Rule> &all_rules, const int &n_rules, const std::vector<int> &unique_pages, const int &n_unique_pages, const size_t &update_size, std::vector<int> &update);
std::vector<std::vector<int>> get_rules_graph(std::ofstream &debug_file, const std::vector<int> &unique_pages, const int &n_unique_pages, const std::vector<Rule> &update_rules, const int &n_update_rules, std::vector<int> &page_degrees);
std::vector<int> get_final_order(std::ofstream &debug_file, const std::vector<int> &update, const std::vector<int> &unique_pages, const int &n_unique_pages, std::vector<int> &page_degrees, const std::vector<std::vector<int>> &page_graph);
bool compare_pages(const int &page_a, const int &page_b, const std::vector<Rule> &update_rules);
int partition_update(std::vector<int> &vec, int low, int high, const std::vector<Rule> &rules);
void quicksort_update(std::vector<int> &vec, int low, int high, const std::vector<Rule> &rules);
void correct_update(std::vector<int> &update, const int &update_size, const std::vector<Rule> &update_rules);
int go_over_updates(std::ofstream &debug_file, std::ifstream &input_file, const std::vector<Rule> &all_rules, const int &n_rules);
void summ_mid_pages_correct_updates(std::ofstream &debug_file, const std::string &filepath, const std::string &filename);

#endif
