#include "functions.hpp"
#include <iostream>
#include <sstream>
using namespace std;

// -----------------------------------------------

std::vector<Rule> get_rules(std::ifstream &input_file){

	std::string file_line;
	std::vector<Rule> all_rules;

	// --- --- ---

	while( std::getline(input_file, file_line) ){

		if(file_line.empty()){ // Leave loop once we get to the empty line
			break;
		}

		// Line is X|Y
		// Need to get X and Y from string file_line. For this:
		// - Treat string as a vector of characters
		// - Find position of |
		// - Get X and Y from this position

		size_t rule_break_pos = file_line.find('|');

		std::string X_str = file_line.substr(0, rule_break_pos);
		int X_page = std::stoi(X_str);

		std::string Y_str = file_line.substr(rule_break_pos + 1);
		int Y_page = std::stoi(Y_str);

		all_rules.push_back({X_page, Y_page});

	}

	return all_rules;

}

// -----------------------------------------------

bool contains_page(const std::vector<int> &unique_pages, const int &page){

	// This function verifies if the unique_pages vector already contains the given page
	// By default, we assume it does not
	// The if evaluates if it has; and if so, returns true, indicating page is already in unique_pages

	for( int unique_page_number : unique_pages ){ if( unique_page_number == page ) return true; }

	return false;
}

// -----------------------------------------------

int get_page_index(const std::vector<int> &unique_pages, const int &n_unique_pages, const int &page){

	// This function returns the page index of a given page, with regards to unique_pages
	// This assumes that, indeed, unique_pages has all unique entries, no duplicates

	for(int i_page = 0; i_page < n_unique_pages; ++i_page){
		if( unique_pages[i_page] == page ){
			return i_page;
		}
	}

	return -1; // By default, returns a negative number, indicating the page is not in unique_pages, which should not happen in principle
}

// -----------------------------------------------

std::vector<int> get_unique_pages(const std::vector<Rule> &all_rules){

	std::vector<int> unique_pages;

	// --- --- ---

	for(const Rule &rule : all_rules){

		// These if statements evaluate if the page in X or Y or rule X|Y already exist in unique_pages
		// If they don't already exist, they are added to the vector. If they already exist, nothing happens, we move on to the next rule

		if( !contains_page(unique_pages, rule.x) ) unique_pages.push_back(rule.x);
		if( !contains_page(unique_pages, rule.y) ) unique_pages.push_back(rule.y);

	}

	return unique_pages;

}

// -----------------------------------------------

std::vector<Rule> get_rules_subset(std::ofstream &debug_file, const std::vector<Rule> &all_rules, const int &n_rules, const std::vector<int> &unique_pages, const int &n_unique_pages, const size_t &update_size, std::vector<int> &update){

	std::vector<Rule> update_rules;

	// --- --- ---

	// Let's build a lookup table that indicates if each of the unique pages is present in the update or not
	// The pages are tagged by their indices - this table has the same size as unique_pages
	std::vector<bool> lookup_table(n_unique_pages, false); // By default, none of the pages are present in the update

	// Go over the pages in the update
	for(const int &page : update){

		int i_page = get_page_index(unique_pages, n_unique_pages, page); // Page index of this page in unique_pages
		lookup_table[i_page] = true; // In the lookup table, we indicate, via the page index, that this page is present in the update

	}

	// Go over all rules
	for(const Rule &rule : all_rules){

		int i_page_x = get_page_index(unique_pages, n_unique_pages, rule.x); // Page index of X page in rule X|Y in unique_pages
		int i_page_y = get_page_index(unique_pages, n_unique_pages, rule.y); // Page index of Y page in rule X|Y in unique_pages

		// If both X and Y pages are present in the update, this rule needs to be considered
		// And so, we add this rule to the subset of rules
		if(lookup_table[i_page_x] == true && lookup_table[i_page_y] == true){
			update_rules.push_back(rule);
		}

	}

	return update_rules;

}

// -----------------------------------------------

std::vector<std::vector<int>> get_rules_graph(std::ofstream &debug_file, const std::vector<int> &unique_pages, const int &n_unique_pages, const std::vector<Rule> &update_rules, const int &n_update_rules, std::vector<int> &page_degrees){

	std::vector<std::vector<int>> page_graph(n_unique_pages);

	// --- --- ---

	// DEBUG
	debug_file << "With the rules subset defined, building the page graph created by them" << endl;
	debug_file << endl;

	for(const Rule &rule : update_rules){ // Go over rules

		int page_x = rule.x; // X page of rule X|Y
		int page_x_index = get_page_index(unique_pages, n_unique_pages, page_x); // Unique page index of page X

		int page_y = rule.y; // Y page of rule X|Y
		int page_y_index = get_page_index(unique_pages, n_unique_pages, page_y); // Unique page index of page Y

		// DEBUG
		debug_file << "\t" << "Rule: " << page_x << " | " << page_y << endl;

		page_graph[page_x_index].push_back(page_y_index); // By their page indices, build the connection X --> Y

		// DEBUG
		debug_file << "\t" << "X --> Y connection built. Dependent pages of X (" << page_x << ") are now: ";
		for(size_t i_page_y = 0; i_page_y < page_graph[page_x_index].size(); ++i_page_y){
			int dependent_page_index = page_graph[page_x_index][i_page_y];
			debug_file << unique_pages[dependent_page_index] << " ";
		}
		debug_file << endl;

		// DEBUG
		debug_file << "\t" << "Degree of page X (" << page_x << ") = " << page_degrees[page_x_index] << endl;

		page_degrees[page_y_index] = page_degrees[page_y_index] + 1; // Increase degree of page Y by 1

		// DEBUG
		debug_file << "\t" << "Degree of page Y (" << page_y << ") = " << page_degrees[page_y_index] << endl;

		// DEBUG
		debug_file << endl;

	}

	return page_graph;

}

// -----------------------------------------------

std::vector<int> get_final_order(std::ofstream &debug_file, const std::vector<int> &update, const std::vector<int> &unique_pages, const int &n_unique_pages, std::vector<int> &page_degrees, const std::vector<std::vector<int>> &page_graph){

	std::vector<int> final_order, queue;

	// --- --- ---

	// DEBUG
	debug_file << "Putting all degree 0 pages in the printing queue: ";

	for(const int &page : update){ // Go over pages in update

		int page_index = get_page_index(unique_pages, n_unique_pages, page);

		// If page has degree 0, we put its index in the queue
		if(page_degrees[page_index] == 0){
			queue.push_back(page_index);

			// DEBUG
			debug_file << page << " ";
		}

	}
	
	// DEBUG
	debug_file << endl;
	debug_file << "Processing pages" << endl;
	debug_file << endl;

	size_t queue_index = 0;
	while(queue_index < queue.size()){ // Go over queue until we reach its end

		int page_index_in_queue = queue[queue_index]; // Page index of the page in front of the queue

		// DEBUG
		debug_file << "\t" << "Looking at first page in queue: " << unique_pages[page_index_in_queue] << endl;

		final_order.push_back(unique_pages[page_index_in_queue]); // We put the page into the vector that sets the page order

		// DEBUG
		debug_file << "\t" << "Page is printed. Master order currently looking like: ";
		for(size_t i = 0; i < final_order.size(); ++i){
			debug_file << final_order[i] << " ";
		}
		debug_file << endl;
		debug_file << "\t" << "Go to the dependent pages of this page and decrease their degrees:" << endl;

		// Go over dependent pages of the queue page being analyzed
		for(size_t i_page_y = 0; i_page_y < page_graph[page_index_in_queue].size(); ++i_page_y){

			int dependent_page_index = page_graph[page_index_in_queue][i_page_y]; // Page index of dependent page under analysis

			// DEBUG
			debug_file << "\t" << "\t" << "Dependent page number " << unique_pages[dependent_page_index] << " | Degree = " << page_degrees[dependent_page_index] << endl;
			
			page_degrees[dependent_page_index] = page_degrees[dependent_page_index] - 1; // Reduce dependent page's degree

			// DEBUG
			debug_file << "\t" << "\t" << "Degree of page is reduced by 1 | Degree = " << page_degrees[dependent_page_index] << endl;

			if(page_degrees[dependent_page_index] == 0){ // If reducing by 1 is enough to bring dependent page degree to 0, we add it to the queue

				queue.push_back(dependent_page_index);

				// DEUBG
				debug_file << "\t" << "\t" <<  "Degree of page reached 0. Queue now looking like: ";
				for(size_t i = 0; i < queue.size(); ++i){
					debug_file << unique_pages[queue[i]] << " ";
				}
				debug_file << endl;
			}
		}

		queue_index = queue_index + 1; // Going to the next page in the queue

		// DEUBG
		debug_file << endl;

	}

	return final_order;

}

// -----------------------------------------------

bool compare_pages(const int &page_a, const int &page_b, const std::vector<Rule> &update_rules){

	// This function evaluates if, given two pages, a and b, a should come before b
	// a should come before b if a rule a|b exists -> Returns true in this case
	// a should not come before b if a rule b|a exists -> Returns false in thise case
	// By default, returns false

	for(const Rule& rule : update_rules){

		// We look at all the rules until we find an instance of a rule a|b or a rule b|a, returning the appropriate boolean in those cases

		if(rule.x == page_a && rule.y == page_b) return true; // If X = a and Y = b, this means an a|b rule exists. a can come before b
		if(rule.x == page_b && rule.y == page_a) return false; // If X = b and Y = a, this means an b|a rule exists. a cannot come before b

	}

	return false;

}

// -----------------------------------------------

int partition_update(std::vector<int> &vec, int low, int high, const std::vector<Rule> &rules){

	// Adapted quicksort - given rules vector. This is not a simple ascending order quicksort
	// vec is an array of pages
	// low, high, i and j are indices of theses pages with respect to vec. Not their unique index, with respect to unique_pages

	int pivot = vec[high]; // We chose the largest element of the vector as the pivot; We want to find where the pivot belongs in the vector
	int i = (low - 1); // This is the boundary, the index of the elements that must come before the pivot. It starts as an index where no left element exists

	for(int j = low; j <= high - 1; ++j){ // We go through all elements, from the left-most to the one right before the pivot

		// Here, in a normal quicksort, you would see if vec[j] < pivot
		// But, with compare_pages, we can use the rules set by the input file as the correct order rules
		// Another way to do this, that would match the theory I wrote at the end of this file would be to:
		// - Sort the degrees of pages instead of the pages themselves. We would sort the degrees in ascending order, rule independent
		// - Then, we would put the corresponding pages to each degree, and that would give us the correct order
		if( compare_pages(vec[j], pivot, rules) ){ // We ask - does the page we are seeing right now come before pivot according to the rules?
			
			i = i+1; // If it does, then we advance the boundary

			// And we swap the elements before and after the boundary to be in the correct, rules approved, order
			int temp = vec[i];
			vec[i] = vec[j];
			vec[j] = temp;

			// Again, without rules, this would be a simple ascending or descending order comparision of elements in the if, depending on our sorting goal

		}
	}

	// All elements smaller than the pivot are to its left and all elements larger than it are to its right. We insert the pivot in the middle of these regions, as it should be in
	int temp = vec[i+1];
	vec[i+1] = vec[high];
	vec[high] = temp;

	return (i+1); // This is the position of the pivot

}

// -----------------------------------------------

void quicksort_update(std::vector<int> &vec, int low, int high, const std::vector<Rule> &rules){

	if(low < high){ // The function will stop calling itself once the sorting hits single elements, where low = high and the if argument returns false

		int pi = partition_update(vec, low, high, rules); // Returns the position of the split element

		quicksort_update(vec, low, pi-1, rules); // Orders the sub vector made until the split element, its left region
		quicksort_update(vec, pi, high, rules); // Orders the sub vector made from the split element forward, its right region

	}

}

// -----------------------------------------------

void correct_update(std::vector<int> &update, const int &update_size, const std::vector<Rule> &update_rules){

	quicksort_update(update, 0, update_size - 1, update_rules);

}

// -----------------------------------------------

int go_over_updates(std::ofstream &debug_file, std::ifstream &input_file, const std::vector<Rule> &all_rules, const int &n_rules){

	std::string file_line, line_seg;

	// --- --- ---

	int summ = 0; // Initialize cumulative sum of middle page numbers
	int summ_of_incorrects = 0; // Initialize cumulative sum of middle page numbers for the incorrect orders that were then corrected
	int i_update = 1; // Update number (not a vector index)

	// Go over each update
	while( std::getline(input_file, file_line) ){

		std::vector<int> update; // Array that stores the page numbers of the current update
		std::stringstream line_stream(file_line);

		while( std::getline(line_stream, line_seg, ',') ){ // Puts the page numbers in the order they appear in, into the update array
			update.push_back( std::stoi(line_seg) );
		}

		int update_size = update.size(); // Length of the update array

		// DEBUG
		debug_file << "Update number " << i_update << " :: ";
		for(int i_page = 0; i_page < update_size; ++i_page){
			debug_file << update[i_page] << " ";
		}
		debug_file << endl;

		// Let's start by building a vector with all unique page numbers in all_rules
		// This will also give each page number a page index (the index of their entry into this vector)
		// Let's call this vector unique_pages
		std::vector<int> unique_pages = get_unique_pages(all_rules);
		int n_unique_pages = unique_pages.size();

		// Now we need to get the subset of rules that involve only the pages in the update
		std::vector<Rule> update_rules = get_rules_subset(debug_file, all_rules, n_rules, unique_pages, n_unique_pages, update_size, update);

		// DEBUG
		debug_file << "Applicabale rules for this update: ";
		int n_update_rules = update_rules.size();
		for(int i_rule = 0; i_rule < n_update_rules; ++i_rule){
			debug_file << update_rules[i_rule].x << "|" << update_rules[i_rule].y << " ; ";
		}
		debug_file << endl;
		debug_file << "In total, there are " << n_update_rules << " in the subset of rules for this update (out of a total of " << n_rules << " rules)" << endl;
		std::cout << "Subset of rules for update " << i_update << " have been computed" << endl;

		// Having the subset of rules that matter for this update, we now build the page order set by these rules

		std::vector<int> page_degrees(n_unique_pages, 0); // Initialize all page degrees to 0
		std::vector<std::vector<int>> page_graph(n_unique_pages);
		page_graph = get_rules_graph(debug_file, unique_pages, n_unique_pages, update_rules, n_update_rules, page_degrees); // page_degrees is output

		// DEBUG
		debug_file << "Here are the degrees and dependent pages of each page number in the update:" << endl;
		debug_file << endl;
		for(int i_page = 0; i_page < update_size; ++i_page){

			int update_page = update[i_page];
			int update_page_index = get_page_index(unique_pages, n_unique_pages, update_page);

			debug_file << "\t" << "Page " << update_page << " | Degree = " << page_degrees[update_page_index] << " | Dependent pages: ";

			for(size_t i_page_y = 0; i_page_y < page_graph[update_page_index].size(); ++i_page_y){
				int dependent_page_index = page_graph[update_page_index][i_page_y];
				debug_file << unique_pages[dependent_page_index] << " ";
			}
			debug_file << endl;
		}
		debug_file << endl;
		std::cout << "Page graph and degrees for update " << i_update << " have been computed" << endl;

		// We can now get the order in which the pages in the update should appear in
		std::vector<int> final_order = get_final_order(debug_file, update, unique_pages, n_unique_pages, page_degrees, page_graph);
		int n_final_order = final_order.size();

		bool is_correct = true; // Assume update to be correctly ordered, given the rules subset

		// Now analyze update and see if it is in the right order
		for(int i_page = 0; i_page < update_size-1; ++i_page){ // update_size-1 because we compare with the i_page+1 page

			// We use the get_page_index function to get the indices of the current and next update pages in the final_order vector

			int current_page = update[i_page];
			int current_page_final_order_index = get_page_index(final_order, n_final_order, current_page);

			int next_page = update[i_page+1];
			int next_page_final_order_index = get_page_index(final_order, n_final_order, next_page);

			// If the current page index is larger than the next page index, the pages are not correctly ordered

			if( current_page_final_order_index > next_page_final_order_index ){

				is_correct = false;

				// DEBUG
				debug_file << "Update page order is incorrect | Page " << current_page << " appears before page " << next_page << " when it should be the other way around" << endl;
				
				break;
			}

		}

		// If all the pages are ordered correctly, we put the middle page number in the cumulative sum
		if( is_correct ){

			int mid_update_page_index = update_size/2; // Automatically rounds down if update_size is odd, which is what we want for 0-based indexing

			summ = summ + update[mid_update_page_index]; // Put middle page number into cumulative sum

			// DEBUG
			debug_file << "All update pages are correctly ordered. Middle page nubmer = " << update[mid_update_page_index] << " | Sum now equals " << summ << endl;

		} else {

			// Update is in incorrect order. We need to correct it, find the middle page number and put it into its own cumulative sum
			correct_update(update, update_size, update_rules); // Modifies update - puts it into its correct order

			int mid_update_page_index = update_size/2; // Automatically rounds down if update_size is odd, which is what we want for 0-based indexing

			summ_of_incorrects = summ_of_incorrects + update[mid_update_page_index]; // Put middle page number into cumulative sum

			debug_file << "Update has been correct. Middle page nubmer = " << update[mid_update_page_index] << " | Sum now equals " << summ << endl;

		}

		i_update = i_update + 1; // Go to the next update

		// DEBUG
		debug_file << endl;
		debug_file << "-------------------------------------------------------------" << endl;
		std::cout << "-------------------------------------------------------------" << endl;
		debug_file << endl;

	}

	std::cout << "Total number of updates = " << i_update-1 << endl;
	debug_file << "Total number of updates = " << i_update-1 << endl;

	std::cout << "Sum of the corrected incorrectly ordered updates = " << summ_of_incorrects << endl;
	debug_file << "Sum of the corrected incorrectly ordered updates = " << summ_of_incorrects << endl;

	return summ;

}

// -----------------------------------------------

void summ_mid_pages_correct_updates(std::ofstream &debug_file, const std::string &filepath, const std::string &filename){

	// Opening input file
	std::ifstream input_file;
	input_file.open(filepath + filename);

	// Array to store all rules
	std::vector<Rule> all_rules = get_rules(input_file); // input_file is left at the empty line
	int n_rules = all_rules.size();

	// DEBUG
	debug_file << "The file contains " << n_rules << " rules" << endl;
	debug_file << endl;
	std::cout << "The file contains " << n_rules << " rules" << endl;

	// Go over updates. The aim is to:
	// - Select an update and put its pages into a vector
	// - Go over the rules that involve only the pages in this array
	// - Build the rules order for these pages
	// - Verify is the update has the pages in a correct or incorrect order
	// - Get the middle page number and put it into a collective sum if the order is correct
	int summ = go_over_updates(debug_file, input_file, all_rules, n_rules); // Resumes from where input_file was left at

	// DEBUG
	debug_file << "Final middle page sum for correct updates = " << summ << endl;
	std::cout << "Final middle page sum for correct updates = " << summ << endl;

	/*
	Something I noticed, the difference between correctly and not correctly ordered updates.
	Check out this example input file output:

	--> Update number 1:

	Here are the degrees and dependent pages of each page number in the update:

	Page 75 | Degree = 0 | Dependent pages: 29 53 47 61 
	Page 47 | Degree = 1 | Dependent pages: 53 61 29 
	Page 61 | Degree = 2 | Dependent pages: 53 29 
	Page 53 | Degree = 3 | Dependent pages: 29 
	Page 29 | Degree = 4 | Dependent pages: 

	--> Update number 2:

	Here are the degrees and dependent pages of each page number in the update:

	Page 97 | Degree = 0 | Dependent pages: 13 61 29 53 
	Page 61 | Degree = 1 | Dependent pages: 13 53 29 
	Page 53 | Degree = 2 | Dependent pages: 29 13 
	Page 29 | Degree = 3 | Dependent pages: 13 
	Page 13 | Degree = 4 | Dependent pages: 

	--> Update number 5:

	Here are the degrees and dependent pages of each page number in the update:

	Page 61 | Degree = 0 | Dependent pages: 13 29 
	Page 13 | Degree = 2 | Dependent pages: 
	Page 29 | Degree = 1 | Dependent pages: 13 

	--> Update number 6:

	Here are the degrees and dependent pages of each page number in the update:

	Page 97 | Degree = 0 | Dependent pages: 13 47 29 75 
	Page 13 | Degree = 4 | Dependent pages: 
	Page 75 | Degree = 1 | Dependent pages: 29 47 13 
	Page 29 | Degree = 3 | Dependent pages: 13 
	Page 47 | Degree = 2 | Dependent pages: 13 29 

	Even before making the final order, we can already see that:
	- Correctly ordered updates already come with their pages in degree ascending order and dependent pages descending ordered
	- Incorrectly ordered updates do not have these orders in such a way
	Maybe to correct the incorrect updates, one needs only to sort the pages in ascending order of degree
	Doing so automatically puts them in descending order for dependent pages. They are correlated.
	In the same way, maybe one doesn't even need to build the final order with the queue.
	One can just check the degree ascending order. If the pages are in it, they are correctly ordered. If the pages or not in it, they are incorrectly ordered.
	*/

}
