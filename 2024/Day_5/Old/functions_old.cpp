#include "functions_old.hpp"
#include <iostream>
#include <sstream>
using namespace std;

// -----------------------------------------------

std::vector<Rule> get_rules(std::ifstream &input_file){

	size_t rule_break_pos;
	int X_page, Y_page;

	std::string file_line, X_str, Y_str;
	std::vector<Rule> all_rules;

	// --- --- ---

	while( std::getline(input_file, file_line) ){

		if(file_line.empty()){ // LEave loop once we get to the empty line
			break;
		}

		// Line is X|Y
		// Need to get X and Y from string file_line. For this:
		// - Treat string as an array of characters
		// - Find position of |
		// - Get X and Y from this position

		rule_break_pos = file_line.find('|');

		X_str = file_line.substr(0, rule_break_pos);
		X_page = std::stoi(X_str);

		Y_str = file_line.substr(rule_break_pos + 1);
		Y_page = std::stoi(Y_str);

		all_rules.push_back({X_page, Y_page});

	}

	return all_rules;

}

// -----------------------------------------------

bool contains_value(const std::vector<int> &vec, const int &value){

	// If we see value inside vec, even once, then it has it
	for( int val : vec ){ if( val == value ) return true; }

	return false; // By default, we say that value is not inside vec
}

// -----------------------------------------------

int get_page_index(const std::vector<int> &unique_vec, const int &N, const int &value){

	// We are assuming it unique_vec has only unique elements inside
	for(int index = 0; index < N; ++index){
		if( unique_vec[index] == value ){
			return index;
		}
	}

	return -1; // By default, returns a negative number, indicating the value is not in unique_vec
}

// -----------------------------------------------

std::vector<int> get_unique_pages(const std::vector<Rule> &all_rules){

	int rule_x, rule_y;

	std::vector<int> pages;

	// --- --- ---

	for(const Rule &rule : all_rules){
		
		rule_x = rule.x;
		rule_y = rule.y;

		// If we see that rule_x and rule_y are not yet present in pages, we add them

		if( !contains_value(pages, rule_x) ) pages.push_back(rule_x); // If X page number in rule is not in pages yet, add it
		if( !contains_value(pages, rule_y) ) pages.push_back(rule_y); // If Y page number in rule is not in pages yet, add it

	}

	return pages;

}

// -----------------------------------------------

std::vector<std::vector<int>> get_pages_graph(std::ofstream &debug_file, const int &n_pages, const std::vector<Rule> &all_rules, const int &n_rules, const std::vector<int> &pages, std::vector<int> &degrees){

	int x_page, y_page, x_page_index, y_page_index, dependent_page_index;

	std::vector<std::vector<int>> graph(n_pages);

	// --- --- ---

	debug_file << "Building the pages graph:" << endl;
	debug_file << endl;

	for(const Rule &rule : all_rules){

		x_page = rule.x; // X page number in X|Y
		y_page = rule.y; // Y page number in X|Y

		debug_file << ">> Rule: " << x_page << " | " << y_page << endl;

		x_page_index = get_page_index(pages, n_pages, x_page); // Index of X page number in the array of all unique pages. This index uniquely identifies X page number
		y_page_index = get_page_index(pages, n_pages, y_page); // Index of Y page number in the array of all unique pages. This index uniquely identifies Y page number

		debug_file << ">> Page number X (" << x_page << ") has pages index = " << x_page_index << endl;
		debug_file << ">> Page number Y (" << y_page << ") has pages index = " << y_page_index << endl;

		graph[x_page_index].push_back(y_page_index); // Make the X -> Y connection

		debug_file << ">> Make X -> Y connection | Dependent pages of X are now: ";

		for( size_t i = 0; i < graph[x_page_index].size(); ++i){
			dependent_page_index = graph[x_page_index][i];
			debug_file << pages[dependent_page_index] << " ";
		}
		debug_file << endl;

		debug_file << "Degree of page number X (" << x_page << ") = " << degrees[x_page_index] << endl;

		degrees[y_page_index] = degrees[y_page_index] + 1;

		debug_file << "Degree of page number Y (" << y_page << ") = " << degrees[y_page_index] << endl;

		debug_file << endl;

	}

	return graph;

}

// -----------------------------------------------

std::vector<int> get_final_order(std::ofstream &debug_file, const std::vector<int> &pages, const int &n_pages, std::vector<int> &degrees, const std::vector<std::vector<int>> &graph){

	std::vector<int> final_order, queue;

	// --- --- ---

	// Putting all pages with degree = 0 into the printing queue

	debug_file << "Putting degree = 0 pages into the printing queue" << endl;

	for(int i = 0; i < n_pages; ++i){
		debug_file << "(*) Page index " << i << ", corresponding to page number " << pages[i] << ", has degree = " << degrees[i] << " | ";
		if(degrees[i] == 0){
			queue.push_back(i); // queue stores page indices, not the page numbers themselves. One can easily obtain those with the pages array
			debug_file << "Degree is zero. Page added to queue. Size of queue now equals " << queue.size() << endl;
		} else {
			debug_file << "Degree is not zero. Page not added to queue. Going to next page" << endl;
		}
	}

	debug_file << endl;

	// Go over queue, print page first in line, check its dependent pages, decrease their degrees by 1, if a page's degree reaches 0, add it to the back of the queue

	debug_file << "Starting build of master printing order" << endl;
	debug_file << endl;

	size_t queue_index = 0;
	int page_index_in_queue, dependent_page_index;

	while(queue_index < queue.size()){

		page_index_in_queue = queue[queue_index];

		debug_file << "(*) Looking at first page in queue: Page index " << page_index_in_queue << ", corresponding to page number " << pages[page_index_in_queue] << endl;

		final_order.push_back(pages[page_index_in_queue]); // We put the actual page numbers in final_order

		debug_file << "(*) Page is printed. Master order currently looking like: ";

		for(size_t i = 0; i < final_order.size(); ++i){
			debug_file << final_order[i] << "";
		}
		debug_file << endl;

		debug_file << "(*) Go to the dependent pages and decrease their degrees" << endl;

		for(size_t i_y = 0; i_y < graph[page_index_in_queue].size(); ++i_y){

			dependent_page_index = graph[page_index_in_queue][i_y];
			debug_file << "(*) >> Dependent page number " << pages[dependent_page_index] << " (page index " << dependent_page_index << ") | Degree = " << degrees[dependent_page_index] << endl;
			
			degrees[dependent_page_index] = degrees[dependent_page_index] - 1;
			debug_file << "(*) >> Degree is reduced by 1, now equaling " << degrees[dependent_page_index] << endl;

			if(degrees[dependent_page_index] == 0){

				queue.push_back(dependent_page_index);

				debug_file << "(*) >> Degree reached zero. Queue now looking like: ";
				for(size_t i = 0; i < queue.size(); ++i){
					debug_file << pages[queue[i]] << " ";
				}
				debug_file << endl;
			}
		}

		queue_index = queue_index + 1; // Going to the next page in the queue

		debug_file << endl;

	}

	return final_order;

}

// -----------------------------------------------

std::vector<int> ordering_rules(std::ofstream &debug_file, const std::vector<Rule> &all_rules, const int &n_rules){

	int n_pages, n_final_order, dependent_page_index;

	std::vector<int> pages, final_order;

	// --- --- ---
	
	// Need to find all unique page numbers. We'll also find how many page numbers appear as X and how many as Y, in total that is
	// pages will be a vector with all unique page numbers. Unordered, but that's fine. It serves the purpose of being the repository with all unique page numbers

	pages = get_unique_pages(all_rules);
	n_pages = pages.size();

	// DEBUG
	debug_file << "There are " << n_pages << " unique page numbers: ";
	for(const int &page : pages){
		debug_file << page << " ";
	}
	debug_file << endl;
	debug_file << endl;
	std::cout << "There are " << n_pages << " unique page numbers" << endl;

	// Let's think about printing the pages in terms of how soon they can be printed
	// A page that only appears as X can be printed as soon as it wants
	// A page that appears as Y in an X|Y rule is conditioned by X to be printed first before itself can be printed
	// If a page appears multiple times as X, doesn't change how soon it can be printed
	// If a page appears multiple times as Y, it does affect how soo it can be printed, because it's conditioned by various other pages
	// Let's define a page's degrees as an integer that quantifies how soon it can be printed:
	// - degrees = 0: This means the page has only appeared as X
	// - degrees = 1: This means the page requires 1 other page to be printed first before it can be printed
	// - degrees = 2: This means the page requires 2 other pages to be printed first before it can be printed
	// - etc ...
	// This means, the number of times a page appears as Y is the number of pages it needs to wait for before it can be printed

	// We have a pages array that contains all unique pages that exist
	// The indices of these pages therefore function as unique labels to these pages
	// All rules show up as X|Y. But now I want summaries of these rules; all the Ys a given X has, for all unique Xs
	// We can do this by using these indices. Make an array with the same size as pages. Then, the element of an index will be an array of all the indices it rules over.
	// Let's call this new variable, std::vector<std::vector<int>> graph. graph[i] contains the array of all Ys that pages[i] rules over as X
	// If pages[i] is never an X, then graph[i] will just be an empty array

	int page_index;
	std::vector<std::vector<int>> graph(n_pages);
	std::vector<int> degrees(n_pages, 0);

	graph = get_pages_graph(debug_file, n_pages, all_rules, n_rules, pages, degrees);

	// DEBUG
	debug_file << "Here are the degrees and dependent pages of each page number:" << endl;
	for(const int &page : pages){
		page_index = get_page_index(pages, n_pages, page);
		debug_file << "(*) Page number " << page << ", corresponding to page index " << page_index << " has degree " << degrees[page_index] << " and dependent pages: ";
		for(size_t i = 0; i < graph[page_index].size(); ++i){
			dependent_page_index = graph[page_index][i];
			debug_file << pages[dependent_page_index] << " ";
		}
		debug_file << endl;
	}
	debug_file << endl;
	std::cout << "Degrees and dependent pages have been computed" << endl;

	// Now, we make a queue of all pages with degrees equal to 0. These are the pages ready to be printed
	// When one of these pages is printed, the pages it was pointing to (its Y pages, given by graphs) will have their degrees decreased by 1
	// The pages whose degrees reach 0, get added to the queue
	// This process will repeat until the queue is empty, meaning no pages are left to print

	final_order = get_final_order(debug_file, pages, n_pages, degrees, graph);

	// DEBUG
	debug_file << "The final master order of pages is: ";
	std::cout << "The final master order of pages is: ";

	n_final_order = final_order.size();

	for(int i = 0; i < n_final_order; ++i){
		debug_file << final_order[i] << " ";
		std::cout << final_order[i] << " ";
	}
	debug_file << endl;
	debug_file << endl;
	std::cout << endl;

	return final_order;

}

// -----------------------------------------------

int go_over_updates(std::ofstream &debug_file, std::ifstream &input_file, const std::vector<int> final_order, const int &n_final){

	int i_update, summ, update_size, current_pos, next_pos, mid_index;

	std::string file_line;
	std::string line_seg;

	bool is_correct;

	// --- --- ---

	i_update = 1;

	summ = 0; // Initialize middle page summ

	while( std::getline(input_file, file_line) ){

		debug_file << "Update number " << i_update << " :: "+file_line << endl;

		std::vector<int> current_update; // Needs to be defined inside the while loop, to be reset after each
		std::stringstream line_stream(file_line);

		while( std::getline(line_stream, line_seg, ',') ){
			current_update.push_back( std::stoi(line_seg) ); // Array with the pages in the update as entries
		}

		is_correct = true; // By default, update is assumed to be correctly ordered

		update_size = current_update.size();

		for(int i = 0; i < update_size-1; ++i){ // update_size-1 because in each loop iteration we compare i with i+1

			current_pos = get_page_index(final_order, n_final, current_update[i]);
			next_pos = get_page_index(final_order, n_final, current_update[i+1]);

			if( current_pos > next_pos ){ // If the index of the current page comes after the index of the next page, as determined by final_order, then the order of the update is incorrect
				is_correct = false;
				debug_file << ">> Incorrect order detected. Current page = " << current_update[i] << " with final_order index = " << current_pos << " | Next page = " << current_update[i+1] << " with final_order index = " << next_pos << endl;
				debug_file << ">> This update is incorrect. Moving on to the next one" << endl;
				break;
			}

		}

		if(is_correct){ // If the update is correct

			debug_file << ">> No incorrect order detected. Update is considered correct. Finding middle page number" << endl;

			mid_index = update_size/2; // Automatically rounds down if update_size is odd, which is what we want for 0-based index arrays

			summ = summ + current_update[mid_index];

			debug_file << ">> Found to be page number " << current_update[mid_index] << " | Updating summ = " << summ << endl;

		}

		i_update = i_update + 1; // Go to next update

		debug_file << endl;

	}

	return summ;

}

// -----------------------------------------------

void summ_mid_pages_correct_updates(std::ofstream &debug_file, const std::string &filepath, const std::string &filename){

	/*
	Read the file. It has two parts

	(*) Part I is a series of X|Y lines. Store them as pairs. Will need to summarize them
	For example, if you have
	X|Y
	X|Z
	Y|Z
	>> Part I.1 is to summarize all rules for the same page.
	This means X needs to be printed before Y and Z. This is the summary of rules for X.
	There is only one rule for Y: Y to come before Z. This is the summary of rules for Y.
	>> Part I.2 is to figure out the correct order from the summaries.
	In this example, if X comes before Y and Z, and Y comes before Z, then...
	...the correct order is: X, then Y, then Z

	(*) Part II is an empty line that separates the ordering rules and the updates

	(*) Part III are the lines of updates. These are page numberes separated by commas.
	Each line is an update. The order in which they appear in will be the order to evaluate.

	We will not be storing the updates. They will be evaluated as they are read, to save memory.
	Regarding the rules, only the final correct order matters.
	Then, for a given update, we find the integers in the final order and see if they all go as intended.
	*/

	int n_rules, summ, n_final;

	std::ifstream input_file;
	std::vector<int> final_order;
	std::vector<Rule> all_rules;

	// --- --- ---

	// Opening input file
	input_file.open(filepath + filename);

	// Getting rules
	all_rules = get_rules(input_file); // input_file is left at the empty line
	n_rules = all_rules.size();

	debug_file << "The file contains " << n_rules << " rules" << endl;
	debug_file << endl;
	std::cout << "The file contains " << n_rules << " rules" << endl;

	// --- --- ---

	// Ordering the rules
	final_order = ordering_rules(debug_file, all_rules, n_rules);
	n_final = final_order.size();

	// --- --- ---

	// Go over updates
	summ = go_over_updates(debug_file, input_file, final_order, n_final); // Resumes from where input_file was left at

	debug_file << "Final middle page sum for correct updates = " << summ << endl;
	std::cout << "Final middle page sum for correct updates = " << summ << endl;

}

// -----------------------------------------------
