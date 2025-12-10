#ifndef functions
#define functions

#include<fstream>
#include<string>
#include<sstream>
#include<vector>

struct Point{

	long long x;
	long long y;

	/*
	Comparision operator implementation. Compares Point with another Point (called other) to see if they are larger or smaller
	- First, compares x
	- Or, if x are the same, compares y
	*/

	bool operator < (const Point &other) const { // Is Point less than other?
		return x < other.x || (x == other.x && y < other.y);
	}

	bool operator > (const Point &other) const { // Is Point greater than other?
		return x > other.x || (x == other.x && y > other.y);
	}
};

// -------------------------------------------------

std::vector<Point> get_red_tiles(std::ofstream &debug_file, std::ifstream &input_file, int &n_tiles);

long long cross_product(const Point &O, const Point& A, const Point &B);

int partition(std::vector<Point> &arr, int low, int high);

void quicksort(std::vector<Point> &arr, int low, int high);

std::vector<Point> get_convex_hull(std::vector<Point> &red_tiles, const size_t &n_tiles);

void main_function(std::ofstream &debug_file, const std::string &filepath, const std::string &filename);

#endif
