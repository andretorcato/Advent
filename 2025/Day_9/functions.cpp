#include "functions.hpp"
#include <iostream>
using namespace std;

// -----------------------------------------------

std::vector<Point> get_red_tiles(std::ofstream &debug_file, std::ifstream &input_file, int &n_tiles){

	std::string line;
	std::vector<Point> red_tiles;

	debug_file << "Red tiles locations:" << endl;

	while(std::getline(input_file, line)){

		if(line.empty()){
			break;
		}

		size_t comma_pos = line.find(',');

		long long red_tile_x = stoll( line.substr(0, comma_pos) );
		long long red_tile_y = stoll( line.substr(comma_pos+1) );

		red_tiles.push_back({red_tile_x, red_tile_y});

	}

	int i_tile = 1;
	for(const Point &red_tile_location : red_tiles){
		debug_file << "(*) Tile number " << i_tile << " | x = " << red_tile_location.x << " | y = " << red_tile_location.y << endl;
		i_tile = i_tile + 1;
	}
	n_tiles = i_tile - 1;
	debug_file << endl;

	return red_tiles;

}

// -----------------------------------------------

long long cross_product(const Point &O, const Point& A, const Point &B){

	/*
	This function evaluates the cross product between 3 Points. The purpose is to get a rotation sign. If result is:
	- > 0  :: Counter-clockwise | Left turn | Right hand rule, thumb pointing towards you
	- == 0 :: No rotation | Collinear
	- < 0  :: Clockwise | Right turn | Right hand rule, thumb pointing away from you
	*/

	return ( A.x - O.x )*( B.y - O.y ) - ( A.y - O.y )*( B.x - O.x );

}

// -----------------------------------------------

std::vector<Point> get_convex_hull(std::vector<Point> &red_tiles, const size_t &n_tiles){

	std::vector<Point> hull;

	// It requires sorting of red_tiles. I implemented a quicksort, to train how to do it
	// The sorting is based on the custom < operator built into the Point struct. Occurs automatically when comparing two Point variables
	quicksort(red_tiles, 0, n_tiles-1); // The actual start and end indices are passed as arguments

	for(size_t i = 0; i < n_tiles; ++i){

		/*
		The while evaluates two things:
		- The number of elements in the hull has to be at least 2, other wise the cross product cannot be done
		- cross_product(hull[hull.size()-2], hull.back(), red_tiles[i]):
		- (*) hull[hull.size()-2] is the 2nd-to-last Point inside hull
		- (*) hull.back() is the last Point inside hull
		- (*) red_tiles[i] is the candidatewe are trying to add to hull
		The cross product analyzes if the rotation direction to get to red_tiles[i] was the same than the direction to get from the 2nd-to-last point ot the last point of the hall
		If the cross product is positive, this is a good sign, it means the boundary of the amourphous shape created by the points is following the same direction, meaning this point should be part of the hull
		If the cross product is negative or zero however, that means hull.back() would actually be inside the shape made between the other two points, given the new direction
		Therefore, this point should be remove from hull (hull.pop_back()) since it no longer belongs to the boundary imaginary line of the point cloud
		*/

		while( hull.size() >= 2 && cross_product(hull[hull.size()-2], hull.back(), red_tiles[i]) <=0 ){
			hull.pop_back(); // Removes last element of hull
		}

		hull.push_back(red_tiles[i]); // By default, we add every point to the hull, and with the while, we remove those that turn out to not be part of the imaginary boundary

	}

	/*
	The hull just built is actually incomplete. We call it the lower hull
	This is because it's only concerned with the x coordinate, due to how the sorting of the Points worked (x was priority, then y)
	So we need to do another pass through the points, the upper hull, which will connect back to the lower hull, completing it
	Closing the imaginary point cloud boundary
	
	This is the trait of the Monotone Chain algorithm. There is another algorithm, called Graham Scan, which only requires one pass.
	But its sorting is different, since it relies on sorting by angles. This requires floating-point calculations.
	*/

	size_t lower_hull_size = hull.size();

	int upper_hull_start_point = n_tiles-2;

	for(int i = upper_hull_start_point; i >=0; --i){ // i cannot be size_t here, because it is unsigned. When it reaches 0 and --i, it would wrap around to being the largest integer possible that is not long nor long long, causing an infinite loop, and then, a seg fault

		while( hull.size() > lower_hull_size && cross_product(hull[hull.size()-2], hull.back(), red_tiles[i]) <= 0 ){
			hull.pop_back();
		}

		hull.push_back(red_tiles[i]);

	}

	// The starting point was counted twice due to the second pass, so we remove it
	hull.pop_back();

	return hull;

}

// -----------------------------------------------

/*
Quicksort strategy (PPR):
- Pivot :: Pick an element of the array (we will start with the last element)
- Partition :: Reorder the array so that:
- (*) All elements smaller than the pivot are to its left
- (*) All elements larger than the pivot are to its right
- Recurse: Apply the same steps to the sub-arrays made from the elements left of the pivot and the elements right of the pivot
*/

int partition(std::vector<Point> &arr, int low, int high){

	// Make the pivot start as the last element of the array
	Point pivot = arr[high];

	// Index of the boundary, between smaller and larger elements compared to the pivot
	// At the end, i will be the index of the pivot, after it has been correctly determined
	int i = low - 1; // Cannot be size_t because that is unsignes, and when low = 0, i would be max possible integer instead of -1

	Point tmp;

	// Go through all elements of array and compare them to pivot
	for( int j = low; j < high; j++ ){

		// If arr[j] is smaller than pivot, we advance the boundary of the two regions and swap this element with the pivot
		// In this case, the elements of the array are our Point, so the < operator is our custom one
		if( arr[j] < pivot ){

			// Boundary advancement
			i = i + 1;

			// Swap arr[i] (current pivot) with arr[j] (element smaller than the pivot)
			tmp = arr[i];
			arr[i] = arr[j];
			arr[j] = tmp;

		}
	}

	// Insert the pivot in between the two regions, with a swap
	int pivot_index = i+1;
	tmp = arr[pivot_index];
	arr[pivot_index] = arr[high];
	arr[high] = tmp;

	return pivot_index;

}

void quicksort(std::vector<Point> &arr, int low, int high){

	if(low < high){ // Recursive call of quicksort stops when low == high, meaning we cannot divide arr into sub arrays any further

		int pivot_index = partition(arr, low, high); // Obtains pivot index. arr now has all elements smaller than it to its left and all elements larger than it to its right

		quicksort(arr, low, pivot_index - 1); // Recursive call to sort the subarray of all elements left of the pivot

		quicksort(arr, pivot_index + 1, high); // Recursive call to sort the subarray of all elements right of the pivor

	}

}

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

// -----------------------------------------------

void main_function(std::ofstream &debug_file, const std::string &filepath, const std::string &filename){

	// Opening input file
	std::ifstream input_file;
	input_file.open(filepath + filename);

	// Getting red tiles
	int n_tiles;
	std::vector<Point> red_tiles = get_red_tiles(debug_file, input_file, n_tiles);

	// Testing Point operators that were created
	/*size_t n = 4;
	Point *point_arr = new Point[n];
	point_arr[0] = {2, 4};
	point_arr[1] = {3, 3};
	point_arr[2] = {3, 4};
	point_arr[3] = {4, 3};
	debug_file << "Testing custom Point operators" << endl;
	for(size_t i = 0; i < n-1; i++){
		for(size_t j = i+1; j < n; j++){
			bool larger_eval = point_arr[i] > point_arr[j];
			debug_file << "[*] (" << point_arr[i].x << ", " << point_arr[i].y << ") > (" << point_arr[j].x << ", " << point_arr[j].y << ") = " << larger_eval << endl;
			bool smaller_eval = point_arr[i] < point_arr[j];
			debug_file << "[*] (" << point_arr[i].x << ", " << point_arr[i].y << ") < (" << point_arr[j].x << ", " << point_arr[j].y << ") = " << smaller_eval << endl;
		}
	}
	debug_file << endl;
	delete[] point_arr;*/

	// Get convex hull vector
	std::vector<Point> hull = get_convex_hull(red_tiles, n_tiles);
	size_t n_hull_points = hull.size();

	debug_file << "The imaginary boundary of the point cloud is made from:" << endl;
	for(const Point &hull_point : hull){
		debug_file << "(*) Point (" << hull_point.x << ", " << hull_point.y << ")" << endl;
	}
	debug_file << endl;

	// Calculating rectangle areas and finding the largest one
	long long max_area = 0;

	for(size_t i_tile = 0; i_tile < n_hull_points; ++i_tile){
		for(size_t j_tile = 0; j_tile < n_hull_points; ++j_tile){

			// Width is x length
			long long width = std::abs( hull[i_tile].x - hull[j_tile].x ) + 1; // The difference doesnt count one of the points by mathematical default, so we add it

			// Height is y length
			long long height = std::abs( hull[i_tile].y - hull[j_tile].y ) + 1;

			long long area = width*height;

			if(area > max_area){
				max_area = area;
			}

		}
	}

	std::cout << max_area << endl;

}
