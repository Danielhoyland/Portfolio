#include <array>
#include <glm/glm.hpp>
namespace GeometricTools {
	
	/* Basic Shapes */
	constexpr std::array<float, 3 * 2> UnitTriangle2D = { -0.5f, -0.5f, 0.5f, -0.5f, 0.0f, 0.5f }; // [2,3]
	constexpr std::array<float, 4 * 2> UnitSquare2D = { -0.5f, 0.5f, 0.5f, 0.5f, -0.5f, -0.5f, 0.5f, -0.5f }; // [2,4]

	/* Grid Coordinates Gen */
	inline float* UnitGridGeometry2D(int seg) {
		int size = seg + 1; // amount of grid lines
		float* grid = new float[(size * size) * 2]; // the data structure for the coords
		float width = (1.0f / (seg)); // width of one tile
		int index = 0;
		// generating an x and y for each point in the grid
		for (int i = 0; i < size; i++) {
			float y = (i * width) - 0.5f;
			for (int j = 0; j < size; j++) {
				float x = (j * width) - 0.5f;
				grid[index] = x;
				index++;
				grid[index] = y;
				index++;
			}
		}
		return grid;
	}
	/* Grid Coordinates Gen with texture coordinates*/
	inline float* UnitGridGeometry2DTX(int seg) {
		int size = seg + 1; // amount of grid lines
		float* grid = new float[(size * size) * 4]; // the data structure for the coords
		float width = (1.0f / (seg)); // width of one tile
		int index = 0; 
		// generating an x and y for each point in the grid 
		// also texture coordinates
		for (int i = 0; i < size; i++) {
			float y = (i * width) - 0.5f;
			for (int j = 0; j < size; j++) {
				float x = (j * width) - 0.5f;
				// vertice coordinates
				grid[index] = x;
				index++;
				grid[index] = y;
				index++;
				// texture coordinates
				grid[index] = j * width;
				index++;
				grid[index] = i * width;
				index++;
			}
		}
		// print coords for testing
		/*
		std::cout << "__________" << std::endl;
		for (int i = 0; i < ((size * size) * 4); i++) {
				std::cout << grid[i] << " ";
		}
		std::cout << std::endl;
		*/
		return grid;
	}

	//    0---1   size = 1
	//    /   /
	//    2---3   indice = 2

	//    0---1---2
	//    /   /   /   size = 2
	//    3---4---5
	//    /   /   /   indice = 3
	//    6---7---8

	/* Grid Element buffer */
	inline GLuint* GridElementBuffer2D(int size) {
		int indice = size + 1;
		GLuint* elems = new GLuint[size * size * 6];
		int index =  0;

		for (int i = 0; i < size; i++) {		// row (size - 1) times (multiplier)
			int r = indice * i;
			for (int j = 0; j < size; j++) {			// each square in row
				for (int k = 0; k < 2; k++) {			// triangles in a square (2)
					elems[index] = r + j;				// number of square in row
					index++;
					int res = (k == 0) ? r+j+1 : r+j+indice;
					elems[index] = res;
					index++;
					elems[index] = r + j + indice + 1;
					index++;
				}
			}
		}
		
		//print for testing
		/*
		std::cout << "__________" << std::endl;
		std::cout << index << std::endl;
		for (int i = 0; i < (size * size * 6); i++) {
			std::cout << elems[i] << " ";
		}
		std::cout << std::endl;
		*/
		return elems;
	}

	/* Generating Cube coordinates centered at origin */
	/*
		+------+. 
		|`.    | `
		|  `+--+---+
		|   |  |   |
		+---+--+.  |
		 `. |    `.|
		   `+------+
	*/
	// 8 points, 3 values for each point
	constexpr std::array<float, 8 * 3> UnitCube3D = 
	{ 
		//"back" points
		-0.5f, 0.5f, -0.5f,
		0.5f, 0.5f, -0.5f,
		-0.5f, -0.5f, -0.5f,
		0.5f, -0.5f, -0.5f,
		//"front" points
		-0.5f, 0.5f, 0.5f,
		0.5f, 0.5f, 0.5f,
		-0.5f, -0.5f, 0.5,
		0.5f, -0.5f, 0.5f
	};
	// cube with normals
	constexpr std::array<float, 6 * 24> UnitCube3DNormals =
	{
	-0.5f, -0.5f, -0.5f,  0.0f,  0.0f, -1.0f,
	 0.5f, -0.5f, -0.5f,  0.0f,  0.0f, -1.0f,
	-0.5f,  0.5f, -0.5f,  0.0f,  0.0f, -1.0f,
	 0.5f,  0.5f, -0.5f,  0.0f,  0.0f, -1.0f,


	-0.5f, -0.5f,  0.5f,  0.0f,  0.0f, 1.0f,
	 0.5f, -0.5f,  0.5f,  0.0f,  0.0f, 1.0f,
	-0.5f,  0.5f,  0.5f,  0.0f,  0.0f, 1.0f,
	 0.5f,  0.5f,  0.5f,  0.0f,  0.0f, 1.0f,

	 -0.5f,  0.5f,  0.5f, -1.0f,  0.0f,  0.0f,
	 -0.5f,  0.5f, -0.5f, -1.0f,  0.0f,  0.0f,
	 -0.5f, -0.5f,  0.5f, -1.0f,  0.0f,  0.0f,
	 -0.5f, -0.5f, -0.5f, -1.0f,  0.0f,  0.0f,

	 0.5f,  0.5f,  0.5f,  1.0f,  0.0f,  0.0f,
	 0.5f,  0.5f, -0.5f,  1.0f,  0.0f,  0.0f,
	 0.5f, -0.5f,  0.5f,  1.0f,  0.0f,  0.0f,
	 0.5f, -0.5f, -0.5f,  1.0f,  0.0f,  0.0f,

	-0.5f, -0.5f, -0.5f,  0.0f, -1.0f,  0.0f,
	 0.5f, -0.5f, -0.5f,  0.0f, -1.0f,  0.0f,
	-0.5f, -0.5f,  0.5f,  0.0f, -1.0f,  0.0f,
	 0.5f, -0.5f,  0.5f,  0.0f, -1.0f,  0.0f,

	-0.5f,  0.5f, -0.5f,  0.0f,  1.0f,  0.0f,
	 0.5f,  0.5f, -0.5f,  0.0f,  1.0f,  0.0f,
	-0.5f,  0.5f,  0.5f,  0.0f,  1.0f,  0.0f,
	 0.5f,  0.5f,  0.5f,  0.0f,  1.0f,  0.0f
	};
	/* Cube Index */
	// 6 walls, 6 indices
	constexpr std::array<GLuint, 6 * 6> CubeElementBuffer3D =
	{
		//back wall
		0, 1, 3,	0, 2, 3,
		//left wall
		0, 4, 6,	0, 2, 6,
		//right wall
		5, 1, 3,	5, 7, 3,
		//top wall
		0, 1, 5,	0, 4, 5,
		//floor
		2, 3, 7,	2, 6, 7,
		//front wall
		4, 5, 7,	4, 6, 7
	};

	/* Cube transformation for grid */
	glm::vec3** createGridCubeTransVec(const int gridsize) {
		int index = 0;
		float gridsizef = static_cast<float>(gridsize);
		// calculating length of each tile
		float length = 1 / gridsizef;
		/* "algo" is the calculation of the coordinate of the column cube placement from left to right on the board (y) */
		// the "length" is the length of a square on the board
		// -(gridsize / 2) is the length from origin to the left side
		// + 0.5 offsets the placement to the middle of a tile
		float algo = -(gridsize / 2) + 0.5;
		// initializing the 2xarray 
		glm::vec3** cubeTrans = new glm::vec3 * [gridsize];
		for (int i = 0; i < gridsize; i++) {
			cubeTrans[i] = new glm::vec3[gridsize];
		}
		// iterating through the board size and calculating the x and y lengths
		// then adding them to the 2x array with a z of -0.7 to add a levitating effect
		for (int i = 0; i < gridsize; i++) {
			float x = (algo + static_cast<float>(i))*length;
			for (int j = 0; j < gridsize; j++) {
				float y = (algo + static_cast<float>(j))*length;
				cubeTrans[i][j] = glm::vec3(x, y, length/2);;
				index++;
			}
		}
		
		//printing for debug
		/*
		for (int i = 0; i < gridsize; i++) {
			for (int j = 0; j < gridsize; j++) {	
				std::cout << cubeTrans[i][j].x << " " << cubeTrans[i][j].y << " " << cubeTrans[i][j].z << " ";
			}
			std::cout << std::endl;
		}
		*/
		

		return cubeTrans;
	}


}
