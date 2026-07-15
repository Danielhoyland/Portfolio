#ifndef GEOMETRICTOOLS_H
#define GEOMETRICTOOLS_H

#include <array>
#include <vector>

namespace GeometricTools {

    // Function to encapsulate a unit triangle centered at the origin
    constexpr std::array<float, 6> UnitTriangle2D = { -0.5f, -0.5f, 0.5f, -0.5f, 0.0f, 0.5f }; // [2,3]

    // Function to encapsulate a unit square centered at the origin
    constexpr std::array<float, 8> UnitSquare2D = { -0.5f, -0.5f, 0.5f, -0.5f, 0.5f, 0.5f, -0.5f, 0.5f }; // [2,4]

    constexpr std::array<unsigned int, 6> UnitSquare2DTopology = {
        0, 1, 2, 2, 3, 0
    };

    std::vector<unsigned int> UnitGridTopologyTriangles(int DivisionsX, int DivisionsY) {
        std::vector<unsigned int> indices;
        int index = 0;
        for (int y = 0; y < DivisionsY; y++) {
            for (int x = 0; x < DivisionsX; x++) {
                indices.push_back(index);
                indices.push_back(index + 1);
                indices.push_back(index + 2);
                indices.push_back(index + 2);
                indices.push_back(index + 3);
                indices.push_back(index + 1);

                index +=4;
            }
        }
        return indices;
    }



    std::vector<float> UnitGridGeometry2d(int DivisionsX, int DivisionsY) {
        std::vector<float> gr;

        float divY = 1.0f / static_cast<float>(DivisionsY);
        float divX = 1.0f / static_cast<float>(DivisionsX);

        for (int i = 0; i < DivisionsY; i++) {
            float yPos = 0.5f - (divY * static_cast<float>(i));
            for (int j = 0; j < DivisionsX; j++) {
                float xPos = -0.5f + (divX * static_cast<float>(j));

                gr.push_back(xPos);
                gr.push_back(yPos);

                gr.push_back(xPos + divX);
                gr.push_back(yPos);


                gr.push_back(xPos);
                gr.push_back(yPos - divY);

                gr.push_back(xPos + divX);
                gr.push_back(yPos - divY);

                
            }
        }
        return gr;
    }

    // Constexpr version that generates the geometry of the grid, including texture coordinates
    template<unsigned int X, unsigned int Y>
    constexpr std::array<float, (X + 1)* (Y + 1) * 4> UnitGridGeometry2DWTCoords()
    {
        constexpr float stepX = 1.0f / X;
        constexpr float stepY = 1.0f / Y;
        std::array<float, (X + 1)* (Y + 1) * 4> vertices;

        for (unsigned int y = 0; y <= Y; ++y)
        {
            for (unsigned int x = 0; x <= X; ++x)
            {
                const float xPos = x * stepX - 0.5f;
                const float yPos = y * stepY - 0.5f;
                const float texCoordX = xPos;
                const float texCoordY = yPos;

                // Vertex positions (x, y)
                vertices[(x + (X + 1) * y) * 4] = xPos;
                vertices[(x + (X + 1) * y) * 4 + 1] = yPos;

                // Texture coordinates (s, t)
                vertices[(x + (X + 1) * y) * 4 + 2] = texCoordX;
                vertices[(x + (X + 1) * y) * 4 + 3] = texCoordY;
            }
        }

        return vertices;
    }

    // Function version of the above
    std::vector<float> UnitGridGeometry2DWTCoords(unsigned int X, unsigned int Y)
    {
        float stepX = 1.0f / X;
        float stepY = 1.0f / Y;
        std::vector<float> vertices;
        for (unsigned int y = 0; y <= Y; y++)
        {
            for (unsigned int x = 0; x <= X; x++)
            {
                float xPos = x * stepX - 0.5f;
                float yPos = y * stepY - 0.5f;
                float texCoordX = xPos;
                float texCoordY = yPos;

                // Vertex positions (x, y)
                vertices.push_back(xPos);
                vertices.push_back(yPos);

                // Texture coordinates (s, t)
                vertices.push_back(texCoordX);
                vertices.push_back(texCoordY);
            }
        }

        return vertices;
    }

    std::vector<unsigned int> UnitGridTopologyTriangles2DW(unsigned int DivisionsX, unsigned int DivisionsY) {
        std::vector<unsigned int> indices;
        for (unsigned int y = 0; y <= DivisionsY; y++) {
            for (unsigned int x = 0; x <= DivisionsX; x++) {
                unsigned int vertexIndex = y * (DivisionsX + 1) + x;
                printf("%i: ", vertexIndex);
                // Triangle 1
                indices.push_back(vertexIndex);
                indices.push_back(vertexIndex + 1);
                indices.push_back(vertexIndex + DivisionsX + 1);

                // Triangle 2
                indices.push_back(vertexIndex + 1);
                indices.push_back(vertexIndex + DivisionsX + 2);
                indices.push_back(vertexIndex + DivisionsX + 1);
            }
        }
        return indices;
    }




    std::vector<float> GenerateUnitCubeVertices() {
        std::vector<float> vertices = {
            // Front face
            -0.5f, -0.5f,  0.5f,
             0.5f, -0.5f,  0.5f,
             0.5f,  0.5f,  0.5f,
            -0.5f,  0.5f,  0.5f,

            // Back face
            -0.5f, -0.5f, -0.5f,
             0.5f, -0.5f, -0.5f,
             0.5f,  0.5f, -0.5f,
            -0.5f,  0.5f, -0.5f,
        };

        return vertices;
    }

    // Function to generate element (index) array for a unit cube
    std::vector<unsigned int> GenerateUnitCubeIndices() {
        std::vector<unsigned int> indices = {
            // Front face
            0, 1, 2,
            2, 3, 0,

            // Right face
            1, 5, 6,
            6, 2, 1,

            // Back face
            7, 6, 5,
            5, 4, 7,

            // Left face
            4, 0, 3,
            3, 7, 4,

            // Top face
            3, 2, 6,
            6, 7, 3,

            // Bottom face
            4, 5, 1,
            1, 0, 4
        };

        return indices;
    }
    constexpr std::array<float, 3 * 24 * 2> UnitCube3D24WNormals = {
        // Back face
        -0.5f, -0.5f, -0.5f,  0.0f,  0.0f, -1.0f,
         0.5f, -0.5f, -0.5f,  0.0f,  0.0f, -1.0f,
        -0.5f,  0.5f, -0.5f,  0.0f,  0.0f, -1.0f,
         0.5f,  0.5f, -0.5f,  0.0f,  0.0f, -1.0f,

         // Front face
        -0.5f, -0.5f,  0.5f,  0.0f,  0.0f, 1.0f,
         0.5f, -0.5f,  0.5f,  0.0f,  0.0f, 1.0f,
        -0.5f,  0.5f,  0.5f,  0.0f,  0.0f, 1.0f,
         0.5f,  0.5f,  0.5f,  0.0f,  0.0f, 1.0f,
         
         // Left face
         -0.5f,  0.5f,  0.5f, -1.0f,  0.0f,  0.0f,
         -0.5f,  0.5f, -0.5f, -1.0f,  0.0f,  0.0f,
         -0.5f, -0.5f,  0.5f, -1.0f,  0.0f,  0.0f,
         -0.5f, -0.5f, -0.5f, -1.0f,  0.0f,  0.0f,
         // Right face
         0.5f,  0.5f,  0.5f,  1.0f,  0.0f,  0.0f,
         0.5f,  0.5f, -0.5f,  1.0f,  0.0f,  0.0f,
         0.5f, -0.5f,  0.5f,  1.0f,  0.0f,  0.0f,
         0.5f, -0.5f, -0.5f,  1.0f,  0.0f,  0.0f,
         // Bottom face
        -0.5f, -0.5f, -0.5f,  0.0f, -1.0f,  0.0f,
         0.5f, -0.5f, -0.5f,  0.0f, -1.0f,  0.0f,
        -0.5f, -0.5f,  0.5f,  0.0f, -1.0f,  0.0f,
         0.5f, -0.5f,  0.5f,  0.0f, -1.0f,  0.0f,
        // Top face
        -0.5f,  0.5f, -0.5f,  0.0f,  1.0f,  0.0f,
         0.5f,  0.5f, -0.5f,  0.0f,  1.0f,  0.0f,
        -0.5f,  0.5f,  0.5f,  0.0f,  1.0f,  0.0f,
         0.5f,  0.5f,  0.5f,  0.0f,  1.0f,  0.0f
    };

    
    constexpr std::array<unsigned int, 6 * 3 * 2> UnitCube3DTopologyTriangles24 = {

        // Front 
        1, 0, 2, 1, 2, 3,

        // Back
        5, 4, 6, 5, 6, 7,

        // Right
        9, 8, 10, 9, 10, 11,

        // Left
        13, 12, 14, 13, 14, 15,

        // Top
        17, 16, 18, 17, 18, 19,

        // Bottom
        21, 20, 22, 21, 22, 23
    };
    
    std::vector<float> generateChessCube(float left, float right, float up, float down) {
        float diffrenceLR = right - left;
        float diffrenceUD = up - down;
        float xChange = diffrenceLR / 4;
        float yChange = diffrenceUD / 4;
        float l = left + xChange;
        float r = right - xChange;
        float d = down + yChange;
        float u = up - yChange;
        float f = 0.01f;
        float b = f + yChange*2;
        std::vector<float> UnitCube3D24WNormalsChess = {
            // Front face
            l, d,  b,
             r, d,  b,
             r,  u,  b,
            l,  u,  b,

            // Back face
            l, d, f,
             r, d, f,
             r,  u, f,
            l,  u, f
        };
        return UnitCube3D24WNormalsChess;
    }
}

#endif
