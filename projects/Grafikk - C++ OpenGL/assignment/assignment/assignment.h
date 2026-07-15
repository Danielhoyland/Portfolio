#ifndef ASSIGNMENT_H
#define ASSIGNMENT_H

#include "GLFWApplication.h"
#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <iostream>
#include <string>
#include <array>
#include <vector>
#include "Shader/Shader.h"
#include "Shader/ShaderDatatypes.h"
#include "Vertex/VertexBuffer.h"
#include "Vertex/IndexBuffer.h"
#include "Vertex/VertexArray.h"

void input(GLFWwindow* window, int key, int scancode, int action, int mods);
GLuint LoadTexture(const std::string& filepath, GLuint slot);

class Assignment : public GLFWApplication
{
public:
    Assignment(std::string name, std::string version);
    ~Assignment();

    unsigned Run();
    unsigned Init();
    unsigned IfInput();
    void textureInput(std::shared_ptr <Shader> chessboardShader); //for texture
    glm::mat4 rotateAndZoomInput(glm::mat4 value); //for zoom and rotate
    std::shared_ptr<VertexArray> DrawUnits(int x, int y); //for getting the vertexarray for the cubes on the board

private:
    GLFWwindow* window;

    bool colors[8][8];            //what square is the green
    int unitColor[8][8];          //the color of each square if cubes is there
    bool unitPlacement[8][8];     //where cubes are placed
    bool selected = false;        //if cube is selected

    int boardCordinateX = 0;      //cordinate of board X value
    int boardCordinateY = 0;      //cordinate of board Y value
    int savedCordinateX = 0;      //temp global variable to store the X cordinate of where a cube was
    int savedCordinateY = 0;      //temp global variable to store the Y cordinate of where a cube was
    int savedColor = 0;           //temp global variable to store the color value of a cube

    int DivisionsXY = 8;          //amount of squares the chess board have
    bool textureBool = false;     //used to alter the shaderbool in a way both shaders get the change

    bool shaderBool = false;      //if the shaders both cube and chessboard should use the texture or not

    float zoomMIN = 0.4;          //Min zoom of the board meaning how close you can zoom in
    float zoomMAX = 2.0;          //max zoom of the board meaning how far you can go out
    float zoomCount = 1;          //is the counter to see what zoom is in the moment

    

};

#endif