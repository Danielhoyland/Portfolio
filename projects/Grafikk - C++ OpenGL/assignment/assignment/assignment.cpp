#define GLFW_INCLUDE_NONE
#include "assignment.h"
#include "resources/shaders/chess.h"
#include "resources/shaders/unit.h"

#include <iostream>
#include <string>
#include <cmath>
#include <array>
#include <vector>


#include <GLFW/glfw3.h>
#include "Texture/TextureManager.h"
#include "GeometricTools.h"
#include "RenderCommands.h"
#include "Camera/OrthographicCamera.h"
#include "Camera/PerspectiveCamera.h"



using namespace std;
int keys[10] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};


Assignment::Assignment(std::string name, std::string version) : GLFWApplication(name, version)
{
    //gives all list locations in the 2D array for the green square the value false
    for (int i = 0; i < DivisionsXY; i++) {
        for (int j = 0; j < DivisionsXY; j++) {
            colors[i][j] = false;
        }
    }
    //making the front two rows and last 2 rows being a color and that cubes are placed there
    for (int x = 0; x < 8; x++) {
        for (int y = 0; y < 8; y++) {
            if (y == 0 || y == 1 || y == 6 || y == 7) {
                unitPlacement[x][y] = true;
            }
            else {
                unitPlacement[x][y] = false;
            }

            if (y == 0 || y == 1) {
                unitColor[x][y] = 1;
            }
            else if (y == 6 || y == 7) {
                unitColor[x][y] = 2;
            }
            else {
                unitColor[x][y] = 0; 
            }
        }
    }
    cout << "Created instance!" << endl;

}

Assignment::~Assignment()
{
    glfwDestroyWindow(window);
    cout << "Closed window!" << endl;
}



unsigned Assignment::Init() {
   
    // Initialization of glfw.
    if (!glfwInit())
    {
        std::cin.get();

        return EXIT_FAILURE;
    }
    //set the openGL version
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    //Initialization of window
    GLFWApplication::WindowSize(800, 600);
    window = glfwCreateWindow(windowX, windowY, "Assignment", nullptr, nullptr);
    if (window == nullptr)
    {
        glfwTerminate();

        return EXIT_FAILURE;
    }
    glfwMakeContextCurrent(window);

    //init GLaDOS
    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    {
        std::cout << "Failed to initialize GLAD" << std::endl;
        glfwTerminate();
        return EXIT_FAILURE;
    }
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    return EXIT_SUCCESS;
}

unsigned Assignment::Run() {
    //gray color
    glm::vec4 grayColor = glm::vec4(0.5f, 0.5f, 0.5f, 1.0f); 
    // setting the background color
    RenderCommands::SetClearColor(grayColor);
    
    auto vertexArray = std::make_shared<VertexArray>(); //starting a vertexarray
    auto gridVertices = GeometricTools::UnitGridGeometry2d(DivisionsXY, DivisionsXY); //generate the vertices
    auto gridVertexBuffer = std::make_shared<VertexBuffer>(gridVertices.data(), gridVertices.size() * sizeof(float));  //set the vertexbuffer
    BufferLayout gridBufferLayout = {
         {ShaderDataType::Float2, "position"},
         {ShaderDataType::Float2, "tcoords"}

    };
    gridVertexBuffer->SetLayout(gridBufferLayout);//set the vertecbuffer layout
    vertexArray->AddVertexBuffer(gridVertexBuffer); //adding it to the vertexArray variable

    auto gridIndexBuffer = std::make_shared<IndexBuffer>(GeometricTools::UnitGridTopologyTriangles(DivisionsXY, DivisionsXY).data(), GeometricTools::UnitGridTopologyTriangles(DivisionsXY, DivisionsXY).size() * sizeof(float));
    vertexArray->SetIndexBuffer(gridIndexBuffer); //making and adding the grids to the vertexArray

    // Enable vertex attributes
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(float) * 2, nullptr);
    glEnableVertexAttribArray(0);

    
    //making the cordinate of the green sqaure in the 2D array to True
    colors[boardCordinateX][boardCordinateY] = true;

    TextureManager* textureManager = TextureManager::GetInstance();
    // Chessboard variables
    glm::vec3 chessboardRotationAxis(-1.0f, 0.0f, 0.0f);

    float chessboardAngle = 90.0f; //Flat on the "table"
    float chessboardAspect = windowX / windowY; //aspect of the object in the window
    float chessboardNear = 0.1f; //how close something can be to the camera
    float chessboardFar = 10.0f; //how far the camera can see objects

    glm::vec3 chessboardCameraPos = glm::vec3(0.0f, 0.8f, 0.6f); //postion of camera
    glm::vec3 chessboardCameraLooking = glm::vec3(0.0f, 0.0f, 0.0f); //looks at
    glm::vec3 chessboardUpDirection = glm::vec3(0.0f, 1.0f, 0.0f); //Up direction is y cordiante

    //making a perpectiveAmera with ealier statet values
    PerspectiveCamera perspCamera(
        { chessboardAngle, chessboardAspect, chessboardAspect, chessboardNear, chessboardFar },
        chessboardCameraPos,
        chessboardCameraLooking,
        chessboardUpDirection
    );

    // Projection matrix for the chessboard
    glm::mat4 ProjectionMatrix = perspCamera.GetProjectionMatrix();

    // View matrix for the chessboard
    glm::mat4 ViewMatrix = perspCamera.GetViewMatrix();

    glm::vec3 chessboardOrigo = glm::vec3(0.0f, 0.0f, 0.0f);
    glm::vec3 chessboardScaleTransform = glm::vec3(1.0f, 1.0f, 1.0f);

    // Model matrix for the chessboard
    glm::mat4 ModelMatrix = glm::mat4(1.0f);
    ModelMatrix = glm::translate(ModelMatrix, chessboardOrigo);
    ModelMatrix = glm::rotate(ModelMatrix, glm::radians(chessboardAngle), chessboardRotationAxis); //making the roation
    ModelMatrix = glm::scale(ModelMatrix, chessboardScaleTransform); //making the scale

    //
    auto chessboardShader = std::make_shared<Shader>(chessVertexShaderSrc, chessFragmentShaderSrc);
    chessboardShader->Bind();
    //send the values to the shader
    chessboardShader->SetMatrix4("u_projection", ProjectionMatrix);
    chessboardShader->SetMatrix4("u_view", ViewMatrix);
    chessboardShader->SetMatrix4("u_model", ModelMatrix);

    //Gets the greensquare uniform to change the one square that should be to green
    int positionLocation = chessboardShader->GetUniformLocation("position");

    //tecture path
    const std::string texturesDir = std::string(TEXTURES_DIR) + std::string("floor_texture.png");
    //loads the texture and uploads it to the shader
    textureManager->LoadTexture2DRGBA("chessTexture", texturesDir, 0); 
    chessboardShader->UploadUniformInt("uTexture", 0);
    
    //cube shader settings
    //getting the unitshader for teh cubes
    auto cubeShader = std::make_shared<Shader>(unitVertexShaderSrc, unitFragmentShaderSrc);
    cubeShader->Bind();

    //uploads the same data as for the chessboard
    cubeShader->SetMatrix4("u_projection2", ProjectionMatrix);
    cubeShader->SetMatrix4("u_view2", ViewMatrix);
    cubeShader->SetMatrix4("u_model2", ModelMatrix);

    //gets the path for the texture for the cubes
    const std::string texturesDirC = std::string(TEXTURES_DIR) + std::string("cube_texture.png");
    //loads the texture and uploads it to the shader
    textureManager->LoadCubeMapRGBA("cubeTexture", texturesDirC, 0);
    cubeShader->UploadUniformInt("uTexture", 0);

    //allows for keypresses
    glfwSetKeyCallback(window, input);	//sets callback
    glfwSwapInterval(1);				//vsync
    while (!glfwWindowShouldClose(window)) {
        //binding the chess vertex and shader
        vertexArray->Bind();
        chessboardShader->Bind();

        RenderCommands::Clear(); //clears the objects on screen
        Assignment::IfInput(); // checks if input from user

        glUniform2i(positionLocation, boardCordinateX, boardCordinateY); //says where the green square should be on board

        if(textureBool){textureInput(chessboardShader);}//The texture change/check if T is pressed

        //making the ModelMatrix for the cubes and the chess board roatet if the rotation buttons is pressed and uploads it first to chessboard
        ModelMatrix = rotateAndZoomInput(ModelMatrix);
        chessboardShader->SetMatrix4("u_model", ModelMatrix);

        RenderCommands::SetSolidMode(); //set to solid mode for the chessboard
        glDrawElements(GL_TRIANGLES, gridVertices.size(), GL_UNSIGNED_INT, nullptr); //Draws the chess board

        //unbinds the vertex and shader for the chessboard
        chessboardShader->Unbind();
        vertexArray->Unbind();

        //binds the cubeshader
        cubeShader->Bind();

        if (textureBool) { textureBool = false; textureInput(cubeShader);}//The texture change/check if T is pressed

        //now uploads the modelmatric to the cube shader while its binded
        cubeShader->SetMatrix4("u_model2", ModelMatrix);

        //making a doble for loop to "simulate" the 8x8 nature of chess board
        for (int x = 0; x < 8; x++) {
            for (int y = 0; y < 8; y++) {
                //if the color value of the unitColor is 3 then it is uploaded with selected bool
                if (unitColor[x][y]==3) {
                    cubeShader->UploadUniformBool("u_selected", true);
                }
                //if the color value of the colors is true then it is uploaded green color to show the hover over
                else if (colors[x][y]) {
                    cubeShader->UploadUniformBool("u_green", true);
                }
                //if unitColor based of the cordinate is 1 then the cubes would be blue
                else if (unitColor[x][y]==1) {
                    cubeShader->UploadUniformBool("u_colorBool", true);
                }
                //if there is something plasced in the cordinate then draw it and get the vertexfor it
                if (unitPlacement[x][y]) {
                    //gets the vertex for the cubes
                    auto unitVertex = DrawUnits(x, y);
                    unitVertex->Bind();
                    // Set to Solid mode for the cube
                    RenderCommands::SetSolidMode();
                    RenderCommands::DrawIndex(unitVertex, GL_TRIANGLES);
                    // Set to Wireframe mode for the cube
                    RenderCommands::SetWireframeMode();
                    RenderCommands::DrawIndex(unitVertex, GL_TRIANGLES);
                    //unbind the vertex for the next cube or for the chessboard
                    unitVertex->Unbind();
                }
                //just a precuation to reset the bools in the shader
                cubeShader->UploadUniformBool("u_green", false);
                cubeShader->UploadUniformBool("u_colorBool", false);
                cubeShader->UploadUniformBool("u_selected", false);
            }
        }
        //unbinding the cube shader
        cubeShader->Unbind();

        glfwPollEvents();
        glfwSwapBuffers(window);

        // If q is pressed close the window
        if (glfwGetKey(window, GLFW_KEY_Q) == GLFW_PRESS) {
            break;
        }
    }
    return EXIT_SUCCESS;
}
unsigned Assignment::IfInput() {
    
    //if left key pressed and x position is not zero then move one to the left
    if (keys[0] == 1 && boardCordinateX != 0) {
        colors[boardCordinateX][boardCordinateY] = false;
        boardCordinateX--;
        colors[boardCordinateX][boardCordinateY] = true;
        keys[0] = 0;
    }
    //If left key is pressed but it is on the left most of the board then reset key and dont move
    else if (keys[0] == 1) {
        keys[0] = 0;
    }
    //if up key pressed and y position is not 7 (upper most postion of the board) then move one up
    if (keys[1] == 1 && boardCordinateY != 7) {
        colors[boardCordinateX][boardCordinateY] = false;
        boardCordinateY++;
        colors[boardCordinateX][boardCordinateY] = true;
        keys[1] = 0;
    }
    //If up key is pressed but it is on the upper most of the board then reset key and dont move
    else if (keys[1] == 1) {
        keys[1] = 0;
    }
    //if down key pressed and y position is not 0 then move one down
    if (keys[2] == 1 && boardCordinateY != 0) {
        colors[boardCordinateX][boardCordinateY] = false;
        boardCordinateY--;
        colors[boardCordinateX][boardCordinateY] = true;
        keys[2] = 0;
    }
    //If up down is pressed but it is on the lowest most of the board then reset key and dont move
    else if (keys[2] == 1) {
        keys[2] = 0;
    }
    //if right key pressed and x position is not 7 (right most postion) then move one to the right
    if (keys[3] == 1 && boardCordinateX != 7) {
        colors[boardCordinateX][boardCordinateY] = false;
        boardCordinateX++;
        colors[boardCordinateX][boardCordinateY] = true;
        keys[3] = 0;
    }
    //If right key is pressed but it is on the right most of the board then reset key and dont move
    else if (keys[3] == 1) {
        keys[3] = 0;
    }
    if (keys[4] == 1) {
        if (unitPlacement[boardCordinateX][boardCordinateY] && !selected) {
            //saving the color of the cube in the start
            savedColor = unitColor[boardCordinateX][boardCordinateY];
            //saving the first cordinate of the cube chosen
            savedCordinateX = boardCordinateX;
            savedCordinateY = boardCordinateY;
            //changing the cube to be selected color
            unitColor[boardCordinateX][boardCordinateY] = 3;
            selected = true; // bool change to inticate that a cube is selected 
        }
        //if a selected cube is being placed on a empty square
        else if (!unitPlacement[boardCordinateX][boardCordinateY] && selected) {
            //it will get unplacedon its ealier cordinate
            unitPlacement[savedCordinateX][savedCordinateY] = false;
            //the color will be the neutral color of the ealier square
            unitColor[savedCordinateX][savedCordinateY] = 0;
            //its placed in its new cordinate
            unitPlacement[boardCordinateX][boardCordinateY] = true;
            //and given its ealier color
            unitColor[boardCordinateX][boardCordinateY] = savedColor;
            //the player is has not selected a unit after its placed
            selected = false;
        }
        //if a cube is selected and it is tried to be placed on another cubes square
        else if (unitPlacement[boardCordinateX][boardCordinateY] && selected) {
            selected = false;//unselects the cube
            unitColor[savedCordinateX][savedCordinateY] = savedColor; //and changes the color back
        }
        keys[4] = 0;
    }
    if (keys[5] == 1) {
        //if T is pressed then change global assignment variable to true
        textureBool = true;
        keys[5] = 0;
    }
    
    return EXIT_SUCCESS;
}
void Assignment::textureInput(std::shared_ptr <Shader> Shader) {
    //if T is pressed the texture will get changed to the texture and blend or back to normal
    if(textureBool){
        if (shaderBool) { shaderBool = false; }
        else { shaderBool = true; }
    }
    //send the bool statement to the shader
    Shader->UploadUniformBool("u_textureBool", shaderBool);
}
//taking in modelMatrix of the board and cubes
glm::mat4 Assignment::rotateAndZoomInput(glm::mat4 value) {
    float rotationSpeed = 1.0f; //how fast it will rotate around the board
    glm::vec3 moveZ = glm::vec3(0.0f, 0.0f, 1.0f); //just to change the z direction to go around the board
    bool ifChange = false; //bool variable for ifs
    float zoomValue = 1.0; //local variable that will help to change the scale rightly each tim we zoom

    if (keys[6] == 1) {
        //move the board and cubes clockvise
        value = glm::rotate(value, glm::radians(rotationSpeed), moveZ);
        keys[6] = 0;
    }
    if (keys[7] == 1) {
        //move the board and cubes counter clockvise
        value = glm::rotate(value, glm::radians(-rotationSpeed), moveZ);
        keys[7] = 0;
    }
    if (keys[8] == 1) {
        //limits how much a person can zoom in
        if (zoomCount < zoomMAX) {
            zoomValue += 0.01; //used to change the scale of the object to "zoom"
            zoomCount += 0.01; //counting for imiting the zoom 
            ifChange = true;
        }
        keys[8] = 0;
    }
    if (keys[9] == 1) {
        //limits how much a person can zoom out
        if (zoomCount > zoomMIN) {
            zoomValue -= 0.01; //used to change the scale of the object to "zoom"
            zoomCount -= 0.01; //counting for imiting the zoom 
            ifChange = true;
        }
        keys[9] = 0;
    }
    //if soething have been changed then do something with the zoom and chaning the modelMatrix to zoom
    if (ifChange) {
        glm::vec3 scaleTransform = glm::vec3(zoomValue, zoomValue, zoomValue);
        value = glm::scale(value, scaleTransform);
    }
    
    return value;
}


//draw a cube per call with x and y cordinate
std::shared_ptr<VertexArray> Assignment::DrawUnits(int x, int y) {
    //making a vertexarray variable
    auto vertexArrayC = std::make_shared<VertexArray>();

    //adding the left most cordinate of the chessboard which is -0.5f+1/8 times what x square its on, same with theother x just plus an extra 1/8 and same with the Y cordinates just with y instead of x
    //the values that will come out is all cordinates for a cube on a spesific square based on x and y
    auto cubeVertices = GeometricTools::generateChessCube(-0.5f + (1.0f / 8.0f) * x, -0.5f + (1.0f / 8.0f) * (x + 1), -0.5f + (1.0f / 8.0f) * (y + 1), -0.5f + (1.0f / 8.0f) * y);
    auto cubeVertexBuffer = std::make_shared<VertexBuffer>(cubeVertices.data(), cubeVertices.size() * sizeof(float)); //vertexbuffer
    
    BufferLayout cubeBufferLayout = {
        { ShaderDataType::Float3, "a_Position" }
    };//adding a_postion to the bufferlayout

    cubeVertexBuffer->SetLayout(cubeBufferLayout); //adding buffer layout to vertexbuffer

    vertexArrayC->AddVertexBuffer(cubeVertexBuffer); // adding the vertexbuffer to the vertexArray

    auto cubeIndices = GeometricTools::GenerateUnitCubeIndices(); // deciding the datapoint reading of the cubeVertices
    auto cubeIndexBuffer = std::make_shared<IndexBuffer>(cubeIndices.data(), cubeIndices.size()); //adding it to ndexbuffer

    vertexArrayC->SetIndexBuffer(cubeIndexBuffer); //adding indexbuffer to vertexArray

    //enabeling attripointer and vertexAttribarray for the vertex array
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(float) * 3, nullptr);
    glEnableVertexAttribArray(0);
   
    return vertexArrayC;
}
void input(GLFWwindow* window, int key, int scancode, int action, int mods) {
    int state;
    //exiting
    if (key == GLFW_KEY_Q) { glfwSetWindowShouldClose(window, 1); }

    //chess movement
    state = glfwGetKey(window, GLFW_KEY_LEFT);
    if (key == GLFW_KEY_LEFT && state == GLFW_RELEASE) { keys[0] = 1; }

    state = glfwGetKey(window, GLFW_KEY_UP);
    if (key == GLFW_KEY_UP && state == GLFW_RELEASE) { keys[1] = 1; }

    state = glfwGetKey(window, GLFW_KEY_DOWN);
    if (key == GLFW_KEY_DOWN && state == GLFW_RELEASE) { keys[2] = 1; }

    state = glfwGetKey(window, GLFW_KEY_RIGHT);
    if (key == GLFW_KEY_RIGHT && state == GLFW_RELEASE) { keys[3] = 1; }

    //selecting a square
    state = glfwGetKey(window, GLFW_KEY_ENTER);
    if (key == GLFW_KEY_ENTER && state == GLFW_PRESS) { keys[4] = 1; }

    //Texture change
    state = glfwGetKey(window, GLFW_KEY_T);
    if (key == GLFW_KEY_T && state == GLFW_RELEASE) { keys[5] = 1; }

    //Rotation of camera
    state = glfwGetKey(window, GLFW_KEY_L);
    if (key == GLFW_KEY_L && state == GLFW_PRESS) { keys[6] = 1; }

    state = glfwGetKey(window, GLFW_KEY_H);
    if (key == GLFW_KEY_H && state == GLFW_PRESS) { keys[7] = 1; }

    //zoom of camera
    state = glfwGetKey(window, GLFW_KEY_P);
    if (key == GLFW_KEY_P && state == GLFW_PRESS) { keys[8] = 1; }

    state = glfwGetKey(window, GLFW_KEY_O);
    if (key == GLFW_KEY_O && state == GLFW_PRESS) { keys[9] = 1; }


}
