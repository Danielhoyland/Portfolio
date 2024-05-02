#include <iostream>
#define GLFW_INCLUDE_NONE
#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include "exam.h"
#include "GeometricTools.h"
#include "VertexBuffer.h"
#include "IndexBuffer.h"
#include "VertexArray.h"
#include "RenderCommands.h"
#include "Shaders.h"
#include "shaders/exam_shader.h"
#include "Camera/PerspectiveCamera.h"
#include "TextureManager.h"

using namespace RenderCommands;

//global variable to tracked pressed key
int keypressed = 0;
static bool toggle = false;

void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods);
int spinFunc();
void zoomFunc(float& zoom);

examApplication::examApplication(const std::string& name, const std::string& version) : GLFWApplication(name, version) {
    std::cout << "Created instance" << std::endl;
}

unsigned examApplication::Run() const {

    /* -*-*-*-*-*-*-* SETUP -*-*-*-*-*-*-* */
    // time
    float deltaTime = 0.0f;	// time between current frame and last frame
    float lastFrame = 0.0f; // time of last frame
    float elapsed = 0.0f;   // time since program start
    float cycle = 0.0f;     // tracking a cycle (0.2f - 1.0f)
    // amount of squares on grid
    const int gridsize = 10;
    int size = gridsize + 1;        // amount of grid lines
    // top level values
    int rot = 0;               // variable to keep track if camera rotates
    float zoom = 10.0f;        // variable to keep track if camera zooms 
    float ambient = 0.2f;       // global ambient strength
    float diffuse = 0.8f;       // global diffuse strength
    float specular = 0.5f;      // global specular strength
    // colors
    const glm::vec3 red = glm::vec3(1.0f, 0.3f, 0.3f);
    const glm::vec3 green = glm::vec3(0.6f, 0.85f, 0.3f);

    // ---------------- LEVEL GENERATION ----------------

    srand(time(NULL)); // initializing RNG with current time
    int pX = 0, pY = 0;

    // - - - - PILLARS - - - -
    for (int i = 0; i < 7; i++) {
        do { // checking if new position is valid
            pX = (rand() % 7) + 1;
            pY = (rand() % 7) + 1;
        } while (cubeSetup[pX][pY] != 0);
        if(i < 6){ cubeSetup[pX][pY] = 1; } // wall
        else if (i < 7) { cubeSetup[pX][pY] = 2; playerPos[0] = pX; playerPos[1] = pY; } //player
    }
    // - - - - BOXES - - - -
    for (int i = 0; i < 6; i++) {
        do { // checking if new position is valid
            pX = (rand() % 5) + 2;
            pY = (rand() % 5) + 2;
        } while (cubeSetup[pX][pY] != 0);
        cubeSetup[pX][pY] = 3; // box
    }
    // - - - - LOCATIONS - - - -
    for (int i = 1; i < 7; i++) {
        do { // checking if new position is valid 
            pX = (rand() % 7) + 1;
            pY = (rand() % 7) + 1;
        } while (locations[pX][pY] != 0 || cubeSetup[pX][pY] != 0);
        locations[pX][pY] = 1; 
    }

    // -*-*-*-*-*-*-*SETTING UP CAMERA -*-*-*-*-*-*-*

    // making a perspective camera instance
    auto cameraPos = glm::vec3(0.0f, -20.0f, 20.0f);
    // Initialize the camera using cameraPosition
    PerspectiveCamera camera({ 45.0f, 1024, 1024, 1, -1 }, cameraPos, { 0.0f, 0.0f, 0.0f }, { 0.0f, 0.0f, 1.0f });

    // -*-*-*-*-*-*-* SETTING UP LIGHT SOURCE (SUN) -*-*-*-*-*-*-*

    auto sunPos = glm::vec3(0.0f, 0.0f, 0.0f);      // variable tracking sun position
    float sunX = 0.0f;                              // sun X value
    float sunZ = 0.0f;                              // sun Y value
    const float r = 0.25f;                          // radius (distance from origin)
    float speed = 1.0f;                             // speed at which the sun rotates
    glm::vec3 sunCol = glm::vec3(0.0f, 0.0f, 0.0f); // sun color

    // -*-*-*-*-*-*-* TEXTURES -*-*-*-*-*-*-*

    // making a texturemanager instance
    auto texture = TextureManager::GetInstance();
    // loading textures
    GLuint floorTex = texture->LoadTexture2DRGBA("floorTex", std::string(TEXTURES_DIR) + std::string("floortexture.jpg"), 0);
    GLuint boxTex = texture->LoadCubeMapRGBA("boxTex", std::string(TEXTURES_DIR) + std::string("boxtexture.png"), 1);
    GLuint wallTex = texture->LoadCubeMapRGBA("wallTex", std::string(TEXTURES_DIR) + std::string("walltexture.png"), 2);

    // -*-*-*-*-*-*-* CREATING CHESSBOARD -*-*-*-*-*-*-*

    // creating grid vertices and grid index
    auto gridV = GeometricTools::UnitGridGeometry2DTX(gridsize);
    auto gridI = GeometricTools::GridElementBuffer2D(gridsize);
    // creating index buffer
    auto gridIndexBuffer = std::make_shared<IndexBuffer>(gridI, gridsize * gridsize * 6);
    // creating vertex buffer & setting layout
    auto gridVertexBuffer = std::make_shared<VertexBuffer>(gridV, sizeof(float) * ((gridsize + 1) * (gridsize + 1) * 4));
    auto gridBufferLayout = BufferLayout({
        {ShaderDataType::Float2, "i_position"},
        {ShaderDataType::Float2, "i_texCoords"}
    });
    gridVertexBuffer->SetLayout(gridBufferLayout);
    // creating vertex array and adding index and vertex buffers
    auto gridVertexArray = std::make_shared<VertexArray>();
    gridVertexArray->AddVertexBuffer(gridVertexBuffer);
    gridVertexArray->SetIndexBuffer(gridIndexBuffer);

    // -*-*-*-*-*-*-* CHESSBOARD PERSPECTIVE -*-*-*-*-*-*-*

    // defining vectors for scaling and translation
    glm::vec3 scaleVec(1.0f, 1.0f, 1.0f);
    glm::vec3 translateVector(0.0f, 0.0f, 0.0f);
    // creating scaling, rotation and translation matrices
    glm::mat4 scaleMat = glm::scale(glm::mat4(1.0f), scaleVec);
    glm::mat4 translateMat = glm::translate(glm::mat4(1.0f), translateVector);
    // combining the matrices
    glm::mat4 chessboardModelMatrix = (scaleMat * translateMat);

    // -*-*-*-*-*-*-* CREATING CUBE -*-*-*-*-*-*-*
    // placeholder cube color
    glm::vec3 cubeColor = glm::vec3(1.0f, 0.0f, 0.0f);
    glm::vec3 wireColor = glm::vec3(1.0f, 0.0f, 0.0f);
    // creating cube vertices and grid index
    auto cubeVertices = GeometricTools::UnitCube3DNormals;
    auto cubeIndex = GeometricTools::CubeElementBuffer3D;
    // cubesTrans are the translation vectors for all places on the board
    // for drawing the cubes
    auto cubesTrans = GeometricTools::createGridCubeTransVec(gridsize);
    // creating index buffer
    auto cubeIndexBuffer = std::make_shared<IndexBuffer>(cubeIndex.data(), cubeIndex.size());
    // creating vertex buffer & setting layout
    auto cubeVertexBuffer = std::make_shared<VertexBuffer>(cubeVertices.data(), cubeVertices.size() * 6 * sizeof(float));
    auto cubeBufferLayout = BufferLayout({
        {ShaderDataType::Float3, "i_position"},
        {ShaderDataType::Float3, "i_normal"}
    });
    cubeVertexBuffer->SetLayout(cubeBufferLayout);
    // creating vertex array and adding index and vertex buffers
    auto cubeVertexArray = std::make_shared<VertexArray>();
    cubeVertexArray->AddVertexBuffer(cubeVertexBuffer);
    cubeVertexArray->SetIndexBuffer(cubeIndexBuffer);

    // -*-*-*-*-*-*-* CUBE PERSPECTIVE -*-*-*-*-*-*-*

    // defining vectors for scaling and translation
    float cubeSize = 1 / static_cast<float>(gridsize);
    // cube scale vector & matrix
    glm::vec3 cubeSVec(cubeSize);
    glm::mat4 cubeScaleMat = glm::scale(glm::mat4(1.0f), cubeSVec);
    // model matrix 
    glm::mat4 cModel = glm::mat4(1.0f);

    // -*-*-*-*-*-*-* SETTING UP SHADERS -*-*-*-*-*-*-*

    auto shader = std::make_shared<Shader>(vertexShaderSrc, fragmentShaderSrc);
    auto cubeShader = std::make_shared<Shader>(cubeVertexShaderSrc, cubeFragmentShaderSrc);

    // CHESSBOARD SHADER
    shader->Bind();
    shader->UploadUniformMat4("u_model", chessboardModelMatrix);            // model matrix
    shader->UploadUniformUint("u_gridsize", gridsize);          // size of chessboard  
    shader->UploadUniformMat4("u_projectionView", camera.GetViewProjectionMatrix());   // projection view
    // CUBE SHADER
    cubeShader->Bind();
    cubeShader->UploadUniformFloat3("u_color", cubeColor);          // cube color       
    cubeShader->UploadUniformMat4("u_model", cModel);          // model matrix
    cubeShader->UploadUniformMat4("u_projectionView", camera.GetViewProjectionMatrix());   // projection view

    /* -*-*-*-*-*-*-* APPLICATION LOOP -*-*-*-*-*-*-* */

    while (!glfwWindowShouldClose(window))
    {
        // clear color/depth before next loop
        Clear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        // ---------------- FUNCTIONALITY ----------------

        // resetting values between loops
        glm::mat4 cubeTranslateMat = glm::mat4(1.0f);
        float scaleMod = 1.0f; 
        int type = 0;

        // set key callback
        glfwSetKeyCallback(window, key_callback);

        // time calculations
        float currentFrame = glfwGetTime();
        deltaTime = currentFrame - lastFrame;
        lastFrame = currentFrame;
        elapsed += deltaTime;       // time elapsed since program start
        cycle = cos(elapsed);
        if (cycle < 0.2f) { cycle = 0.2f; }

        // update sun location
        sunZ = r * cos(elapsed*speed) * 6;
        sunX = r * sin(elapsed*speed) * 6;
        sunPos = glm::vec3(sunX, 0.0f, sunZ);
        
        // day/night cycle logic
        ambient = cycle;
        if (cycle == 0.2f) {
            specular = 0.0f; diffuse = 0.0f; ambient = 0.2f;
        }
        else {
            specular = 0.8f; diffuse = 0.8f;
        }
        // updating background color based on day/night
        SetClearColor(cycle*0.4f, cycle*0.4f, cycle, 1.0f);

        // updating shaders with new values
        cubeShader->UploadUniformFloat("u_ambientStrength", ambient);
        cubeShader->UploadUniformFloat("u_diffuseStrength", diffuse);
        cubeShader->UploadUniformFloat("u_specularStrength", specular);
        
        shader->UploadUniformFloat("u_ambientStrength", ambient);
        shader->UploadUniformFloat("u_diffuseStrength", diffuse);
        shader->UploadUniformFloat("u_specularStrength", specular);

        // tells shaders if textures are enabled / disabled
        if (mode) {
            shader->UploadUniformBool("u_mode", true);
            cubeShader->UploadUniformBool("u_mode", true);
        }
        else {
            shader->UploadUniformBool("u_mode", false);
            cubeShader->UploadUniformBool("u_mode", false);
        }

        // checks for zoom input (updates zoom)
        zoomFunc(zoom);         
        glm::mat4 scale = glm::scale(glm::mat4(1.0f), glm::vec3(zoom, zoom, zoom));
        // updating chessboard model matrix based on zoom
        glm::mat4 chessboardModelMatrix = (scale * scaleMat * translateMat);

        //updating rotation
        auto newRot = glm::rotate(glm::mat4(1.0f), (0.04f* spinFunc()), glm::vec3(0.0f, 0.0f, 1.0f));
        camera.SetPosition(newRot * glm::vec4(camera.GetPosition(), 0.0f));

        //updating rotation in shaders
        shader->UploadUniformFloat3("u_lightSourcePosition", sunPos);
        cubeShader->UploadUniformFloat3("u_lightSourcePosition", sunPos);
        shader->UploadUniformFloat3("u_cameraPosition", camera.GetPosition());
        cubeShader->UploadUniformFloat3("u_cameraPosition", camera.GetPosition());

        // if a key is pressed, do key event
        if (keypressed != 0) {
            keyevent(size);
        }

        // ---------------- DRAWING ----------------

        // drawing the grid 
        shader->Bind();
        shader->UploadUniformBool("u_toggle", false);
        shader->UploadUniformMat4("u_model", chessboardModelMatrix);
        shader->UploadUniformMat4("u_projectionView", camera.GetViewProjectionMatrix());
        gridVertexArray->Bind();
        gridVertexArray->GetIndexBuffer()->Bind();
        SetWireMode(GL_FILL);
        glDrawElements(GL_TRIANGLES, (gridsize * gridsize * 6), GL_UNSIGNED_INT, 0);
        shader->Unbind();

        // drawing the cubes
        cubeShader->Bind();
        cubeVertexArray->Bind();
        cubeVertexArray->GetIndexBuffer()->Bind();
        for (int i = 0; i < gridsize; i++) {
            for (int j = 0; j < gridsize; j++) {
                if (cubeSetup[i][j] != 0) {
                    switch (cubeSetup[i][j]) {
                    case 0: break;
                    case 1: // walls
                        cubeColor = glm::vec3(0.5f, 0.5f, 0.5f);
                        wireColor = glm::vec3(0.5f, 0.5f, 0.5f);
                        scaleMod = 1.0f; type = 1;
                        break;
                    case 2: // player
                        cubeColor = glm::vec3(0.4f, 0.6f, 1.0f);
                        wireColor = glm::vec3(0.0f, 0.0f, 0.0f);
                        scaleMod = 0.65f; type = 2;
                        break;
                    case 3: // red boxes
                        cubeColor = red;
                        wireColor = glm::vec3(0.0f, 0.0f, 0.0f);
                        scaleMod = 0.5f; type = 3;
                        break;
                    case 4: // green boxes
                        cubeColor = green;
                        wireColor = glm::vec3(0.0f, 0.0f, 0.0f);
                        scaleMod = 0.8f; type = 3;
                        break;
                    }
                    // scaling cube
                    cubeScaleMat = glm::scale(glm::mat4(1.0f), cubeSVec * scaleMod);
                    // getting translation
                    cubeTranslateMat = glm::translate(glm::mat4(1.0f), cubesTrans[i][j]);
                    // calculating new model matrix
                    glm::mat4 cubeModelMatrix = (scale * cubeTranslateMat * cubeScaleMat);
                    // setting texture type
                    cubeShader->UploadUniformUint("u_type", type);
                    // uploading new model matrix
                    cubeShader->UploadUniformMat4("u_model", cubeModelMatrix);
                    // drawing cubes
                    SetWireMode(GL_FILL);
                    cubeShader->UploadUniformFloat3("u_color", cubeColor);
                    glDrawElements(GL_TRIANGLES, 6 * 6, GL_UNSIGNED_INT, 0);
                }
                //locations
                if (locations[i][j] != 0) {
                    switch (locations[i][j]) {
                    case 1: cubeColor = red; type = 0; break;
                    case 2: cubeColor = green; type = 0; break;
                    }
                    cubeScaleMat = glm::scale(glm::mat4(1.0f), cubeSVec);
                    // getting translation
                    cubeTranslateMat = glm::translate(glm::mat4(1.0f), cubesTrans[i][j]);
                    // calculating new model matrix
                    glm::mat4 cubeModelMatrix = (scale * cubeTranslateMat * cubeScaleMat);
                    // uploading new model matrix
                    cubeShader->UploadUniformMat4("u_model", cubeModelMatrix);
                    // setting texture type
                    cubeShader->UploadUniformUint("u_type", type);
                    // drawing locations
                    SetWireMode(GL_FILL);
                    cubeShader->UploadUniformFloat3("u_color", cubeColor);
                    glDrawElements(GL_TRIANGLES, 6 * 6, GL_UNSIGNED_INT, 0);
                }
            }
        }
        // ------- SUN -------
        // color
        cubeColor = glm::vec3(1.0f, 1.0f, 1.0f); 
        // scaling
        cubeScaleMat = glm::scale(glm::mat4(1.0f), cubeSVec);
        // setting position to sunPos
        cubeTranslateMat = glm::translate(glm::mat4(1.0f), sunPos);
        // calculating model matrix
        glm::mat4 cubeModelMatrix = (scale * cubeTranslateMat * cubeScaleMat);
        // updating shaders
        cubeShader->UploadUniformMat4("u_model", cubeModelMatrix);
        cubeShader->UploadUniformUint("u_type", 4);
        cubeShader->UploadUniformFloat3("u_color", cubeColor);
        // drawing
        SetWireMode(GL_FILL);
        glDrawElements(GL_TRIANGLES, 6 * 6, GL_UNSIGNED_INT, 0);
        
        // --------------

        keypressed = 0; // resetting pressed key

        cubeShader->UploadUniformMat4("u_projectionView", camera.GetViewProjectionMatrix());
        
        cubeShader->Unbind();
        shader->Unbind();

        glfwSwapBuffers(window);
        glfwPollEvents();
    }


    shader->Unbind();
    cubeShader->Unbind();
    return 0;
}

/**
*
* This function returns an int corresponding to which way user rotates
*
*/
int spinFunc() {
    if (keypressed == 3) { //'A'
        return -1;
    }
    else if (keypressed == 4) { //'D'
        return 1;
    }
    return 0;
}

/**
* This function updates zoom if user is pressing 'W' or 'S'
*
* @param &zoom the zoom amount for the cubes and chessboard
*/
void zoomFunc(float& zoom) {
    if (keypressed == 1) { // 'W'
        zoom += 0.2f;
    }
    else if (keypressed == 2) { // 'S'
        zoom -= 0.2f;
    }
}

void examApplication::keyevent(int size) const {
    auto p = playerPos;
    auto cs = cubeSetup;
    int t[2] = {0, 0};
    int t2[2] = {0, 0};
    // setting previous player position to empty
    cubeSetup[playerPos[0]][playerPos[1]] = 0;
    switch (keypressed) {
    case 9:
        mode = !mode;
        break;
    case 5: // up
        t[0] = p[0]; t[1] = p[1]+1;
        t2[0] = p[0]; t2[1] = p[1]+2;
        break;
    case 6: // left
        t[0] = p[0]-1; t[1] = p[1];
        t2[0] = p[0]-2; t2[1] = p[1];
        break;
    case 7: // down
        t[0] = p[0]; t[1] = p[1]-1;
        t2[0] = p[0]; t2[1] = p[1]-2;
        break;
    case 8: // right
        t[0] = p[0]+1; t[1] = p[1];
        t2[0] = p[0]+2; t2[1] = p[1];
        break;
    }
    // if target spot is not a wall
    if (cs[t[0]][t[1]] != 1) {
        // if target is a box
        if (cs[t[0]][t[1]] == 3 || cs[t[0]][t[1]] == 4) {
            // if spot behind box is a location
            if (locations[t2[0]][t2[1]] == 1 && cs[t2[0]][t2[1]] == 0) {
                // if player moves box out of location, revert location to "not completed"
                if (locations[t[0]][t[1]] == 2) { locations[t[0]][t[1]] = 1; }
                cubeSetup[t2[0]][t2[1]] = 4;
                locations[t2[0]][t2[1]] = 2;
                playerPos[0] = t[0]; playerPos[1] = t[1];
                toggle = false;
            }
            // if spot behind box is empty
            else if (cs[t2[0]][t2[1]] == 0) { // move player and box
                // if player moves box out of location, revert location to "not completed"
                if (locations[t[0]][t[1]] == 2) { locations[t[0]][t[1]] = 1; }
                cubeSetup[t2[0]][t2[1]] = 3;
                playerPos[0] = t[0]; playerPos[1] = t[1];
                toggle = false;
            }
        }
        else { // if target spot is empty
            // move player to target spot
            playerPos[0] = t[0]; playerPos[1] = t[1];
            toggle = false;
        }
    }
    // setting new player position to player
    cubeSetup[playerPos[0]][playerPos[1]] = 2;
}


void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    // -_-_-_-_-_-_ EXIT PROGRAM _-_-_-_-_-_-
    if (key == GLFW_KEY_Q && action == GLFW_PRESS) {
        glfwTerminate();
        exit(EXIT_SUCCESS);
    }
    // -_-_-_-_-_-_ ZOOM _-_-_-_-_-_-
    else if (key == GLFW_KEY_W) {
        keypressed = 1;
    }
    else if (key == GLFW_KEY_S) {
        keypressed = 2; 
    }
    else if (key == GLFW_KEY_W && action == GLFW_RELEASE) {
        keypressed = 0; 
    }
    else if (key == GLFW_KEY_S && action == GLFW_RELEASE) {
        keypressed = 0; 
    }
    // -_-_-_-_-_-_ ROTATION _-_-_-_-_-_-
    else if (key == GLFW_KEY_A) {
        keypressed = 3; 
    }
    else if (key == GLFW_KEY_D) {
        keypressed = 4;
    }
    else if (key == GLFW_KEY_A && action == GLFW_RELEASE) {
        keypressed = 0; 
    }
    else if (key == GLFW_KEY_D && action == GLFW_RELEASE) {
        keypressed = 0;
    }
    // -_-_-_-_-_-_ MOVEMENT _-_-_-_-_-_-
    else if (key == GLFW_KEY_UP && action == GLFW_PRESS) {
        keypressed = 5;
    }
    else if (key == GLFW_KEY_LEFT && action == GLFW_PRESS) {
        keypressed = 6; 
    }
    else if (key == GLFW_KEY_DOWN && action == GLFW_PRESS) {
        keypressed = 7; 
    }
    else if (key == GLFW_KEY_RIGHT && action == GLFW_PRESS) {
        keypressed = 8; 
    }
    // -_-_-_-_-_-_ TOGGLE TEXTURES _-_-_-_-_-_-
    else if (key == GLFW_KEY_T && action == GLFW_PRESS) {
        keypressed = 9;
    }
}

