#include "assignment.h"

using namespace std;

int main(int argv, char** argc) {

    Assignment app("Assignment", "1.0");

    app.Init();                                    //initilize window, glfw and glad

    app.Run();                                    //runs the application

    cout << "Returning main function!" << endl;

    return EXIT_SUCCESS;
}