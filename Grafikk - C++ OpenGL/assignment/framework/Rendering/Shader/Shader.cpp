#include "Shader.h"
 


Shader::Shader(const std::string& vertexSrc, const std::string& fragmentSrc) {
    VertexShader = glCreateShader(GL_VERTEX_SHADER);
    FragmentShader = glCreateShader(GL_FRAGMENT_SHADER);

    CompileShader(GL_VERTEX_SHADER, vertexSrc);
    CompileShader(GL_FRAGMENT_SHADER, fragmentSrc);

    ShaderProgram = glCreateProgram();
    glAttachShader(ShaderProgram, VertexShader);
    glAttachShader(ShaderProgram, FragmentShader);
    glLinkProgram(ShaderProgram);

    glDeleteShader(VertexShader);
    glDeleteShader(FragmentShader);
}

Shader::~Shader() {
    glDeleteProgram(ShaderProgram);
}

void Shader::Bind() const {
    glUseProgram(ShaderProgram);
}

void Shader::Unbind() const {
    glUseProgram(0);
}

void Shader::UploadUniformFloat2(const std::string& name, const glm::vec2& vector) {
    GLint location = glGetUniformLocation(ShaderProgram, name.c_str());
    glUniform2fv(location, 1, &vector[0]);
}
void Shader::CompileShader(GLenum shaderType, const std::string& shaderSrc) {
    GLuint shader = 0;
    shader = glCreateShader(shaderType);

    const GLchar* source = shaderSrc.c_str();
    glShaderSource(shader, 1, &source, nullptr);
    glCompileShader(shader);

    GLint success;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);

    if (!success) {
        GLchar infoLog[512];
        glGetShaderInfoLog(shader, 512, nullptr, infoLog);
        std::cerr << "Shader compilation error: " << infoLog << std::endl;
    }

    if (shaderType == GL_VERTEX_SHADER) {
        VertexShader = shader;
    }
    else if (shaderType == GL_FRAGMENT_SHADER) {
        FragmentShader = shader;
    }
}
GLint Shader::GetUniformLocation(const std::string& name) const {
    GLint location = glGetUniformLocation(ShaderProgram, name.c_str());
    if (location == -1) {
        std::cerr << "Uniform '" << name << "' not found in shader program." << std::endl;
    }
    return location;
}
void Shader::SetMatrix4(const std::string& name, const glm::mat4& matrix) {
    GLint location = glGetUniformLocation(ShaderProgram, name.c_str());
    glUniformMatrix4fv(location, 1, GL_FALSE, glm::value_ptr(matrix));
}
void Shader::UploadUniformInt(const std::string& name, int value) {
    glUniform1i(GetUniformLocation(name), value);
}
void Shader::UploadUniformFloat(const std::string& name, float value)
   {
    glUniform1f(GetUniformLocation(name), value);
}
void Shader::UploadUniformFloat3(const std::string& name, glm::vec3 value)
{
    glUniform3fv(GetUniformLocation(name), 1, glm::value_ptr(value));
}
void Shader::UploadUniformBool(const std::string& name, bool value)
{
    glUniform1i(GetUniformLocation(name), value);
}

