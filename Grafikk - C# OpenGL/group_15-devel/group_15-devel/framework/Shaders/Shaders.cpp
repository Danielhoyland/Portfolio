#include "Shaders.h"
#include <glm/gtc/type_ptr.hpp>

Shader::Shader(const std::string& vertexSrc, const std::string& fragmentSrc) {
	/* Compile Vertex Shader */
	VertexShader = glCreateShader(GL_VERTEX_SHADER);
	const GLchar* vss = vertexSrc.c_str();
	glShaderSource(VertexShader, 1, &vss, nullptr);
	glCompileShader(VertexShader);

    /* Compile Fragment shader */ 
    FragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    const GLchar* fss = fragmentSrc.c_str();
    glShaderSource(FragmentShader, 1, &fss, nullptr);
    glCompileShader(FragmentShader);

    /* Create Shader Program */
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

void Shader::UploadUniformBool(const std::string& name, const bool b) {
    Bind();
    auto ul = glGetUniformLocation(ShaderProgram, name.c_str());
    glUniform1i(ul, b ? 1 : 0);
}

void Shader::UploadUniformUint(const std::string& name, const glm::uint& i) {
    Bind();
    auto ul = glGetUniformLocation(ShaderProgram, name.c_str());
    glProgramUniform1i(ShaderProgram, ul, i);
}

void Shader::UploadUniformFloat2(const std::string& name, const glm::vec2& vector){
    Bind();
    auto ul = glGetUniformLocation(ShaderProgram, name.c_str());
    glProgramUniform2f(ShaderProgram, ul, vector.x, vector.y);
}

void Shader::UploadUniformFloat3(const std::string& name, const glm::vec3& vector) {
    Bind();
    auto ul = glGetUniformLocation(ShaderProgram, name.c_str());
    glProgramUniform3f(ShaderProgram, ul, vector.x, vector.y, vector.z);
}

void Shader::UploadUniformMat4(const std::string& name, const glm::mat4& matrix) {
    Bind();
    auto ul = glGetUniformLocation(ShaderProgram, name.c_str());
    glProgramUniformMatrix4fv(ShaderProgram, ul, 1, false, glm::value_ptr(matrix));
}

void Shader::UploadUniformFloat(const std::string& name, const glm::float32& f) {
    Bind();
    auto ul = glGetUniformLocation(ShaderProgram, name.c_str());
    glProgramUniform1f(ShaderProgram, ul, f);
}