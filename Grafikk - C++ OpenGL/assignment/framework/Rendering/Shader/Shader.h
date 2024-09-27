#ifndef SHADER_H_
#define SHADER_H_

#include <iostream>
#include <string>
#include <cmath>
#include <array>
#include <vector>

#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>      
#include <glm/gtc/matrix_transform.hpp> 
#include <glm/gtc/type_ptr.hpp>

class Shader
{
public:
	Shader(const std::string& vertexSrc, const std::string& fragmentSrc);
	~Shader();

	void Bind() const;
	void Unbind() const;
	void UploadUniformFloat2(const std::string& name, const glm::vec2& vector);
	GLint GetUniformLocation(const std::string& name) const;
	void SetMatrix4(const std::string& name, const glm::mat4& matrix);
	void SetVector4f(const std::string& name, const glm::vec4& value) {
		glUniform4fv(GetUniformLocation(name), 1, glm::value_ptr(value));
	}
	void UploadUniformInt(const std::string& name, int value);
	void UploadUniformFloat(const std::string& name, float value);
	void UploadUniformFloat3(const std::string& name, glm::vec3 value);
	void UploadUniformBool(const std::string& name, bool value);
private:
	GLuint VertexShader;
	GLuint FragmentShader;
	GLuint ShaderProgram;
	void CompileShader(GLenum shaderType, const std::string& shaderSrc);
};
#endif