#ifndef SHADERS_H_
#define SHADERS_H_
#include <iostream>
#include <string>
#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/type_ptr.hpp>

class Shader
{
public:
	Shader(const std::string& vertexSrc, const std::string& fragmentSrc);
	~Shader();

	void Bind() const;
	void Unbind() const;
	void UploadUniformBool(const std::string& name, const bool b);
	void UploadUniformFloat2(const std::string& name, const glm::vec2& vector);
	void UploadUniformFloat3(const std::string& name, const glm::vec3& vector);
	void UploadUniformMat4(const std::string& name, const glm::mat4& matrix);
	void UploadUniformUint(const std::string& name, const glm::uint& i);
	void UploadUniformFloat(const std::string& name, const glm::float32& f);

private:
	GLuint VertexShader;
	GLuint FragmentShader;
	GLuint ShaderProgram;

	void CompileShader(GLenum shaderType, const std::string& shaderSrc);
};


#endif // SHADERS_H_
