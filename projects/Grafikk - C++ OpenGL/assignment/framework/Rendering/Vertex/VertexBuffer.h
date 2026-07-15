#ifndef VERTEXBUFFER_H_
#define VERTEXBUFFER_H_

#include <glad/glad.h>
#include "BufferLayout.h"

class VertexBuffer
{
public:
	VertexBuffer(float* vertices, GLsizei size);
	~VertexBuffer();

	void Bind() const;

	void Unbind() const;

	void BufferSubData(GLintptr offset, GLsizeiptr size, const void* data) const;

	const BufferLayout& GetLayout() const { return Layout; }
	void SetLayout(const BufferLayout& layout) { Layout = layout; }


private:
	GLuint VertexBufferID;
	BufferLayout Layout;
};

#endif // VERTEXBUFFER_H_
