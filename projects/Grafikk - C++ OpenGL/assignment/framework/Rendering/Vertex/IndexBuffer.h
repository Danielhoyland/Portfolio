#ifndef INDEXBUFFER_H_
#define INDEXBUFFER_H_

#include <glad/glad.h>


class IndexBuffer
{
public:
	IndexBuffer(GLuint* indices, GLsizei count);
	~IndexBuffer();

	void Bind() const;

	void Unbind() const;

	inline GLuint GetCount() const { return Count; }

private:
	GLuint IndexBufferID;
	GLuint Count;
};

#endif // INDEXBUFFER_H_