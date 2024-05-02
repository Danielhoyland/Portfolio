#include <iostream>
#include <glad/glad.h>
#include "IndexBuffer.h"

IndexBuffer::IndexBuffer(GLuint* indices, GLsizei count) {
	//Count = count;
	glGenBuffers(1, &IndexBufferID);
	Bind();
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, count* sizeof(GLuint), indices, GL_STATIC_DRAW);
};

IndexBuffer::~IndexBuffer() {
	glDeleteBuffers(1, &IndexBufferID);
};

// Bind the vertex buffer.
void IndexBuffer::Bind() const {
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IndexBufferID);
};

// Unbind the vertex buffer.
void IndexBuffer::Unbind() const {
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
};