#include <iostream>
#include <glad/glad.h>
#include "VertexBuffer.h"

VertexBuffer::VertexBuffer(const void* vertices, GLsizei size) {
	glGenBuffers(1, &VertexBufferID);
	Bind();
	glBufferData(GL_ARRAY_BUFFER, size, vertices, GL_STATIC_DRAW);
};

VertexBuffer::~VertexBuffer() {
	glDeleteBuffers(1, &VertexBufferID);
};

// Bind the VertexBuffer
void VertexBuffer::Bind() const {
	glBindBuffer(GL_ARRAY_BUFFER, VertexBufferID);
};

// Unbind the VertexBuffer
void VertexBuffer::Unbind() const {
	// unbinding buffer by binding 0
	glBindBuffer(GL_ARRAY_BUFFER, 0);
};

// Fill a specific segment of the buffer specified by an offset and size with data.
void VertexBuffer::BufferSubData(GLintptr offset, GLsizeiptr size, const void* data) const {
	Bind();
	glBufferSubData(GL_ARRAY_BUFFER, offset, size, data);
};