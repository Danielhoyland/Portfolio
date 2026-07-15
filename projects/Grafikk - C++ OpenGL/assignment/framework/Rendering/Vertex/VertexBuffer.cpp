#include "VertexBuffer.h"

VertexBuffer::VertexBuffer(float* vertices, GLsizei size)
{
    glGenBuffers(1, &VertexBufferID);

    Bind();

    glBufferData(GL_ARRAY_BUFFER, size, vertices, GL_STATIC_DRAW);
}

VertexBuffer::~VertexBuffer()
{
    glDeleteBuffers(1, &VertexBufferID);
}

void VertexBuffer::Bind() const
{
    glBindBuffer(GL_ARRAY_BUFFER, VertexBufferID);
}

void VertexBuffer::Unbind() const
{
    glBindBuffer(GL_ARRAY_BUFFER, 0);
}

void VertexBuffer::BufferSubData(GLintptr offset, GLsizeiptr size, const void* data) const
{
    Bind();

    glBufferSubData(GL_ARRAY_BUFFER, offset, size, data);
}
