#ifndef VERTEXARRAY_H_
#define VERTEXARRAY_H_

#include <glad/glad.h>
#include "VertexBuffer.h"
#include "IndexBuffer.h"
#include <vector>
#include <memory>

class VertexArray {
public:
    VertexArray();
    ~VertexArray();

    void Bind() const;

    void Unbind() const;

    void AddVertexBuffer(const std::shared_ptr<VertexBuffer>& vertexBuffer);

    void SetIndexBuffer(const std::shared_ptr<IndexBuffer>& indexBuffer);

    const std::shared_ptr<IndexBuffer>& GetIndexBuffer() const { return IdxBuffer; }

private:
    GLuint m_vertexArrayID;
    std::vector<std::shared_ptr<VertexBuffer>> VertexBuffers;
    std::shared_ptr<IndexBuffer> IdxBuffer;

    const std::vector<std::shared_ptr<VertexBuffer>>& GetVertexBuffers() const { return VertexBuffers; }
};

#endif // VERTEXARRAY_H_
