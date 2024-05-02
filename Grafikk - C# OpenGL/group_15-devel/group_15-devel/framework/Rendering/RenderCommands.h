#ifndef RENDERCOMMANDS_H_
#define RENDERCOMMANDS_H_

#include <iostream>
#include <glm/glm.hpp>

namespace RenderCommands
{
    inline void Clear(GLuint mode = GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    {
        glClear(mode);
    }

    inline void SetPolygonMode(GLenum face, GLenum mode)
    {
        glPolygonMode(face, mode);
    }

    inline void DrawIndex(const std::shared_ptr<VertexArray>& vao, GLenum primitive)
    {
        glDrawElements(primitive, vao->GetIndexBuffer()->GetCount(), GL_UNSIGNED_INT, nullptr);
    }

    inline void SetClearColor(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
    {
        glClearColor(red, green, blue, alpha);
    }

    inline void SetWireMode(GLuint mode = GL_LINE | GL_FILL)
    {
        glPolygonMode(GL_FRONT_AND_BACK, mode);     
    }
}
#endif //RENDERCOMMANDS
