#ifndef BUFFERLAYOUT_H
#define BUFFERLAYOUT_H

#include <vector>
#include <string>
#include "../Shader/ShaderDataTypes.h"


struct BufferAttribute
{
    BufferAttribute(ShaderDataType type, const std::string& name, GLboolean normalized = GL_FALSE)
        : Name(name), Type(type), Size(ShaderDataTypeSize(type)), Offset(0), Normalized(normalized) {}

    std::string Name;
    ShaderDataType Type;
    GLuint Size;
    GLuint Offset;
    GLboolean Normalized;
    
};

class BufferLayout
{
public:
    BufferLayout() {}
    BufferLayout(const std::initializer_list<BufferAttribute>& attributes)
        : Attributes(attributes)
    {
        CalculateOffsetAndStride();
    }

    inline const std::vector<BufferAttribute>& GetAttributes() const { return Attributes; }
    inline GLsizei GetStride() const { return Stride; }
    
    std::vector<BufferAttribute>::iterator begin() { return Attributes.begin(); }
    std::vector<BufferAttribute>::iterator end() { return Attributes.end(); }
    std::vector<BufferAttribute>::const_iterator begin() const { return Attributes.begin(); }
    std::vector<BufferAttribute>::const_iterator end() const { return Attributes.end(); }

private:
    void CalculateOffsetAndStride()
    {
        GLsizei offset = 0;
        Stride = 0;
        for (auto& attribute : Attributes)
        {
            attribute.Offset = offset;
            offset += attribute.Size;
            Stride += attribute.Size;
        }
    }

private:
    std::vector<BufferAttribute> Attributes;
    GLsizei Stride;
};

#endif // BUFFERLAYOUT_H
