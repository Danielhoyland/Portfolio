#include <iostream>
#include "VertexArray.h"

// Constructor & Destructor
VertexArray::VertexArray() {
	glGenVertexArrays(1, &m_vertexArrayID);
};
VertexArray::~VertexArray() {
	glDeleteVertexArrays(1, &m_vertexArrayID);
};

// Bind vertex array
void VertexArray::Bind() const {
	glBindVertexArray(m_vertexArrayID);
};
// Unbind vertex array
void VertexArray::Unbind() const {
	glBindVertexArray(0);
};

// Add vertex buffer. This method utilizes the BufferLayout internal to
// the vertex buffer to set up the vertex attributes. Notice that
// this function opens for the definition of several vertex buffers.
void VertexArray::AddVertexBuffer(const std::shared_ptr<VertexBuffer>& vertexBuffer) {
	Bind();
	vertexBuffer->Bind();

	const auto& layout = vertexBuffer->GetLayout();

	int index = 0;
	for (auto& attrib : layout) {
		glEnableVertexAttribArray(index);
		glVertexAttribPointer(
			index, /* index for buffer (color/vertices) */
			ShaderDataTypeComponentCount(attrib.Type), /* if its Float2/Float3 the size of 1 thing */
			ShaderDataTypeToOpenGLBaseType(attrib.Type), /* the type : GL_FLOAT/GL_INT */
			(attrib.Normalized ? GL_TRUE : GL_FALSE), /* if its normalized or not and make it GL_BOOL */
			layout.GetStride(), /* if color and vetices hop 5 (2 vertices, 3 color) */
			(const void *) attrib.Offset /* offset */
		);
		index++;
	}
	VertexBuffers.push_back(vertexBuffer);
};
// Set index buffer
void VertexArray::SetIndexBuffer(const std::shared_ptr<IndexBuffer>& indexBuffer) {
	Bind();
	indexBuffer->Bind();
	IdxBuffer = indexBuffer;
};