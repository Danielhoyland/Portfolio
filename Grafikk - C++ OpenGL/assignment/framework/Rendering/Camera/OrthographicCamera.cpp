#include "OrthographicCamera.h"
#include <glm/gtc/matrix_transform.hpp>

OrthographicCamera::OrthographicCamera(const Frustrum& frustrum, const glm::vec3& position, float rotation)
    : Camera()
{
    this->Position = position;
    this->Rotation = rotation;
    this->CameraFrustrum = frustrum;
    this->RecalculateMatrix();
}

void OrthographicCamera::RecalculateMatrix()
{
    ProjectionMatrix = glm::ortho(
        CameraFrustrum.left, CameraFrustrum.right,
        CameraFrustrum.bottom, CameraFrustrum.top,
        CameraFrustrum.near, CameraFrustrum.far
    );

    glm::mat4 translation = glm::translate(glm::mat4(1.0f), -Position);
    glm::mat4 rotationZ = glm::rotate(glm::mat4(1.0f), glm::radians(Rotation), glm::vec3(0.0f, 0.0f, 1.0f));

    ViewMatrix = rotationZ * translation;
}
