#include "PerspectiveCamera.h"
#include <glm/gtc/matrix_transform.hpp>

PerspectiveCamera::PerspectiveCamera(const Frustrum& frustrum, const glm::vec3& position, const glm::vec3& lookAt, const glm::vec3& upVector)
    : Camera()
{
    this->Position = position;
    this->LookAt = lookAt;
    this->UpVector = upVector;
    this->CameraFrustrum = frustrum;
    this->RecalculateMatrix();
}

void PerspectiveCamera::RecalculateMatrix()
{
    ProjectionMatrix = glm::perspective(
        glm::radians(CameraFrustrum.angle),
        CameraFrustrum.width / CameraFrustrum.height,
        CameraFrustrum.near,
        CameraFrustrum.far
    );

    ViewMatrix = glm::lookAt(
        Position,    // Camera position
        LookAt,      // LookAt vector
        UpVector     // Up vector
    );

    ViewProjectionMatrix = ProjectionMatrix * ViewMatrix;
}
