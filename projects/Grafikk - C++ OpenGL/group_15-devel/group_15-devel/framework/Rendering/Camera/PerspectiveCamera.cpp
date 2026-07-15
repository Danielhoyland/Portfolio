#include "PerspectiveCamera.h"
#include "glm/fwd.hpp"
#include <glm/glm.hpp>
#include <glm/ext/matrix_transform.hpp>
#include <glm/ext/matrix_clip_space.hpp>


PerspectiveCamera::PerspectiveCamera(const Frustrum& frustrum, const glm::vec3& position, const glm::vec3& lookAt, const glm::vec3& upVector) 
	: Camera(){

	this->CameraFrustrum = frustrum;
	this->Position = position;
	this->LookAt = lookAt;
	this->UpVector = upVector;
	// calculation viewmatrix after construction
	this->RecalculateMatrix();
}



void PerspectiveCamera::RecalculateMatrix() {
	
    ProjectionMatrix = glm::perspective(glm::radians(CameraFrustrum.angle), CameraFrustrum.width / CameraFrustrum.height,
        CameraFrustrum.near, CameraFrustrum.far);
    ViewMatrix = glm::lookAt(Position, LookAt, UpVector);

	ViewProjectionMatrix = ProjectionMatrix * ViewMatrix;
}