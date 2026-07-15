#include "OrthographicCamera.h"
#include "glm/fwd.hpp"
#include <glm/glm.hpp>
#include <glm/ext/matrix_transform.hpp>
#include <glm/ext/matrix_clip_space.hpp>

OrthographicCamera::OrthographicCamera(const Frustrum& frustrum, const glm::vec3& position, float rotation) : Camera(){
	this->CameraFrustrum = frustrum;
	this->Position = position;
	this->Rotation = rotation;
	// calculation viewmatrix after construction
	this->RecalculateMatrix();
}



void OrthographicCamera::RecalculateMatrix() {
	auto projectionMatrix = glm::ortho(CameraFrustrum.left, CameraFrustrum.right, CameraFrustrum.bottom, CameraFrustrum.top, CameraFrustrum.near, CameraFrustrum.far);
	glm::mat4 translationMatrix = glm::translate(glm::mat4(1.0f), -Position);
	glm::mat4 rotationMatrix = glm::rotate(glm::mat4(1.0f), glm::radians(Rotation), glm::vec3(0.0f, 0.0f, 1.0f));
	// change the viewmatrix
	ViewMatrix = rotationMatrix * translationMatrix;
}