using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * Camera shake implementation for when the player hits the ground
 */
public class GroundHitShake : CameraShake
{
    // Position
    private Vector3 currentPosition = Vector3.zero;
    private Vector3 targetPosition;
    private Vector3 positionDirection;
    private readonly float maxRadius;
    private float currentRadius;

    // Rotation
    private readonly float rotationStrength;
    private readonly float maxRotation;
    private float currentMaxRotation;
    private int rotationDirection = 1;


    /**
     * Constructor
     */
    public GroundHitShake(
        float duration, // = 0.5f,
        Vector3 targetPosition,
        Vector3 positionDirection,
        float maxRadius,
        float rotationStrength,
        float maxRotation
    )
        : base(duration)
    {
        this.targetPosition = targetPosition;
        this.positionDirection = positionDirection;
        this.maxRadius = this.currentRadius = maxRadius;

        this.rotationStrength = rotationStrength;
        this.maxRotation = maxRotation;
    }


    /**
     * Updates position and rotation
     */
    public override void Update()
    {
        //currentMovement += Position();
        currentRotation += Rotation();
    }

    /**
     * Returns the amount of movement the camera should do this frame
     * 
     * It's a 50/50 chance this is not notisable because:
     *  - Short duration = few frames to move in
     *  - The camera follow smoothing cancels out most of the movement (camera moves a lot when this camera shake happens)
     *  Some of the values are also a bit extreme because of this (but it's necessary)
     */
    private Vector3 Position()
    {
        if (currentPosition.magnitude > currentRadius)
        {
            currentRadius = QuadDownToZero(maxRadius, PortionOfTimeLeft()) * 0.8f;
            targetPosition = Quaternion.Euler(0f, 0f, 180f + Random.Range(-50f, 50f)) * targetPosition * currentRadius;
            positionDirection = (targetPosition - currentPosition).normalized;
        }

        Vector3 positionMovement = positionDirection * 60f * Time.deltaTime;
        currentPosition += positionMovement;
        return positionMovement;
    }

    /**
     * Returns a rotation value to be added to the camera rotation with decreasing amplitude
     */
    private float Rotation()
    {
        currentMaxRotation = Quad(duration, PortionOfTimeLeft(), maxRotation);

        // Rotation direction change
        if (currentRotation > 0 && currentRotation > currentMaxRotation) rotationDirection = -1;
        else if (currentRotation < 0 && currentRotation < -currentMaxRotation) rotationDirection = 1;

        // Rotation update
        float rotation = rotationStrength * Time.deltaTime * currentMaxRotation / maxRotation;
        return rotation * rotationDirection;
    }
}
