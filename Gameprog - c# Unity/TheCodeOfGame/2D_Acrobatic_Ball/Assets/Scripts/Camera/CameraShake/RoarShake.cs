using System.Collections;
using System.Collections.Generic;
using Unity.VisualScripting;
using UnityEngine;

/**
 * Camera shake implementation emulating a monster roar or earthquake
 */
public class RoarShake : CameraShake
{
    // Duration
    private readonly float inDuration;
    private readonly float apexDuration;
    private readonly float outDuration;

    // Position
    private Vector3 currentPosition = Vector3.zero;
    private Vector3 targetPosition;
    private Vector3 positionDirection;
    private readonly float movementSpeed;
    private readonly float maxRadius;
    private float currentRadius;

    // Rotation
    private int rotationDirection = 1;
    private readonly float rotationSpeed;
    private readonly float maxRotation;
    private float currentMaxRotation = 0f;


    /**
     * Conctructor
     */
    public RoarShake(
        float inDuration = 0.5f,
        float apexDuration = 2.5f,
        float outDuration = 1.5f,
        float movementSpeed = 0.25f,
        float maxRadius = 0.01f,
        float rotationSpeed = 10f,
        float maxRotation = 0.3f
    )
        : base(inDuration + apexDuration + outDuration)
    {
        this.inDuration = inDuration;
        this.apexDuration = apexDuration;
        this.outDuration = outDuration;

        this.movementSpeed = movementSpeed;
        this.maxRadius = maxRadius;
        positionDirection = Quaternion.Euler(0f, 0f, Random.Range(0f, 360f)) * Vector3.right;
        targetPosition = maxRadius * positionDirection;

        this.rotationSpeed = rotationSpeed;
        this.maxRotation = maxRotation;
    }


    /**
     * Updates position and rotation
     */
    public override void Update()
    {
        currentMovement += Position();
        currentRotation += Rotation();
    }


    /**
     * Returns the amount of movement the camera should do this frame
     */
    private Vector3 Position()
    {
        // current radius of movement area
        if (TimeUsed() < inDuration)
            currentRadius = Quad(inDuration, TimeUsed(), maxRadius);
        else if (TimeUsed() > inDuration + apexDuration)
            currentRadius = Quad(outDuration, TimeUsed() - duration, maxRadius);

        // Movement direction change
        if (currentPosition.magnitude > currentRadius)
        {
            targetPosition = Quaternion.Euler(0f, 0f, 180f + Random.Range(-50f, 50f)) * targetPosition;
            positionDirection = (targetPosition - currentPosition).normalized;
        }

        // Position update
        float speed = movementSpeed * currentRadius / maxRadius;
        Vector3 positionMovement = speed * Time.deltaTime * positionDirection;
        currentPosition += positionMovement;

        return positionMovement;
    }

    /**
     * Returns a rotation value to be added to the camera rotation with decreasing amplitude
     */
    private float Rotation()
    {
        // Current max rotation
        if (TimeUsed() < inDuration)
            currentMaxRotation = Quad(inDuration, TimeUsed(), maxRotation);
        else if (TimeUsed() > inDuration + apexDuration)
            currentMaxRotation = Quad(outDuration, TimeUsed() - duration, maxRotation);

        // Rotation direction change
        if (currentRotation > 0 && currentRotation > currentMaxRotation) rotationDirection = -1;
        else if (currentRotation < 0 && currentRotation < -currentMaxRotation) rotationDirection = 1;

        // Rotation update
        float speed = rotationSpeed * currentMaxRotation / maxRotation;
        return speed * Time.deltaTime * rotationDirection;
    }
}
