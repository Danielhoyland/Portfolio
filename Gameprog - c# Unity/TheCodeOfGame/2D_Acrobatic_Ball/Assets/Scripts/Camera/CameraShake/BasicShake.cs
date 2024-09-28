using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * Basic camera shake implementation
 */
public class BasicShake : CameraShake
{
    // Position
    private Vector2 size;

    // Rotation
    private float targetRotation = 0f;
    private int rotationDirection = 1;
    private float rotationStrength;
    private float maxRotation;


    /**
     * Construcor
     */
    public BasicShake(
        float duration = 0.2f,
        float sizeX = 0.2f,
        float sizeY = 0.3f,
        float rotationStrength = 50f,
        float maxRotation = 5f
    ) : base(duration)
    {
        size = new(sizeX, sizeY);
        this.rotationStrength = rotationStrength;
        this.maxRotation = maxRotation;
    }


    /**
     * Updates position and rotation
     */
    public override void Update()
    {
        // Position
        currentMovement = new(Random.Range(-size.x, size.x), Random.Range(-size.y, size.y), 0f);

        // Rotation
        rotationDirection = targetRotation > 0
            ? currentRotation > targetRotation ? -1 : 1
            : currentRotation < targetRotation ? 1 : -1;
        targetRotation = maxRotation * TimeLeft() * rotationDirection;
        currentRotation += Mathf.PerlinNoise1D(TimeLeft()) * rotationStrength * rotationDirection * Time.deltaTime;
    }
}
