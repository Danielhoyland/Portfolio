using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;

/**
 * Class with camera shake implementations
 */
public static class CameraShakeFactory
{
    /**
     * When the player hits the gound with a certain velocity
     */
    public static GroundHitShake GetGroundHitShake(Vector2 velocityDifference)
    {
        float duration = 0.5f;

        Vector3 targetPosition = velocityDifference / 200f;
        Vector3 positionDirection = velocityDifference.normalized;
        float maxRadius = targetPosition.magnitude;

        float rotationStrength = Mathf.Pow(velocityDifference.magnitude, 2f) * duration / 150f * 2;
        float maxRotation = Mathf.Pow(velocityDifference.magnitude, 2f) * duration / 5000f * 2;

        return new GroundHitShake(duration, targetPosition, positionDirection, maxRadius, rotationStrength, maxRotation);
    }

    /**
     * When catiching the ball
     */
    public static GroundHitShake GetBallCatchShake()
    {
        float duration = 0.5f;

        float rotationStrength = 4f;
        float maxRotation = 0.1f;

        return new GroundHitShake(duration,
            Vector3.zero, Vector3.zero, 0f,
            rotationStrength, maxRotation);
    }

    /**
     * An "earthquake"
     */
    public static RoarShake GetEarthquakeShake()
    {
        return new RoarShake();
    }
}
