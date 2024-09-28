using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * Manages all camera shakes for the main camera
 * 
 * Taken form https://www.youtube.com/watch?v=fn3hIPLbSn8&t=282s and modified a bit
 */
public class CameraShaker
{
    private List<CameraShake> activeShakes = new();


    /**
     * Adds a new camera shake to the list of active shakes
     */
    public void Init(CameraShake newShake)
    {
        newShake.Start();
        activeShakes.Add(newShake);
    }

    /**
     * Adds the camera shake of all the active shakes to the main camera
     */
    public void Update()
    {
        if (activeShakes.Count == 0) return;

        Vector3 positionShake = Vector3.zero;
        float rotationShake = 0f;

        for (int i = activeShakes.Count - 1; i >= 0; i--)
        {
            if (activeShakes[i].TimeLeft() < 0f)
            {
                activeShakes.RemoveAt(i);
                continue;
            }

            activeShakes[i].Update();
            positionShake += activeShakes[i].currentMovement;
            rotationShake += activeShakes[i].currentRotation;
        }

        Camera.main.transform.position += positionShake;
        Camera.main.transform.rotation = Quaternion.Euler(0f, 0f, rotationShake);
    }
}
