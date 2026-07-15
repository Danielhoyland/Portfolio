using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * Script for handling borders the camera cant move outside of
 */
public class CameraBounds : MonoBehaviour
{
    [SerializeField] private float topLimit;
    [SerializeField] private float bottomLimit;
    [SerializeField] private float leftLimit;
    [SerializeField] private float rightLimit;


    /**
     * Logs and error of the camera can't fit inside the camera bounds
     */
    public void CameraBoundsSizeCheck(Camera cam)
    {
        if (topLimit - bottomLimit < cam.orthographicSize * 2)
            Debug.LogError("Camera bounds height is smaller than the camera height.");

        if (rightLimit - leftLimit < cam.orthographicSize * cam.aspect * 2)
            Debug.LogError("Camera bounds width is smaller than the camera width.");
    }

    /**
     * Clamps the camera position to be inside the camera bounds
     */
    public Vector3 ClampCameraPosition(Camera cam)
    {
        return new(
            Mathf.Clamp(
                cam.transform.position.x,
                leftLimit + cam.orthographicSize * cam.aspect,
                rightLimit - cam.orthographicSize * cam.aspect
            ),
            Mathf.Clamp(
                cam.transform.position.y,
                bottomLimit + cam.orthographicSize,
                topLimit - cam.orthographicSize
            ),
            cam.transform.position.z
        );
    }

    /**
     * Clamps the orthographic size of the camera to stay within the camera bounds
     */
    public float ClampCameraOrthographicSize(float currentSize, float cameraAspect)
    {
        return Mathf.Min(currentSize, MaxVerticalCameraSize(), MaxHorizontalCameraSize(cameraAspect));
    }

    /**
     * Returns the max orthographic size camera can have to not be bigger then the camera bounds in the vertical direction
     */
    public float MaxVerticalCameraSize() => (topLimit - bottomLimit) / 2;

    /**
     * Returns the max orthographic size camera can have to not be bigger then the camera bounds in the horizontal direction
     */
    public float MaxHorizontalCameraSize(float camAspect) => (rightLimit - leftLimit) / (2 * camAspect);


    /**
     * Shows the camera bounds in the scene view
     */
    private void OnDrawGizmos()
    {
        Gizmos.color = Color.yellow;
        Gizmos.DrawLine(new Vector2(leftLimit, topLimit), new Vector2(rightLimit, topLimit));
        Gizmos.DrawLine(new Vector2(leftLimit, bottomLimit), new Vector2(rightLimit, bottomLimit));
        Gizmos.DrawLine(new Vector2(leftLimit, topLimit), new Vector2(leftLimit, bottomLimit));
        Gizmos.DrawLine(new Vector2(rightLimit, topLimit), new Vector2(rightLimit, bottomLimit));
    }
}
