using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using UnityEditor;
using UnityEngine;

/**
 * Class that handles the parallax effect for one of the background images
 */
public class Parallax : MonoBehaviour
{
    private Vector3 startPosition;
    private float backgroundImageWidth;

    [SerializeField] private Vector2 parallaxEffect;
    [SerializeField] private float parallaxZoomEffect;

    private Camera cam;
    private SpriteRenderer spriteRenderer;


    private void Start()
    {
        startPosition = transform.position;
        //startPosition = new(Camera.main.transform.position.x, Camera.main.transform.position.y, transform.position.z);

        cam = Camera.main;
        spriteRenderer = GetComponentInChildren<SpriteRenderer>();

        backgroundImageWidth = spriteRenderer.bounds.size.x;

        LateUpdate();
    }

    /**
     * Must execute after CameraFollow's LateUpdate()
     * https://docs.unity3d.com/Manual/class-MonoManager.html
     */
    private void LateUpdate()
    {
        //Debug.Log("Cam: " + cam.transform.position + ", StartPos: " + startPosition);

        CalculateParallaxZoom();
        CalculateParallaxPosX();
        CalculateParallaxPosY();
    }


    /**
     * Moves the background in X direction
     * https://www.youtube.com/watch?v=NBfhfDJOUhQ
     */
    private void CalculateParallaxPosX()
    {
        float relativeDistance = cam.transform.position.x * parallaxEffect.x;
        float relativeCameraDistance = cam.transform.position.x * (1 - parallaxEffect.x);

        if (relativeCameraDistance > startPosition.x + backgroundImageWidth)
        {
            int n = Mathf.FloorToInt((relativeCameraDistance - startPosition.x) / backgroundImageWidth);
            startPosition.x += backgroundImageWidth * n;
            transform.position += new Vector3(backgroundImageWidth * n, 0f, 0f);
        }
        else if (relativeCameraDistance < startPosition.x - backgroundImageWidth)
        {
            int n = Mathf.Abs(Mathf.FloorToInt((relativeCameraDistance - startPosition.x) / backgroundImageWidth)) - 1;
            startPosition.x -= backgroundImageWidth * n;
            transform.position -= new Vector3(backgroundImageWidth * n, 0f, 0f);
        }

        float newPosition = startPosition.x + relativeDistance;
        transform.position = new(newPosition, transform.position.y, transform.position.z);
    }

    /**
     * Moves the background in Y direction
     */
    private void CalculateParallaxPosY()
    {
        float relativeDistance = (cam.transform.position.y - startPosition.y) * parallaxEffect.y;
        float newPosition = startPosition.y + relativeDistance;
        transform.position = new(transform.position.x, newPosition, transform.position.z);
    }

    /**
     * Scales the background with camera zoom
     */
    private void CalculateParallaxZoom()
    {
        float cameraSizeDifference = cam.orthographicSize / CameraFollow.DEFAULT_VERTICAL_SIZE;
        float newScale = 1 + (cameraSizeDifference - 1) * parallaxZoomEffect;

        // BUG: If player pos X != this pos X, then this backround image will move reletive to the camera when zooming
        //float cameraOffset = cam.transform.position.x - transform.position.x;
        //float scaleDiff = newScale - transform.localScale.x;

        transform.localScale = new(newScale, newScale);
        //startPosition.x += cameraOffset * scaleDiff;
        //transform.position = new(transform.position.x + (cameraOffset * scaleDiff), transform.position.z, transform.position.z);

        backgroundImageWidth = spriteRenderer.bounds.size.x * transform.localScale.x;
    }
}
