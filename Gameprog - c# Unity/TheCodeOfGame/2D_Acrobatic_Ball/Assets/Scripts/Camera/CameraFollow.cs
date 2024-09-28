using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using Unity.VisualScripting;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.UIElements;
using static AirborneState;

/**
 * Script for controlling camera movement
 */
public class CameraFollow : MonoBehaviour
{
    // Main camera stuff
    private Camera mainCamera;
    public const float DEFAULT_VERTICAL_SIZE = 13f; // Units from the center of the camera and up or down to the edge
    public const float CAMERA_Z_COORDIANTE = -10f;


    // Player and ball references
    [SerializeField] private PlayerStateMachine playerRef;
    private Vector3 GetPlayerPos() { return playerRef.transform.position; }
    private Ball2 GetBallRef() { return playerRef.GetBall(); }


    [Header("Camera movement")]
    [Tooltip("Max distance from player to edge of the screen.\nPlayer should prevented from getting closer to camera bounds than this.")]
    [SerializeField] private Vector2 zoomZone = new(8f, 8f); // In units
    [SerializeField] private Vector2 cameraSpeed = new(10f, 7f); // How fast the camera catches uo to the player
    private Vector3 cameraMovement;

    // Camera movement related to being able to look further ahead in movement direction (not working/being used at the moment)
    [Range(0f, 0.5f)][SerializeField] private float cameraXOffset = 0f; //0.16f; // Offset of player
    private float movementInput;
    private int movementDirection = 1;


    [Header("Other")]
    [SerializeField] private CameraBounds cameraBounds;
    private bool levelHasCameraBounds = false;
    private CameraShaker cameraShaker;


    private void OnEnable()
    {
        //Ball.OnTeleport += TeleportCameraToPlayer;
        AirborneState.OnGrounded += OnGroundedEnter;
        Ball2.BallReturnedToPlayer += OnBallReturnedToPlayer;
        //Door.DoorOpen += OnDoorOpen;
    }

    private void OnDisable()
    {
        //Ball.OnTeleport -= TeleportCameraToPlayer;
        AirborneState.OnGrounded -= OnGroundedEnter;
        Ball2.BallReturnedToPlayer -= OnBallReturnedToPlayer;
        //Door.DoorOpen -= OnDoorOpen;
    }


    private void Start()
    {
        if (playerRef == null)
        {
            Debug.LogError("Player reference not set in Inspector.");
        }

        // Main camera
        mainCamera = Camera.main;
        mainCamera.orthographicSize = DEFAULT_VERTICAL_SIZE;
        mainCamera.transform.position = CameraCenterPosition(GetPlayerPos());

        // Camera bounds
        if (cameraBounds != null)
        {
            levelHasCameraBounds = true;
            cameraBounds.CameraBoundsSizeCheck(Camera.main);
        }
        
        // Camera shake
        cameraShaker = new CameraShaker();
    }


    /**
     * Last update function to be called before a frame is drawn
     */
    private void LateUpdate()
    {
        Vector3 positionDifferece = GetPositionDifference();
        cameraMovement = new(HorizontalMovement(positionDifferece.x), VerticalMovement(positionDifferece.y), 0f);
        mainCamera.transform.position += cameraMovement;
        if (levelHasCameraBounds) mainCamera.transform.position = cameraBounds.ClampCameraPosition(mainCamera);

        Zoom();

        cameraShaker.Update();
    }


    #region Movement

    /**
     * Calculates the camera movement in the X direction
     */
    private float HorizontalMovement(float xDifference)
    {
        //float playerSpeedX = cameraSpeed.x + Mathf.Abs(playerRb.velocity.x);
        //return xDifference < 0f
        //    ? Mathf.Max(-playerSpeedX * Time.deltaTime, xDifference)
        //    : Mathf.Min(playerSpeedX * Time.deltaTime, xDifference);

        return xDifference * cameraSpeed.x * Time.deltaTime;
    }

    /**
     * Calculates the camera movement in the Y drection
     */
    private float VerticalMovement(float yDifference)
    {
        return yDifference * cameraSpeed.y * Time.deltaTime;
    }

    /**
     * Sets the camera position to be where the player teleported to when the player teleports
     */
    private void TeleportCameraToPlayer()
    {
        mainCamera.transform.position = CameraCenterPosition(GetPlayerPos());
    }

    #endregion


    #region Zoom

    /**
     * Zoomes the camera in and out depending on the zoom compared to distance between player and ball
     */
    private void Zoom()
    {
        if (playerRef.GetBall() == null)
        {
            mainCamera.orthographicSize = DEFAULT_VERTICAL_SIZE;
            return;
        }

        float newOrthographicSize = Mathf.Max(
            ZoomAxis(true, HorizontalOrthographicSize() - zoomZone.x, Mathf.Abs(mainCamera.transform.position.x - GetPlayerPos().x)),
            ZoomAxis(false, mainCamera.orthographicSize - zoomZone.y, Mathf.Abs(mainCamera.transform.position.y - GetPlayerPos().y)));

        if (!levelHasCameraBounds) mainCamera.orthographicSize = newOrthographicSize;
        else
        {
            mainCamera.orthographicSize = cameraBounds.ClampCameraOrthographicSize(newOrthographicSize, mainCamera.aspect);
            RepositionCamera();
        }
    }

    /**
     * Returns the orthographic size needed in X or Y direction
     */
    private float ZoomAxis(bool xAxis, float deadZone, float positionDifference)
    {
        // IF player/ball is outside dead zone
        if (positionDifference > deadZone)
        {
            float diff = positionDifference - deadZone;
            if (xAxis) diff /= mainCamera.aspect;
            return mainCamera.orthographicSize + diff;
        }

        // IF player/ball is inside dead zone AND camera is zoomed out more than default
        else if (mainCamera.orthographicSize > DEFAULT_VERTICAL_SIZE)
        {
            float diff = deadZone - positionDifference;
            if (xAxis) diff /= mainCamera.aspect;
            return mainCamera.orthographicSize - diff;
        }

        // IF player/ball is inside dead zone AND camera is zoomed in more than default
        return DEFAULT_VERTICAL_SIZE;
    }

    /**
     * Makes sure the player is on screen and that the camera is not outside teh camera bounds after a zoom
     */
    private void RepositionCamera()
    {
        Vector3 playerPos = GetPlayerPos();
        Vector2 cameraOffsetOfPlayer = new(
            playerPos.x - mainCamera.transform.position.x,
            playerPos.y - mainCamera.transform.position.y);

        // X direction
        if (Mathf.Abs(cameraOffsetOfPlayer.x) > HorizontalOrthographicSize() - zoomZone.x)
        {
            float deadZone = HorizontalOrthographicSize() - zoomZone.x;
            if (cameraOffsetOfPlayer.x < 0) deadZone *= -1;
            float newPosX = mainCamera.transform.position.x + cameraOffsetOfPlayer.x - deadZone;
            mainCamera.transform.position = new(newPosX, mainCamera.transform.position.y, mainCamera.transform.position.z);
        }

        // Y direction
        if (Mathf.Abs(cameraOffsetOfPlayer.y) > mainCamera.orthographicSize - zoomZone.y)
        {
            float deadZone = mainCamera.orthographicSize - zoomZone.y;
            if (cameraOffsetOfPlayer.y < 0) deadZone *= -1;
            float newPosY = mainCamera.transform.position.y + cameraOffsetOfPlayer.y - deadZone;
            mainCamera.transform.position = new(mainCamera.transform.position.x, newPosY, mainCamera.transform.position.z);
        }

        // If player is outside dead zone
        mainCamera.transform.position = cameraBounds.ClampCameraPosition(mainCamera);
    }

    #endregion


    #region InputCallbacks

    /**
     * Gets the movement input values
     */
    private void OnMove(InputValue inputValue)
    {
        movementInput = inputValue.Get<Vector2>().x;
        if (Mathf.Abs(movementInput) > 0.1f) movementDirection = (int)Mathf.Round(movementInput);
    }

    // Screen shake testing
    //private void OnTestAction()
    //{
    //    CameraShake shake = CameraShakeFactory.GetEarthquakeShake();
    //    cameraShaker.Init(shake);
    //}

    #endregion


    #region Utils

    /**
     * Returns the distance from the center of the camera to the left or right edge
     */
    private float HorizontalOrthographicSize() => mainCamera.orthographicSize * mainCamera.aspect;


    /**
     * Returns the 2D position of a transform with the camera Z-coordinate
     */
    private Vector3 Get2DPosition(Vector3 position) => new(position.x, position.y, CAMERA_Z_COORDIANTE);

    /**
     * Calculates where the target position for the camera is based on the position of an object
     */
    private Vector3 CameraCenterPosition(Vector3 targetObjectPosition)
    {
        Vector3 cameraCenterPosition = Get2DPosition(targetObjectPosition);

        // If camera should be offset from the center (to look further ahead)
        cameraCenterPosition.x += HorizontalOrthographicSize() * cameraXOffset * movementDirection;

        return cameraCenterPosition;
    }

    /**
     * Returns the distance the camera has to move as a vector
     */
    private Vector3 GetPositionDifference()
    {
        var targetPosition = new Vector3();

        var playerPos = GetPlayerPos();

        var ballRef = GetBallRef();
        if (ballRef == null)
        {
            targetPosition = CameraCenterPosition(playerPos);
        } else
        {
            var ballPos = ballRef.transform.position;
            targetPosition = new(
                playerPos.x + ((ballPos.x - playerPos.x) / 2),
                playerPos.y + ((ballPos.y - playerPos.y) / 2),
                CAMERA_Z_COORDIANTE);
        }


        return targetPosition - mainCamera.transform.position;
    }

    #endregion


    #region Events

    /**
     * Initiates a "ground hit camera shake"
     */
    private void OnGroundedEnter(Vector2 currentVelocity, Vector2 prevoiusVelocity)
    {
        float minVelDiff = 40f;

        if (currentVelocity.y < prevoiusVelocity.y) currentVelocity.y = 0f; // Bug fix
        Vector2 velocityDifference = currentVelocity - prevoiusVelocity;
        
        if (velocityDifference.y < minVelDiff) return;

        CameraShake shake = CameraShakeFactory.GetGroundHitShake(velocityDifference);
        cameraShaker.Init(shake);
    }

    /**
     * Initiates a camera shake when the ball is recaled to the player
     */
    private void OnBallReturnedToPlayer()
    {
        CameraShake shake = CameraShakeFactory.GetBallCatchShake();
        cameraShaker.Init(shake);
    }

    /**
     * Camera shake for when a dor opens
     */
    public void OnDoorOpen()
    {
        CameraShake shake = CameraShakeFactory.GetEarthquakeShake();
        cameraShaker.Init(shake);
    }

    #endregion
}
