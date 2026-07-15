using System.Collections;
using System.Collections.Generic;
using Unity.VisualScripting;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.Rendering;

/**
 * PlayerStateMachine manages the differet states of the player character
 * 
 * Replaces the PlayerController
 */
public class PlayerStateMachine : MonoBehaviour
{
    [Header("State")]
    [SerializeField] private PlayerState currentState;
    [HideInInspector] public PlayerState airborneState { get; private set; }
    [HideInInspector] public PlayerState standingState { get; private set; }
    [HideInInspector] public PlayerState crouchingState { get; private set; }


    // Movement constants
    public const float JUMP_FORCE = 15f;
    public const float COYOTE_TIME = 0.13f;
    public const float JUMP_BUFFER_TIME = 0.2f;
    public const float JUMP_COOLDOWN = 0.2f;
    // In world space distance units, describes how far to visualize the throwing line.
    public const float THROW_MAX_VISUAL_DISTANCE = 10;

    // Movement values that need to be kept track of across states
    private float movementInput;
    private bool crouchIsPressed = false; // To avoid slamming the head into a low roof
    private float lastTimeGrounded = -1f; // For coyote time
    private float lastTimePressedJump;    // For jump buffer
    private float lastTimeJumped;         // "Jump cooldown" to disable coyote time right after a jump
    private Vector2 previousVelocity;     // For ground hit camera shake
    private float lastSlopeAngle = 0f;    // The slope angle from last FixedUpdate (is 180 when airborne to avoid a bug)
    private float qoyoteSlopeAngle = 0f;  // Used when calculating jump force
    private int throwsLeft = 2;


    [Header("Ball settings")]
    [SerializeField] private GameObject _ballPrefab = null;
    public GameObject GetBallPrefab() { return _ballPrefab; }
    [HideInInspector] public Ball2 ball = null;
    public Ball2 GetBall() { return ball; }
    [SerializeField] private float _maxInitialBallVelocity = 20;
    public float GetMaxInitialBallVelocity() { return _maxInitialBallVelocity; }
    [SerializeField]  private float _stopAimingTimeResumeDuration = 0.5f;
    public float GetStopAimingTimeResumeDuration() { return _stopAimingTimeResumeDuration; }
    [SerializeField] private float _aimingTimeScale = 0.05f;
    public float GetAimingTimeScale() { return _aimingTimeScale; }
    
    [HideInInspector] public bool input_shouldStartAiming = false;
    [HideInInspector] public bool input_shouldStopAiming = false;
    [HideInInspector] public bool input_holdingAimingBtn = false;
    [HideInInspector] public Vector2 input_adjustAimPointerPos = Vector2.zero;
    [HideInInspector] public Vector2 input_adjustAimGamepadDir = Vector2.zero;
    [HideInInspector] public Vector2 input_currentAimVector = Vector2.zero;
    // If true, it means we're using keyboard
    [HideInInspector] public bool input_UsingKeyboardMouse = true;
    [HideInInspector] public bool shouldThrowBall = false;
    [HideInInspector] public bool currentlyAiming = false;
    [HideInInspector] public LineRenderer _aiming_LineRenderer = null;


    // Components
    [HideInInspector] public CapsuleCollider2D capsuleCollider;
    [HideInInspector] public Rigidbody2D rb2d;


    // Platform layer
    [Header("Other")]
    [SerializeField] private LayerMask platformLayer;


    [Header("SFX")]
    public AudioSource ballThrowSound;
    public AudioSource ballCatchSound;
    public AudioSource teleportSound;
    public AudioSource deathSound;
    public FootstepsSFX footStepSound;
    public AudioSource groundHitSound;


    [Header("Children")]
    public Animator animator;
    public ParticleSystem slidingParticles;
    public GameObject groundHitParticlesPrefab;


    #region GetttersAndSetters

    public float MovementInput { get => movementInput; }
    public bool CrouchIsPressed { get => crouchIsPressed; }
    public float LastTimeGrounded { get => lastTimeGrounded; set => lastTimeGrounded = value; }
    public float LastTimePressedJump { get => lastTimePressedJump; set => lastTimePressedJump = value; }
    public float LastTimeJumped { get => lastTimeJumped; set => lastTimeJumped = value; }
    public Vector2 PreviousVelocity { get => previousVelocity; set => previousVelocity = value; }
    public float LastSlopeAngle { get => lastSlopeAngle; set => lastSlopeAngle = value; }
    public float QoyoteSlopeAngle { get => qoyoteSlopeAngle; set => qoyoteSlopeAngle = value; }
    public LayerMask PlatformLayer { get => platformLayer; }
    public int ThrowsLeft { get => throwsLeft; set => throwsLeft = value; }

    #endregion


    /**
     * Changes the curret state
     */
    public void SetState(PlayerState state)
    {
        currentState.Exit();
        currentState = state;
        currentState.Enter();
    }


    private void OnEnable()
    {
        if (GetBallPrefab() == null)
        {
            Debug.LogError("Player is missing a Ball prefab to spawn.");
        }
        
        // States
        airborneState = GetComponent<AirborneState>();
        standingState = GetComponent<StandingState>();
        crouchingState = GetComponent<CrouchingState>();

        // Components
        capsuleCollider = GetComponent<CapsuleCollider2D>();
        rb2d = GetComponent<Rigidbody2D>();


        var playerInput = GetComponent<PlayerInput>();
        if (playerInput == null)
        {
            Debug.LogError("No PlayerInput found");
        }
        OnControlsChanged(playerInput);
    }

    private void Start()
    {
        var playerInput = GetComponent<PlayerInput>();


        ClearJumpBuffer();
        currentState = capsuleCollider.IsTouchingLayers(platformLayer) ? standingState : airborneState;
    }

    private void Update()
    {
        currentState.Execute();   
    }

    private void FixedUpdate()
    {
        currentState.FixedExecute();
    }


    #region InputCallbacks

    /**
    * Pause Menu
    */
    void OnPause()
    {
        if (LevelManager.instance.GetComponent<UIManager>().ToggleSettingsPanelBool())
        {
            LevelManager.instance.GetComponent<UIManager>().ToggleSettingsPanel();
        }
        else if(LevelManager.instance.GetComponent<UIManager>().TogglePausePanelBool())
        {
            LevelManager.instance.GetComponent<UIManager>().TogglePausePanel();
        }
        else if(LevelManager.instance.GetComponent<UIManager>().ToggleKeyBindPanelBool())
        {
            LevelManager.instance.GetComponent<UIManager>().ToggleKeyBindPanel();
        }
        else
        {
            LevelManager.instance.GetComponent<UIManager>().TogglePausePanel();
        }

        LevelManager.instance.TogglePause();
    }

    /**
    * Horizontal movement input
    */
    void OnMove(InputValue inputValue)
    {
        if (!LevelManager.instance.isPaused && !LevelManager.instance.isWin && !LevelManager.instance.isDeath)
        {
            var inputVector = inputValue.Get<Vector2>();
            movementInput = inputVector.x;
        }
        else
        {
            movementInput = 0;
        }
    }

    /**
     * Jump input
     */
    void OnJump()
    {
        if(!LevelManager.instance.isPaused && !LevelManager.instance.isWin && !LevelManager.instance.isDeath)
        {
            currentState.DoJump();
        }     
    }

    /**
     * Crouch input
     */
    void OnCrouch(InputValue inputValue)
    {
        if (!LevelManager.instance.isPaused && !LevelManager.instance.isWin && !LevelManager.instance.isDeath)
        {
            crouchIsPressed = inputValue.isPressed;
            if (inputValue.isPressed) currentState.DoCrouch();
        }
        else
        {
            crouchIsPressed = false;
        }
    }

    void OnStartAim(InputValue inputValue)
    {
        if (!LevelManager.instance.isPaused && !LevelManager.instance.isWin && !LevelManager.instance.isDeath)
        {
            if (inputValue.isPressed)
            {
                input_holdingAimingBtn = true;
                input_shouldStartAiming = true;
                input_shouldStopAiming = false;
            }
            else
            {
                input_holdingAimingBtn = false;
                input_shouldStartAiming = false;
                input_shouldStopAiming = true;
            }
        }
        else
        {
            input_holdingAimingBtn = false;
            input_shouldStartAiming = false;
            input_shouldStopAiming = true;
        }
    }

    void OnAdjustAimPointer(InputValue inputValue)
    {
        if (!LevelManager.instance.isPaused && !LevelManager.instance.isWin && !LevelManager.instance.isDeath)
        {
            var mousePos = inputValue.Get<Vector2>();
            input_adjustAimPointerPos = mousePos;
        }

    }

    void OnAdjustAimDirection(InputValue inputValue)
    {
        if (!LevelManager.instance.isPaused && !LevelManager.instance.isWin && !LevelManager.instance.isDeath)
        {
            var input = inputValue.Get<Vector2>();
            input_adjustAimGamepadDir = input;
        }
    }

    void OnActivateThrowOrTeleport(InputValue inputValue)
    {
        if (!LevelManager.instance.isPaused && !LevelManager.instance.isWin && !LevelManager.instance.isDeath)
        {
            if (inputValue.isPressed)
            {
                shouldThrowBall = true;
            }
        }
    }

    void OnControlsChanged(PlayerInput input)
    {
        if (input.currentControlScheme == null || input.currentControlScheme == "Keyboard&Mouse")
        {
            input_UsingKeyboardMouse = true;
        } else if (input.currentControlScheme == "Gamepad")
        {
            input_UsingKeyboardMouse = false;
        }
    }

    #endregion


    #region JumpStuff

    /**
     * Clears the jump buffer by setting the timestamp for last time jump was pressed too far back in time
     */
    public void ClearJumpBuffer()
    {
        lastTimePressedJump = -JUMP_BUFFER_TIME - 1;
    }
    /**
     * Checks if coyote time is valid
     */
    public bool ValidCoyoteTime() => Time.time - lastTimeGrounded <= COYOTE_TIME;

    /**
     * Chects if a jump is buffered
     */
    public bool JumpIsBuffered() => Time.time - lastTimePressedJump <= JUMP_BUFFER_TIME;

    #endregion


    public void OnBallReturn()
    {
        if (!LevelManager.instance.isPaused)
        {
            ball = null;
            if (input_holdingAimingBtn)
            {
                input_shouldStartAiming = true;
                input_shouldStopAiming = false;
            }
        }
    }


    /**
     * Sets the rotation of the player sprite around the contact point between the player and the ground
     */
    public void RotateSprite(float angle)
    {
        animator.transform.localPosition = Vector3.zero;
        animator.transform.rotation = Quaternion.Euler(0f,0f,0f);

        float slopeAngle = Mathf.Abs(lastSlopeAngle);
        if (slopeAngle > 80f && slopeAngle < 280f) return;

        float rotationPoint = -capsuleCollider.offset.y - capsuleCollider.size.y + Mathf.Abs(angle) * 0.01f;

        animator.transform.RotateAround(transform.position + new Vector3(0f, rotationPoint, 0f), Vector3.forward, angle);
    }

    /**
     * Should be executed when the player dies
     */
    public void PlayerDeath()
    {
        deathSound.Play();
        LevelManager.instance.ToggleDeath();
    }
}
