using System.Collections;
using System.Collections.Generic;
using Unity.VisualScripting;
using UnityEngine;
using UnityEngine.UIElements;
using UnityEngine.InputSystem;
using System;

/**
 * Controlls the movement of the player character
 */
[Obsolete("Use PlayerStateMachine instead.")]
public class PlayerController : MonoBehaviour
{
    [Header("Horizontal movement")] // Running
    [SerializeField] float movementSpeed = 15f;
    [SerializeField] float accelerationGround = 3f;
    [SerializeField] float decelerationGround = 8f;
    [SerializeField] float accelerationAir = 1.5f;
    [SerializeField] float crouchWalkingSpeed = 2f;
    [SerializeField] float crouchingAcceleration = 1.5f;
    [SerializeField] float slidingDeceleration = 0.5f;
    float movementInput;
    bool isCrouching = false;
    bool isGoingToHitHead = false;


    [Header("Vertical movement")] // Jumping and falling
    [SerializeField] float jumpForce = 15f;
    [SerializeField] float gravityScale = 3f;
    [SerializeField] float fallingGravityScale = 6f;
    [SerializeField] float coyoteTime = 0.1f;
    [SerializeField] float jumpBufferTime = 0.2f;
    bool isGrounded;
    float lastTimeGrounded = -1f;
    float lastTimePressedJump;
    float lastTimeJumped; // Jump cooldown to fix the bug where coyote time is valid right after a jup
    float jumpCooldown = 0.2f;

    // CapsuleCollider (sliding/crouching)
    Vector2 standingCapsuleColliderSize;
    Vector2 standingCapsuleColliderOffset;
    Vector2 crouchingCapsuleColliderSize;
    Vector2 crouchingCapsuleColliderOffset;

    // Slopes
    bool isOnSlope;
    float slopeCheckCastDistance = 0.5f;
    Vector2 slopeTangent;
    float lastSlopeAngle = 0f;

    // Death and restart
    [Header("Other")]
    [SerializeField] bool  useDeathPlain = true;
    [SerializeField] float deathPlain = -10f;
    [SerializeField] LayerMask platformLayer;

    // Components
    Animator animator;
    CapsuleCollider2D capsuleCollider;
    Rigidbody2D rb2d;

    #region LifeCycleCallbacks

    /**
     * "Setup for me"
     */
    private void Awake()
    {
        clearJumpBuffer();

        //SetState(GetComponent<StandingState>());
    }


    /**
     * Start is called before the first frame update
     */
    void Start()
    {
        // Components
        animator = GetComponent<Animator>();
        capsuleCollider = GetComponent<CapsuleCollider2D>();
        rb2d = GetComponent<Rigidbody2D>();

        // Capsule collider values for crouching and not crouching
        standingCapsuleColliderSize = capsuleCollider.size;
        standingCapsuleColliderOffset = capsuleCollider.offset;
        crouchingCapsuleColliderSize = new Vector2(standingCapsuleColliderSize.x, standingCapsuleColliderSize.y / 2f);
        crouchingCapsuleColliderOffset = new Vector2(standingCapsuleColliderOffset.x, standingCapsuleColliderOffset.y - 0.25f);
    }


    /**
     * Update is called once per frame
     */
    void Update()
    {
        // Death and restart
        if (useDeathPlain && transform.position.y < deathPlain) transform.position = Vector2.zero;

        // Animation
        animator.SetFloat("velocity_y", rb2d.velocity.y);
        animator.SetBool("isGrounded", isGrounded);
    }


    /**
     * Physics updates
     */
    private void FixedUpdate()
    {
        // Horizontal movement
        slopeCheck();
        if (isCrouching || isGoingToHitHead) crouchWalk();
        else run();

        //Debug.Log("vel: " + rb2d.velocity + " " + rb2d.velocity.magnitude + ", slope tangent: " + -slopeTangent + " |"+-slopeTangent.magnitude+"|");

        // Vertical movement
        if (isOnSlope && !isCrouching) rb2d.gravityScale = 0f;
        else rb2d.gravityScale = rb2d.velocity.y < 0f ? fallingGravityScale : gravityScale;
    }

    #endregion

    #region Input

    /**
     * Horizontal movement input
     */
    void OnMove(InputValue inputValue)
    {
        movementInput = inputValue.Get<Vector2>().x;
    }

    /**
     * Jump input
     */
    void OnJump(InputValue inputValue)
    {
        if (!isGrounded && !validCoyoteTime())
            lastTimePressedJump = Time.time;
        else
            jump();
    }

    /**
     * Crouch input
     */
    void OnCrouch(InputValue inputValue)
    {
        if (inputValue.isPressed)
        {
            isCrouching = true;
            crouch();
        }
        else
        {
            isCrouching = false;
            if (!isGoingToHitHead) unCrouch();
        }
    }

    #endregion

    /**
     * Horizontal movement / running logic
     * https://www.youtube.com/watch?v=KKGdDBFcu0Q
     */
    void run()
    {
        float targetVelocity = movementInput * movementSpeed;
        float velocityDifference = targetVelocity - rb2d.velocity.x;
        float accelerationRate = isGrounded
            ? Mathf.Abs(movementInput) > 0.1f ? accelerationGround : decelerationGround
            : accelerationAir; 
        float movementForce = velocityDifference * accelerationRate;

        rb2d.AddForce(movementForce * -slopeTangent);

        animator.SetFloat("velocity_x", Mathf.Abs(movementInput) < 0.1f
            ? isGrounded ? targetVelocity : rb2d.velocity.x
            : targetVelocity);

        Debug.DrawRay(transform.position, movementForce * -slopeTangent, Color.magenta);
    }

    /**
     * Crouching / sliding logic
     */
    void crouchWalk()
    {
        float targetVelocity = movementInput * crouchWalkingSpeed;
        float velocityDifference = targetVelocity - rb2d.velocity.x;
        float accelerationRate = rb2d.velocity.magnitude > crouchWalkingSpeed ? slidingDeceleration : crouchingAcceleration;
        float movementForce = velocityDifference * accelerationRate;

        rb2d.AddForce(movementForce * Vector2.right);

        animator.SetFloat("velocity_x", Mathf.Abs(rb2d.velocity.x) <= crouchWalkingSpeed + 0.1f ? 0f : rb2d.velocity.x);

        // Checks if the player will hit their head if uncrouching
        RaycastHit2D hit = Physics2D.Raycast(transform.position, Vector2.up, 1f, platformLayer);
        if (hit) isGoingToHitHead = true;
        else
        {
            isGoingToHitHead = false;
            if (!isCrouching) unCrouch();
        }

        Debug.DrawRay(transform.position, movementForce * -slopeTangent, Color.magenta);
        Debug.DrawLine(transform.position, transform.position + new Vector3(rb2d.velocity.x, rb2d.velocity.y, 9f), Color.cyan);
        Debug.DrawLine(transform.position, transform.position + new Vector3(0f, 1f, 0f), Color.blue); // Draws headHitCheck
    }

    /**
     * Makes the player crouch/slide by changing the capsule collider and animations
     */
    void crouch()
    {
        capsuleCollider.size = crouchingCapsuleColliderSize;
        capsuleCollider.offset = crouchingCapsuleColliderOffset;
        animator.SetBool("isCrouching", true);
    }

    /**
     * Makes the player stop crouching/sliding by changing the capsule collider and animations
     */
    void unCrouch()
    {
        capsuleCollider.size = standingCapsuleColliderSize;
        capsuleCollider.offset = standingCapsuleColliderOffset;
        animator.SetBool("isCrouching", false);
    }


    /**
     * Makes the player run up slopes without sliding down
     * https://www.youtube.com/watch?v=QPiZSTEuZnw
     */
    void slopeCheck()
    {
        // Ray cast
        Vector2 rayCastOrigin = new Vector2(transform.position.x, transform.position.y + capsuleCollider.offset.y - capsuleCollider.size.y + 0.1f);
        RaycastHit2D hit = Physics2D.Raycast(rayCastOrigin, Vector2.down, slopeCheckCastDistance, platformLayer);

        if (hit)
        {
            // Calculates if player is on a slope or not
            slopeTangent = Vector2.Perpendicular(hit.normal).normalized;
            float slopeAngle = Vector2.Angle(hit.normal, Vector2.up);
            if (Mathf.Abs(slopeAngle) > 0.1f) isOnSlope = true;
            else isOnSlope = false; // If slope is horizontal (normal ground)

            // Rotates player velocity when the slope slightly changes while player is sliding
            float deltaSlopeAngle = Mathf.Abs(Mathf.DeltaAngle(slopeAngle, lastSlopeAngle));
            if (1f < deltaSlopeAngle && deltaSlopeAngle < 46f && isCrouching)
            {
                float velocity = rb2d.velocity.x > 0f ? rb2d.velocity.magnitude : -rb2d.velocity.magnitude;
                rb2d.velocity = -slopeTangent * velocity;
            }

            lastSlopeAngle = slopeAngle;

            Debug.DrawRay(hit.point, hit.normal, Color.green);
            Debug.DrawRay(hit.point, slopeTangent, Color.red);
        }
        else
        {
            isOnSlope = false;
            lastSlopeAngle = 180f;
        }
    }

    #region ColissionCallbacks

    /**
     * Executes once when the player enters the grounded state
     */
    private void OnTriggerEnter2D(Collider2D other)
    {
        if (other.gameObject.layer == LayerMask.NameToLayer("Platform"))
        {
            isGrounded = true;
            if (jumpIsBuffered()) jump();
        }
    }

    /**
     * Executes once when the player leaves the grounded state
     */
    private void OnTriggerExit2D(Collider2D other)
    {
        if (other.gameObject.layer == LayerMask.NameToLayer("Platform"))
        {
            isGrounded = false;
            lastTimeGrounded = Time.time;
        }
    }

    #endregion

    /**
     * Checks if coyote time is valid
     */
    bool validCoyoteTime() => Time.time - lastTimeGrounded <= coyoteTime;

    /**
     * Chects if a jump is buffered
     */
    bool jumpIsBuffered() => Time.time - lastTimePressedJump <= jumpBufferTime;

    /**
     * Clears the jump buffer by setting the timestamp for last time jump was pressed too far back in time
     */
    void clearJumpBuffer()
    {
        lastTimePressedJump = -jumpBufferTime - 1;
    }

    /**
     * Makes the player character jump
     */
    void jump()
    {
        if (Time.time - lastTimeJumped < jumpCooldown) return;
        rb2d.velocity = new Vector2(rb2d.velocity.x, jumpForce);
        clearJumpBuffer();
        lastTimeJumped = Time.time;
    }
}
