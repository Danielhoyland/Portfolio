using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;

/**
 * PlayerState for when the player is grounded and chrouching
 */
public class CrouchingState : GroundedState
{
    // Movement values
    [SerializeField] private float movementSpeed = 2f;
    [SerializeField] private float acceleration = 1.5f;
    [SerializeField] private float deceleration = 0.5f;
    [SerializeField] private float slidingUpGravity = 3f;
    [SerializeField] private float slidingDownGravity = 6f;

    // CapsuleCollider (sliding/crouching)
    private Vector2 standingCapsuleColliderSize;
    private Vector2 standingCapsuleColliderOffset;
    private Vector2 crouchingCapsuleColliderSize;
    private Vector2 crouchingCapsuleColliderOffset;

    private float stateEnterTime = 0f; // To fix bug where player teleports to a place they cant stand up


    private void Start()
    {
        // Capsule collider values for crouching and not crouching
        standingCapsuleColliderSize = stateMachine.capsuleCollider.size;
        standingCapsuleColliderOffset = stateMachine.capsuleCollider.offset;
        crouchingCapsuleColliderSize = new Vector2(standingCapsuleColliderSize.x, standingCapsuleColliderSize.y / 2f);
        crouchingCapsuleColliderOffset = new Vector2(standingCapsuleColliderOffset.x, standingCapsuleColliderOffset.y - 0.25f);
    }


    #region StateMethods

    public override void Enter()
    {
        stateEnterTime = Time.time;

        base.Enter();

        stateMachine.capsuleCollider.size = crouchingCapsuleColliderSize;
        stateMachine.capsuleCollider.offset = crouchingCapsuleColliderOffset;

        stateMachine.animator.SetBool("isCrouching", true);
    }

    public override void Exit()
    {
        base.Exit();

        stateMachine.capsuleCollider.size = standingCapsuleColliderSize;
        stateMachine.capsuleCollider.offset = standingCapsuleColliderOffset;

        stateMachine.animator.SetBool("isCrouching", false);

        stateMachine.slidingParticles.Stop();
    }

    public override void FixedExecute()
    {
        base.FixedExecute();

        // Velocity
        float accelerationRate = stateMachine.rb2d.velocity.magnitude > movementSpeed ? deceleration : acceleration;
        float animationVelocityXValue = Mathf.Abs(stateMachine.rb2d.velocity.x) <= movementSpeed + 0.1f ? 0f : stateMachine.rb2d.velocity.x;
        UpdateVelocity(stateMachine.MovementInput, movementSpeed, accelerationRate, -slopeTangent, animationVelocityXValue);

        // Gravity
        if (stateMachine.rb2d.velocity.y < 0) stateMachine.rb2d.gravityScale = slidingUpGravity;
        else stateMachine.rb2d.gravityScale = slidingDownGravity;

        UpdateSlidingParticles();

        // Safe to stop crouching?
        if (!stateMachine.CrouchIsPressed && (Time.time - stateEnterTime > 0.02f))
        {
            HeadSmackCheck();
        }
    }

    #endregion


    #region MovementLogic

    /**
     * Checks if the players head is going to hit a roof if the player stops crouching
     */
    private void HeadSmackCheck()
    {
        RaycastHit2D hit = Physics2D.Raycast(transform.position, Vector2.up, 1f, stateMachine.PlatformLayer);
        if (!hit) stateMachine.SetState(stateMachine.standingState);

        Debug.DrawLine(transform.position, transform.position + new Vector3(0f, 1f, 0f), Color.blue);
    }

    #endregion


    #region Particles

    /**
     * Updated the sliding particle effect
     */
    void UpdateSlidingParticles()
    {
        float minSPeed = movementSpeed * 2f;

        if (stateMachine.slidingParticles.isPlaying)
        {
            if (stateMachine.rb2d.velocity.magnitude < minSPeed)
            {
                stateMachine.slidingParticles.Stop();
                return;
            }
            SetSlidingParticleParams();
        }
        else if (stateMachine.rb2d.velocity.magnitude >= minSPeed)
        {
            stateMachine.slidingParticles.Play();
            SetSlidingParticleParams();
        }
    }

    /**
     * Sets the rate of particle emission based on player velocity
     */
    void SetSlidingParticleParams()
    {
        // Number of particles spawned
        var emission = stateMachine.slidingParticles.emission;
        emission.rateOverTime = Mathf.Pow(stateMachine.rb2d.velocity.magnitude, 2f) * 0.1f;

        // Particle start speed
        var particleSystem = stateMachine.slidingParticles.main;
        particleSystem.startSpeedMultiplier = stateMachine.rb2d.velocity.magnitude * 0.5f;
    }

    #endregion
}