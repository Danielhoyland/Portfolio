using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using Unity.VisualScripting;
using UnityEngine;
using UnityEngine.InputSystem;

/**
 * PlayerState for when the player is not grounded
 */
public class AirborneState : PlayerState
{
    // Movement values
    [SerializeField] private float movementSpeed = 15f;
    [SerializeField] private float acceleration = 1.5f;
    [SerializeField] private float deceleration = 0f;
    [SerializeField] private float defaultGravityScale = 3f;
    [SerializeField] private float fallingGravityScale = 6f;

    // Events
    public delegate void OnGroundedEnter(Vector2 currentVelocity, Vector2 prevoiusVelocity);
    public static event OnGroundedEnter OnGrounded;


    #region StateMethods

    public override void Enter()
    {
        stateMachine.LastTimeGrounded = Time.time;
        stateMachine.rb2d.gravityScale = stateMachine.rb2d.velocity.y < 0f ? fallingGravityScale : defaultGravityScale;

        stateMachine.animator.SetBool("isGrounded", false);
    }

    public override void Exit()
    {
        if (stateMachine.JumpIsBuffered()) Jump();

        stateMachine.animator.SetBool("isGrounded", true);
    }

    public override void Execute()
    {
        base.Execute();
        stateMachine.animator.SetFloat("velocity_y", stateMachine.rb2d.velocity.y);
    }

    public override void FixedExecute()
    {
        base.FixedExecute();
		
        stateMachine.PreviousVelocity = stateMachine.rb2d.velocity; // For ground hit camera shake

        // Velocity
        UpdateVelocity(stateMachine.MovementInput, movementSpeed, CalculateAccelerationRate(), Vector2.right, stateMachine.rb2d.velocity.x);

        // Gravity
        stateMachine.rb2d.gravityScale = stateMachine.rb2d.velocity.y < 0f ? fallingGravityScale : defaultGravityScale;
    }

    #endregion


    #region MovementLogc

    /**
     * Uses player input and player velocity to decide acceleration rate
     */
    private float CalculateAccelerationRate()
    {
        bool movementInput = Mathf.Abs(stateMachine.MovementInput) > 0.1f;

        bool inputAndVelocityXIsNegative = stateMachine.MovementInput < -0.1f && stateMachine.rb2d.velocity.x < -0.1f;
        bool inputAndVelocityXIsPositive = stateMachine.MovementInput > 0.1f && stateMachine.rb2d.velocity.x > 0.1f;
        bool inputAndVelocityDirectionMatch = inputAndVelocityXIsNegative || inputAndVelocityXIsPositive;

        bool overSpeedLimit = Mathf.Abs(stateMachine.rb2d.velocity.x) > movementSpeed;

        return !movementInput || (overSpeedLimit && inputAndVelocityDirectionMatch) ? deceleration : acceleration;
    }

    #endregion


    #region Input

    /**
     * Jump input
     */
    public override void DoJump()
    {
        if (stateMachine.ValidCoyoteTime()) Jump();
        else stateMachine.LastTimePressedJump = Time.time;
    }

    #endregion


    /**
     * Executes once when the player enters the grounded state
     */
    private void OnTriggerEnter2D(Collider2D other)
    {
        if (other.gameObject.layer == Utils.ToSingleLayer(stateMachine.PlatformLayer.value))
        {
            OnGrounded?.Invoke(stateMachine.rb2d.velocity, stateMachine.PreviousVelocity);
            InitGroundHitParticles(stateMachine.rb2d.velocity - stateMachine.PreviousVelocity);
            

            stateMachine.SetState(stateMachine.CrouchIsPressed ? stateMachine.crouchingState : stateMachine.standingState);
        }
    }

    /**
     * Instantiates a particle effect when the player hits the ground
     */
    void InitGroundHitParticles(Vector2 velocityDifference)
    {
        float speedLimit = 30f;
        if (velocityDifference.magnitude < speedLimit) return;

        Vector3 position = new(
            stateMachine.transform.position.x,
            stateMachine.transform.position.y - stateMachine.capsuleCollider.size.y - stateMachine.capsuleCollider.offset.y + 0.2f,
            stateMachine.transform.position.z);

        GameObject groundHitParticles = Instantiate(
            stateMachine.groundHitParticlesPrefab, 
            position,
            stateMachine.groundHitParticlesPrefab.transform.rotation);

        ParticleSystem particleSystem = groundHitParticles.GetComponent<ParticleSystem>();

        var emission = particleSystem.emission;
        float velocityMultiplier = Mathf.Pow(velocityDifference.magnitude - speedLimit, 2f) * 0.05f;
        emission.SetBurst(0, new ParticleSystem.Burst(0f, velocityMultiplier));
        var ps = particleSystem.main;
        ps.startSpeedMultiplier = velocityDifference.magnitude * 0.2f;

        particleSystem.Play();

        stateMachine.groundHitSound.volume = (velocityDifference.magnitude - speedLimit*0.1f) / 100f;
        stateMachine.groundHitSound.Play();
    }
}