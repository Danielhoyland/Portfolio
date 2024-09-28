using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/**
 * PlayerState for when the player is grounded and not chrouching
 */
public class StandingState : GroundedState
{
    // Movement values
    [SerializeField] private float movementSpeed = 15f;
    [SerializeField] private float acceleration = 3f;
    [SerializeField] private float deceleration = 8f;


    #region StateMethods

    public override void Enter()
    {
        base.Enter();

        stateMachine.rb2d.gravityScale = 0f;
    }

    public override void Exit()
    {
        base.Exit();

        stateMachine.footStepSound.Stop();
    }

    public override void Execute()
    {
        base.Execute();

        if (Mathf.Abs(stateMachine.MovementInput) > 0.1f) stateMachine.footStepSound.Play();
        else stateMachine.footStepSound.Stop();
    }

    public override void FixedExecute()
    {
        base.FixedExecute();

        float accelerationRate = Mathf.Abs(stateMachine.MovementInput) > 0.1f ? acceleration : deceleration;
        UpdateVelocity(stateMachine.MovementInput, movementSpeed, accelerationRate, -slopeTangent, stateMachine.MovementInput);
    }

    #endregion


    #region Input

    /**
     * Crouch input
     */
    public override void DoCrouch()
    {
        stateMachine.SetState(stateMachine.crouchingState);
    }

    #endregion
}
