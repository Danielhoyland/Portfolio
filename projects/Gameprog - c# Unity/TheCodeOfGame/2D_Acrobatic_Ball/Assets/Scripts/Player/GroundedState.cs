using System.Collections;
using System.Collections.Generic;
using Unity.VisualScripting;
using UnityEngine;
using UnityEngine.InputSystem;

/**
 * Abstract PlayerState for when the player is grounded
 */
public abstract class GroundedState : PlayerState
{
    // Slopes
    private float slopeCheckCastDistance = 0.5f;
    protected Vector2 slopeTangent;
    //private float lastSlopeAngle = 0f;


    #region StateMethods

    public override void Enter()
    {
        SlopeCheck();
        stateMachine.RotateSprite(-stateMachine.LastSlopeAngle);
    }

    public override void Exit()
    {
        stateMachine.LastSlopeAngle = 180f;
    }

    public override void FixedExecute()
    {
        base.FixedExecute();
        stateMachine.ThrowsLeft = 2;
        SlopeCheck();
    }

    #endregion


    #region Input

    /**
     * Jump input
     */
    public override void DoJump()
    {
        Jump();
    }

    #endregion


    /**
     * Executes once when the player leaves the grounded state
     */
    private void OnTriggerExit2D(Collider2D other)
    {
        if (other.gameObject.layer == Utils.ToSingleLayer(stateMachine.PlatformLayer.value))
        {
            stateMachine.RotateSprite(0f);
            stateMachine.SetState(stateMachine.airborneState);
        }
    }


    #region MovementLogic

    /**
     * Makes the player run up slopes without sliding down
     * https://www.youtube.com/watch?v=QPiZSTEuZnw
     */
    private void SlopeCheck()
    {
        // Ray cast
        Vector2 rayCastOrigin = new Vector2(transform.position.x, transform.position.y + stateMachine.capsuleCollider.offset.y - stateMachine.capsuleCollider.size.y + 0.1f);
        RaycastHit2D hit = Physics2D.Raycast(rayCastOrigin, Vector2.down, slopeCheckCastDistance, stateMachine.PlatformLayer);

        if (hit)
        {
            // Calculates if player is on a slope or not
            slopeTangent = Vector2.Perpendicular(hit.normal).normalized;
            float slopeAngle = Vector2.SignedAngle(hit.normal, Vector2.up);

            // Rotates player velocity when the slope slightly changes while player is sliding
            float deltaSlopeAngle = Mathf.Abs(Mathf.DeltaAngle(slopeAngle, stateMachine.LastSlopeAngle));
            if (1f < deltaSlopeAngle && deltaSlopeAngle < 31f)
            {
                float velocity = stateMachine.rb2d.velocity.x > 0f ? stateMachine.rb2d.velocity.magnitude : -stateMachine.rb2d.velocity.magnitude;
                stateMachine.rb2d.velocity = -slopeTangent * velocity;
            }
            if (1f < deltaSlopeAngle && deltaSlopeAngle < 46f) stateMachine.RotateSprite(-slopeAngle);

            stateMachine.LastSlopeAngle = slopeAngle;
            stateMachine.QoyoteSlopeAngle = slopeAngle;

            // Debug visuals for the scene view
            Debug.DrawRay(hit.point, hit.normal, Color.green);
            Debug.DrawRay(hit.point, slopeTangent, Color.red);
        }
        else
        {
            stateMachine.LastSlopeAngle = 180f;
        }
    }

    #endregion
}
