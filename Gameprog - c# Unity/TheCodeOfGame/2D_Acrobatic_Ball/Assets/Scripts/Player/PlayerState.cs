using System.Collections;
using System.Collections.Generic;
using Unity.VisualScripting;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.SceneManagement;

/**
 * Abstract state class for player states implementations to inherit from
 */
public abstract class PlayerState : MonoBehaviour
{
    // Reference to state machine
    protected PlayerStateMachine stateMachine;


    private void OnEnable()
    {
        if (stateMachine == null) stateMachine = GetComponent<PlayerStateMachine>();
    }


    #region StateMethods

    public virtual void Enter() { }
    public virtual void Exit() { }
    public virtual void Execute() {
        HandleShouldStartAiming();
        HandleShouldStopAiming();
        HandleCurrentlyAiming();
        HandleBallThrow();
    }
    public virtual void FixedExecute() {
        
    }

    #endregion


    #region Input
    
    public virtual void DoJump() { }
    public virtual void DoCrouch() { }

    #endregion


    #region MovementLogic

    /**
     * Horizontal movement / running logic
     * https://www.youtube.com/watch?v=KKGdDBFcu0Q
     */
    protected void UpdateVelocity(
        float movementInput, 
        float movementSpeed, 
        float accelerationRate,
        Vector2 movementDirection, 
        float animationVelocityXValue)
    {
        float targetVelocity = movementInput * movementSpeed * movementDirection.x;
        float velocityDifference = targetVelocity - stateMachine.rb2d.velocity.x;
        float movementForce = velocityDifference * accelerationRate;

        stateMachine.rb2d.AddForce(movementForce * movementDirection);
        stateMachine.animator.SetFloat("velocity_x", animationVelocityXValue);

        Debug.DrawRay(transform.position, movementForce * movementDirection, Color.magenta);
    }

    /**
     * Makes the player character jump
     */
    protected void Jump()
    {
        if (Time.time - stateMachine.LastTimeJumped < PlayerStateMachine.JUMP_COOLDOWN) return;

        float slopeAngleMultiplier = Mathf.Abs(Mathf.Cos(stateMachine.QoyoteSlopeAngle * Mathf.Deg2Rad));
        float jumpForce = stateMachine.rb2d.velocity.y <= 0
            ? PlayerStateMachine.JUMP_FORCE
            : (PlayerStateMachine.JUMP_FORCE + stateMachine.rb2d.velocity.y) * slopeAngleMultiplier;
        
        stateMachine.rb2d.velocity = new Vector2(stateMachine.rb2d.velocity.x, Mathf.Max(jumpForce, stateMachine.rb2d.velocity.y));

        stateMachine.ClearJumpBuffer();
        stateMachine.LastTimeJumped = Time.time;
    }

    #endregion

    IEnumerator SmoothStepTimeScaleOverTime(float startValue, float endValue, float duration)
    {
        float elapsedTime = 0f;

        while (elapsedTime < duration)
        {
            elapsedTime += Time.unscaledDeltaTime;

            Time.timeScale = Mathf.SmoothStep(startValue, endValue, elapsedTime / duration);

            yield return null;
        }

        Time.timeScale = endValue; // Ensure the value is exactly the end value when the interpolation is done
    }

    #region BallLogic
    void HandleCurrentlyAiming()
    {
        // Updates the visual line when aiming
        if (stateMachine.currentlyAiming)
        {
            var aimVector = new Vector3();

            if (stateMachine.input_UsingKeyboardMouse)
            {
                // Update the aim-vector.
                var playerPos = transform.position;
                var mousePos = stateMachine.input_adjustAimPointerPos;
                // We update the aim-vector.
                var camPos = Camera.main.transform.position;
                var mouseWorldPos = Camera.main.ScreenToWorldPoint(new Vector3(mousePos.x, mousePos.y, Mathf.Abs(camPos.z)));
                var directionVector = mouseWorldPos - playerPos;
                directionVector /= PlayerStateMachine.THROW_MAX_VISUAL_DISTANCE;
                if (directionVector.magnitude > 1) { directionVector.Normalize(); }
                aimVector = directionVector;
            } else
            {
                aimVector = stateMachine.input_adjustAimGamepadDir;
                if (aimVector.magnitude > 1) aimVector.Normalize();
            }

            stateMachine.input_currentAimVector = aimVector;
            var arr = new Vector3[]{
                stateMachine.transform.position,
                stateMachine.transform.position +
                new Vector3(aimVector.x, aimVector.y, 0) * PlayerStateMachine.THROW_MAX_VISUAL_DISTANCE
            };
            stateMachine._aiming_LineRenderer.material = new Material(Shader.Find("Sprites/Default"));
            stateMachine._aiming_LineRenderer.material.color = Color.magenta;
            stateMachine._aiming_LineRenderer.SetPositions(arr);
            
        }

    }

    void HandleShouldStartAiming()
    {
        if (stateMachine.input_shouldStartAiming && !LevelManager.instance.isPaused && stateMachine.ThrowsLeft > 0)
        {
            stateMachine.input_shouldStartAiming = false;

            if (stateMachine.ball != null)
            {
                // We want the ball to return to the player
                stateMachine.ball.BeginReturnToPlayer();
            }
            else if (!stateMachine.currentlyAiming && !LevelManager.instance.isPaused)
            {
                Time.timeScale = stateMachine.GetAimingTimeScale();
                stateMachine.currentlyAiming = true;
                stateMachine._aiming_LineRenderer = gameObject.AddComponent<LineRenderer>();
                stateMachine._aiming_LineRenderer.startWidth = 0.5f;
                stateMachine._aiming_LineRenderer.endWidth = 0.5f;
            }
        }
    }

    void HandleShouldStopAiming()
    {
        if (stateMachine.input_shouldStopAiming && !LevelManager.instance.isPaused)
        {
            stateMachine.input_shouldStopAiming = false;
            if (stateMachine.currentlyAiming)
            {
                stateMachine.currentlyAiming = false;
                Destroy(stateMachine._aiming_LineRenderer);
                stateMachine._aiming_LineRenderer = null;
                StartCoroutine(SmoothStepTimeScaleOverTime(0.1f, 1f, stateMachine.GetAimingTimeScale()));
            }
        }
    }

    void HandleBallThrow()
    {
        if (stateMachine.shouldThrowBall)
        {
            stateMachine.shouldThrowBall = false;

            if (stateMachine.ball != null)
            {
                // Teleport to ball
                var ballPos = stateMachine.ball.transform.position;
                var ballVelocity = stateMachine.ball.GetComponent<Rigidbody2D>().velocity;
                Destroy(stateMachine.ball.gameObject);
                stateMachine.ball = null;

                /*RaycastHit2D hit = Physics2D.Linecast(transform.position, ballPos);

                if (hit.collider != null)
                {
                    if (hit.collider.CompareTag("Enemy"))
                    {
                        
                        if (hit.collider.GetComponent<Enemy>() != null)
                        {
                            hit.collider.GetComponent<Enemy>().death = true;
                        }
                        else if (hit.collider.GetComponent<WizardEnemy>() != null)
                        {
                            hit.collider.GetComponent<WizardEnemy>().death = true;
                        }
                        
                    }
                }*/

                var playerRb = GetComponent<Rigidbody2D>();
                playerRb.position = ballPos + new Vector3(0f, 0.5f, 0f);

                // If player teleports to a place with only 1 unit in height
                RaycastHit2D hitUp = Physics2D.Raycast(playerRb.position, Vector2.up, 1f, stateMachine.PlatformLayer);
                if (hitUp) stateMachine.SetState(stateMachine.crouchingState);

                var toBall = ballPos - transform.position;
                playerRb.velocity = toBall.normalized * ballVelocity.magnitude;

                stateMachine.teleportSound.Play();
            }
            else if (stateMachine.currentlyAiming)
            {
                // If the aiming vector is zero, we can't throw.
                if (stateMachine.input_currentAimVector.magnitude < 0.05)
                {
                    return;
                }

                // If we already have a ball, we can't throw. (We shouldn't even be aiming?)
                if (stateMachine.ball != null)
                {
                    Debug.LogError("Tried to throw ball when we already had one, think this is an error?");
                    return;
                }

                stateMachine.ballThrowSound.Play();

                // Spawn the ball and throw it.
                // Instantiate prefab.
                var ballGameObj = Instantiate(stateMachine.GetBallPrefab(), transform.position, Quaternion.identity);
                if (ballGameObj == null)
                {
                    Debug.LogError("Unable to instantiate the Ball game object");
                }
                stateMachine.ball = ballGameObj.GetComponent<Ball2>();
                if (stateMachine.ball == null)
                {
                    Debug.LogError("Instantiated ball prefab does not have Ball script.");
                }
                stateMachine.ball.player = stateMachine;
                var aimVectorClamped = stateMachine.input_currentAimVector;
                if (aimVectorClamped.magnitude > 1) aimVectorClamped.Normalize();

                var playerVelocity = GetComponent<Rigidbody2D>().velocity;

                stateMachine.ball.GetComponent<Rigidbody2D>().velocity = 
                    aimVectorClamped * stateMachine.GetMaxInitialBallVelocity() +
                    playerVelocity;

                stateMachine.ThrowsLeft = stateMachine.ThrowsLeft - 1;
                stateMachine.input_shouldStopAiming = true;
            }
        }
    }
    #endregion
}
