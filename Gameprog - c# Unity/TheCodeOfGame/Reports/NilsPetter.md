# Nils Petter Skålerud
Most of my time was spent on the ball-mechanic, integrating controller inputs and making a physics-based floating platform.

# Rubric
|Description | Weight |
|----|----|
|Gameplay video | 20 |
|Code video | 0 |
|Good Code  | 20
|Bad Code | 10
|Development process | 20 |
|Reflection | 30 |

# Good code: Ball-throwing
Initially Odin made a very simple implementation of throwing the ball, and I wanted to improve upon it.
Here are some improvements I made:

**Slowing down time while aiming**
This includes using a coroutine to smoothly speed time back up.

**Summoning of ball back to the player**
This lets the player call on the ball after it is thrown, making the ball move smoothly towards the player, while 
passing through all objects. This moves the ball in a direct line towards the player,  continuously accelerating. 

One issue I encountered here was related to the ball moving past the player when accumulating enough 
speed. Initially, I only checked the distance between the ball and the player to determine whether the ball had finished moving towards the player. The issue with this approach is that the  ball would end up moving past the distance threshold within one physics tick, it would then try to go back towards the player but at an even higher speed and therefore move beyond the play. This would repeat and end up with the ball ping-ponging on either side of the player every tick, infinitely. The solution I used to solve this was to use a vector dot-product to determine whether the ball had moved past the player. This essentially drew a line between the player and the balls initial position when recalling begun, it would then compare that line to the balls current position relative to the player. It could then determine whether the ball had move past the player.

Code taken from `Ball2.cs`
```c#
// Vector towards player
var toPlayer = player.transform.position - transform.position;

var playerPos2 = new Vector2(player.transform.position.x, player.transform.position.y);
var ballReturnStart2 = new Vector2(returningToPlayer_startPos.x, returningToPlayer_startPos.y);
var ballPos2 = new Vector2(transform.position.x, transform.position.y);

// We check if the ball has completely passed the player relative to the initial summoning position
var playerToBallStart = (ballReturnStart2 - playerPos2).normalized;
var playerToBallNow = (ballPos2 - playerPos2).normalized;
var dot = Vector2.Dot(playerToBallNow, playerToBallStart);
bool hasPassedPlayer = dot <= 0;
if (hasPassedPlayer)
{
    // Destroy ourselves and trigger event on Player.
    player.OnBallReturn();
    Destroy(gameObject);
    BallReturnedToPlayer?.Invoke();
    player.ballCatchSound.Play();
    return;
}
```
 
**Automatically spawning of the ball**
Initially, the ball needed to be placed into the scene and have the reference be set manually in the player-script 
component in the inspector. This approach makes the player mechanics work immediately only by placing the player prefab in the scene, with minimal setup.

# Good code: Physics-based floating platform
I wanted the game levels to feel more dynamic and respond more to the player. I designed a simple floating platform
that would be held in place using physics, but not in a completely rigid manner. This platform could then either be used for a platform that would only float in place, or you could make it move between two points. The platform works by having a "target-point" where it wants to be, and will continuously apply physics forces to push it there. The further away the platform is from this target point, the stronger it will push. Furthermore, the platform is not rotationally locked, but has a maximum allowed rotation.

This results in the platform being fairly dynamic and fun to use. It moves according to impacts when the player lands on it. I would also describe it as fun. We also found other possible usecases for it, by configuring it some more we 
were able to use it as a trampoline.

**Editor scripting for end-point**
Moving between the two points is done with a simple sine function based on the time since the platform became awake. 
This makes it moves smoothly between a start-point and an end-point. The interesting part here is that I wanted it to 
be simple to author where the end-point should. I wanted to be able to see where the end-point was in the Scene View, not just view the raw coordinates in the inspector. I ended up learning about Editor scripting this way. I wrote a script that created an extra transform handle in the scene viewer that let you move the end-point around the scene.

Code taken from `PhysicsPlatformEditor.cs`
```c#
// Draw a handle to move the point
EditorGUI.BeginChangeCheck();
var newTargetPos = Handles.PositionHandle(currTargetPos, Quaternion.identity);
// Draw a small sphere as a gizmo
Handles.SphereHandleCap(
	0,
	newTargetPos,
	Quaternion.identity,
	0.5f,
	EventType.Repaint);

// Draw debug text
GUIStyle textStyle = new GUIStyle();
textStyle.normal.textColor = Color.grey;
textStyle.fontSize = 16;
var labelPosition = newTargetPos + Vector3.down * 0.2f + Vector3.left * 2.0f; // Adjust the offset as needed
Handles.Label(labelPosition, "Target pos", textStyle);

Handles.color = Color.cyan;
Handles.DrawLine(platform.transform.position, newTargetPos);
```

# Bad code: Integrating controller
My personal envisioning of our game was that it would be primarily played with controller, but also allow you to play
with keyboard+mouse. I spent quite a bit of time working with the input system to get this working. On some occasions
it would work correctly, but there was often times where I had issues getting Unity to even pick up the events from the controller at all. Ultimately, we would remove the controller integration completely from the final version of the game
since it was so unreliable.

**Different input types for aiming**
One particular design issue I ran into was how controller and mouse had different types of input data when it came to aiming. When aiming with a controller, we used the joystick to get a normalized Vector2 type in coordinate space `[-1, 1]`.  When aiming with the mouse, we are receiving a screenspace coordinate in the coordinate space  `[0, width]`, which then had to be projected into world-space  and compared against the player position  to determine the strength and direction of the current aiming vector. It's basically the issue of whether the input type  is a pointer or a joystick. This caused issues in terms of the aiming becoming inconsistent once you changed input device, the aiming vector would suddenly move towards a very unexpected direction.

What I tried to do was setup two separate inputs: `OnAdjustAimDirection` and `onAdjustAimPointer`.  I would then track which type of input was most recently used to determine which one to rely on. This certainly helped but there was still edge cases that I was unable to solve.

In general I had many issues fighting with the input system over this.

Code taken from `Player/PlayerState.cs`
```   
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
			// This code path has since been removed.
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
```

# Reflections
I wish I had spent more time on this project this semester, sadly I  had quite a lot to do in other subjects. If I had more time to work on this project, there are particularly some things I would focus on:

- **Isolate what mechanics are required to make a playable prototype**
 Among the large amounts of ideas we had for this project, it was hard to quite isolate the minimal subset of gameplay mechanics was needed to turn this into a playable and fun game. During the project I felt I neglected this part of the design process and instead focused on implementing the "low-hanging fruit" mechanics that I was somewhat confident would be needed. Given more time, I would have spent more effort giving critical thought to which mechanics we would need to prioritize.

 - **Focus more on level design**
I think our game had much potential, but the lack of proper levels made it hard to showcase. Given more time, I would focus more on creating levels that felt more polished and coherent, while also focusing on making the levels incentivize the player to combine their abilities to get through. This would include portions that would force the player to use the ball-throwing mechanic in specific ways and introduce very simple puzzle elements. Additionally, I felt the game became a lot more engaging once the floating platforms were in place. Moving forwards, I would include more ideas that focus on dynamic level design. This could include rotating platforms, platform that diseappear after a short time after being touched, and also a platform where you would need to use the sticky-ball (not implemented) to hang onto the platform while the platform moves you to a new location.


