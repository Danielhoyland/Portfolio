 
# Daniel Høyland


[<- Back to group report](../gameprog.md)

## Score weighting
| Description | my weight |
| :----:|:----:|
| Gameplay video        | 15 |
| Code video            | 10 |
| Good Code             | 20 |
| Bad Code              | 20 |
| Development process   | 20 |
| Reflection            | 15 |

## Intro
The main coding I did was mostly around making "external" entities that can be used on a few or multiple levels. Others in the group were much more engaged in building the basis for mechanics, so I didn't get to touch very much of that but gave feedback around what could be improved, changed, or removed.

## Good code
In the beginning of the project, I made [`WizardEnemy`](../2D_Acrobatic_Ball/Assets/Scripts/WizardEnemy.cs), which is a ranged enemy that shoots [`FireBall`](../2D_Acrobatic_Ball/Assets/Scripts/FireBall.cs) at the player. I was still new to making assets in Unity by this point and had only added one game object before this. I am very proud of how I managed to make this game object. I found the free assets for the `WizardEnemy` animations and textures. I started with just a single frame with the idle animation to make the script. In the script, the `WizardEnemy` tries to look after the player and shoot a fireball towards the player. It will turn based on the direction of the shooting. I also implemented a way to kill the `WizardEnemy` so we could more easily implement a method to "fight" against the enemies, but that plan was scrapped later because of too little remaining time. I tried to do it myself a little earlier but didn't manage to do it. The `FireBall` has also its own script where if it hits the player, it will kill the player. Additionally, it will destroy itself if it crashes into a game object like a player or into terrain. It will also destroy itself if it's outside of the camera view of the player, so no unintentional lag or other stuff may happen.

The biggest problem I had with the `WizardEnemy` was the animation and of how to time the `FireBall` shooting. Since the asset just had the frames of the animation and this was my first time, it made it difficult for me in the beginning. I managed to make it look decent, but I noticed that either the animation or the shooting of the `FireBall` was delayed. I tried to use delays in the code, but it didn't work (obviously). After a while, I found out about the event option in the animation itself to run a function. I rewrote some of the script, adjusted the values in the script, for example, where the fireball spawns, and adjusted the animation to look better. I did use events to animate the death of the `WizardEnemy` also. One thing I would have improved is the texture/animation of the fireball, since right now it's just a red circle.

I also made the [`Spike`](../2D_Acrobatic_Ball/Assets/Scripts/Spike.cs) script for our game. The spike script is pretty simple but had one a little more complicated part. In the beginning of making the spikes, I used game objects, but it wasn't really intuitive when we wanted to add `Spike` to a level. I tried then to make the script look for a specific tile name that we had in one of our tile palettes. It was a little tricky in the start since I didn't know what to do, but managed to find out in the end. This made it very easy to add `Spike` to every level since that tile in the palette was for that purpose. I think the code for the `Spike` is pretty optimized and can't really be much improved from what it is right now the way I implemented it into our game at least.

The [`SlowBox`](../2D_Acrobatic_Ball/Assets/Scripts/SlowBox.cs) is very similar to the `Spike`. I used a lot of what I learned from the `Spike` in addition to using code parts for the `SlowBox`. It uses a singular tile from the tile palette also. There were really only two parts I had to find out, first how to reduce the velocity of the player and second, how to replace the box with nothing afterwards. The second part was pretty easy to do, but the first one was more difficult. I didn't know if we stored the velocity of the player any place or even where it would be and how to adjust that velocity. I found out that the velocity of the player was stored in the rigid body of the player game object. After that, it was very easy to implement and test.

## Bad code
I did also, of course, some bad code like how a programmed [`Door`](../2D_Acrobatic_Ball/Assets/Scripts/Door.cs). `Door` is a script to be able to have a closed section basically. The idea was that the player had to activate a [`PressurePlate`](../2D_Acrobatic_Ball/Assets/Scripts/PressurePlate.cs) with the ball to open the `Door`. It works but isn't very intuitive. I used a specific tile position to know where the door, or the uppermost of a 2x2 door. That is hardcoded. This isn't very intuitive in the sense that it must be a 2x2 door, additionally to having to check both the starting coordinate and what coordinate to end on. The `PressurePlate` must also have a specific door set in the inspector. The `PressurePlate` is also a game object instead of a tile or tile position. This is very unintuitive really. Changes I would make firstly to the `Door` is to make it much more intuitive in the inspector. I would ask for how many tiles to the left, right up and down should be considered the door. Additionally, I would try to remove the `PressurePlate` script and game object and replace it with the coordinate of where it should be activated. Another thing is a `Door` animation. Right now it just teleports to the new position. It would make it much cooler if it was actually moving slowly.

Another thing I would like to improve is [`Enemy`](../2D_Acrobatic_Ball/Assets/Scripts/Enemy.cs). Enemy is a walking enemy that tries to chase after the player. It works fine for tracking and moving towards the player. Additionally, it will not start to move before it's in the player's view, so it's more intuitive for the player. The really bad thing about the enemy is when it tries to go up a slope. It works kind of, but it's really buggy. I tried to use code similar to the player, but it wouldn't really work. I tried multiple things, tried new things, but in the end, got something that worked. The thing is that code didn't have in mind the round ball of the player that can interact with the `Enemy`. `Enemy` gets pushed way too easily backward. It also has a bigger problem than that again with the ball. If it tries to "walk up" the ball like a slope, there is a very high probability chance that `Enemy` would begin to levitate a little over the ground permanently for the rest of that level. It's not very game-breaking, but it doesn't look good and it should actually work as a player would think it will.

## Video
The code video from me can be found [here](https://youtu.be/0qox6FNj2u4).

## Personal reflection
In the beginning of the semester when we started this course, I didn't really know how to make any game really. I have played many games so I know kind of good standards and know at least what I would enjoy as a gamer. I learned mostly practical aspects from this course by doing the project, but I got some reaffirmation about good game design.

### Concepts from project
I didn't really think that game programming structuring would be very different from normal structures, but in game programming, it's very useful to have an actually good structure when developing a game to be able to use objects, scripts, and textures multiple times more easily. Additionally to this it's the scene structure. It wasn't really hard to understand the concept of this of disabling and enabling UI objects and organization of different objects connected to each other.

Another practical concept is the importance of visual clarity with animations and color. By programming and testing, it was really noticeable when an animation was delayed, from for example `WizardEnemy`. The ball would begin to travel towards the player, and about 1/3 of the way to the player most of the time, the animation would show. This made me confused as a player as I tested. I would not think/see the projectile in time or I would time my jump wrongly to try to dodge it.

Generally how to use Unity. Like I said earlier, I haven't done any game programming with any engine before. I got to find out about Unity's sound management, animation management, public made assets and scripts, and the system overall to make a scene, objects, and having inputs.

### Concepts from lectures

From lectures I learned about how to use and edit prefabs in Unity. I Also learned good concepts like the importance of visual clarity. I remember that we watched a Youtube video in the lecture that showed us examples of almost industry standards. It should be relatively easy for the player to know what they can and can't interact with. This is extra important if it's a more realistic game since it's harder to differentiate objects in games like that.

Good Camera movements are extremely important and usually, the camera may be the whole job for a person in the industry. Like in our game, we want the player to be able to see the ball but we can't zoom out forever so we have to limit it somehow. When the player is moving we could also make a design choice like have the camera a little in front of the player, etc.

Additionally, it's the concept of fair vs. fun gameplay. Some bugs might be a fun addition to the game, but it depends on what game we are making. If it's a multiplayer competitive game, we have to make it as fair as possible, while a single-player game might be better to have the more fun aspect. Additionally to this is the good and bad randomness. Randomness might make a game fun, but it may really easily make it frustrating. So there are multiple ways to do it based on what would be best, but I think the most common "fair" method is the "drawing from a bag" method. This method in a TL;DR form is like saying there is a 2/5 chance to win, but when they lose they remove one of the losses so now the chances are 2/4, until they win. When they win then the "bag is reset to 2/5. It's at least a good concept to take with since randomness is a big part of many types of games.

Other than that, not that much. I learned some Unity basics but, I really understood it when I began using Unity practically.

### Reflection coclusion
I think I got to learn a lot from mostly the game engine concepts, how to use it, but also some more game theory aspects or good game design. I would say I worked pretty consistent in this subject for the project. I liked it a lot, but I know that I have a lot more to learn about game creation and good game design if I ever want to get even better in making a game. I'm pretty sure that I will use these skills in the future for probably a hobby game, so what I have learned now will absolutely be used in some way.
