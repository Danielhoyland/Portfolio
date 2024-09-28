# Submission

Groupmembers and individual reports:

* [Odin Aas](Reports/Odin.md)
* [Harald Andreas Løkkeberg Hansen](Reports/HaraldAndreas.md)
* [Daniel Høyland](Reports/Daniel.md)
* [Nils Petter Skålerud](Reports/NilsPetter.md)

## Gameplay video

Gameplay video of our game can be found [here](https://youtu.be/ip2IkXrGydI).


## Work distribution
| Code | Odin | Daniel | Harald Andreas | Nils |
| :--- | :---: | :----: | :---: | :---: |
| Ball | Some | Touched | None | Most |
| Player | Touched | Touched | Most | Some |
| Enemy | None | All | None | None |
| Audio | None | Half | Half | None |
| UI | Half | Half | None | None |
| Main Menu | Half | Some | Some | None |
| Camera | None | None | All | Touched |


## Development

Development has gone quite well, although we have had large issues with the version history (gitlab). The merge conflicts are incredibly hard to fix without straight up reverting parts of the commit, as for some reason Unity changes certain font files every time you use them (among other things) causing extremely large, unnecessary merge conflicts.

### Game Engine

We chose the unity engine as most of our group already had some experience with this engine, and it was also very easy to learn. 

It proved to be a good choice as it was somewhat easy to get into, and the UI was straightforward. There were also a lot of systems that were already implemented, such as the input system.

Although it was not all good, as a little into the semester it was revealed that Unity would charge money per download, this caused us to reevaluate our use of the engine. We ultimately ended up continuing the development using Unity as we had gotten too far with our planning of the project. Future projects of ours would probably be created in another engine of this reason.

#### Strengths of Unity

The first strength of Unity for our game is the easablility to learn. Some of us on the group as said ealier had some experience with Unity allready, and some did not on the group. The Unity layout, the gameobject making and scripting was pretty easy to understand for all of us really. When we met a problem that we didn't know how to do, we could just go to youtube and get a turtorial for either something similar, or the exact soltuion we needed since Unity is really main stream, or was atleast. One example is the rebinding buttons in our settings. We had no clue how to firstly get the keys, manage to ribind and make sure it is saved. With a tutorial we managed to get a pretty good rebinding menu.

Another strength with Unity is the Asset store from other people and Unity's own released assets/scripts. Many of our textures in our game is from free assets from Unity asset store, which have been a huge help. Additionally in the last example about the rebinding menu, we used allready premade code from Unity to rebind with Unity's new rebinding system, with small modifications with the help of the allready fortold tutorial. 

Another strength of Unity is that all of the options that we have with this engine. There is an own audio mixer we can edit, use and save. There is a particle system to more easily manage to add particle effects to our game. There is and own animation system where we can easily add events to make something happen in our game based upon the animation. The prefab system helping us haveing the same made assets to our diffrent scenes. 

#### Weaknesses of Unity
The first weakness of Unity that came in the news recently is the new price changes that Unity tried to make. They removed the changes, but it made us wonder if we should just change engine beacause of it, but this don't give us any future trust in this engine. If we were to make another game it would probably not be Unity again since they may easily change their mind in the future again.

Another weakness is the non-compability with Git-lab. We got many times merge conflicts from Unity for random reasons like fonts that we didn't touch got changed or something. This was frustrating to try to see what to keep or not and made us somethime revert some progress we made.

Multi-compability with mouse and keyboard or else. It was very non-intuative to work with Unity to try to add controller support to our game. We wanted to add controller support since this looks like a good game to have controller to play it. Sometimes it worked, sometimes it didn't. We thought we had implemented it good, and tried to make it intuative for both keyboard and mouse and controller, but the controller would randomly stop working. In the end we had to drop controller support since all of the problems with it we couldn't fix. 

Additionally the game sometime would work in editor but wouldn't in build. This is more like an irritating issue that we don't get any error messages before building it and it wouldn't work. 

Sometime else that really irrated us, was the boot up time Unity used each time we booted it up. We sometime closed and open Unity to check if something we tried work, if we got a bug or else, and Unity could sometimes take like up to 5 minutes to boot which is not ideal. This is not that big of a weakness, but is still an irritation and a little hindrance when we are trying to show something we had meetings. 

### Process and Communication


#### Process

We did not use a spesific development method for this project, but our process ended up somewhat similar to Scrum and Kanban without the issue board and with less documentation.

We spent some time at the start of the semester writing a design and art document to make sure everyone was on the same page about what we wanted the game to be. These documents specified:

- The structure and pace of the game
- What gameplay mechanics we wanted
- Which art direction we wanted to go for and the setting of the game
- Which games we were inspired by for reference
- That we wanted a lot of juice in the game :D

Finding new tasks under development bacame relatively easy with the help of these documents. Because of this, we managed fine wihout an issue board, but an issue board would have helped us being more organized, especially when we were closing in on the deadline.

#### Meetings

We have very consistently had a meeting each Friday physicaly at campus that lasted around 2-4 hours. On some occasions we had to move the meeting or cancel it due to course work and deadlines in other cources. We also had extra meetings on some occasions when we felt like it was nessesary.

Each weekly meeting usualy consisted of the following:

- Each group member presented what they had worked on since the last meeting.
- We merged code that was ready to be merged, tested the game to make sure everything still worked, and fixed issues and bugs we noticed. More on this the section about [version control](#version-control).
- We discussed the state of the game, what the game needed at that point, and planed what each group member should work on until the next meeting. Each group member decided for themselves what they would like to work on of what was available.
- We usualy also used a good portion of the duration to continue to work on tasks that were not finished yet, or to start working on our new tasks.

#### Communication

We primarily communicated through a discord server outside of meetings. The discord server was used to:
- Plan time and place for the weekly meeting
- Getting help with technical problems
- Give small updates when nessesary
- Sharing assets
- Hold digital meetings on rare occasions

### Version Control

As said earlier we used Gitlab to control our version control. Here we had a system with the branches to try to minimize merge conficts. We had the Main branch, from that we made a branch named. dev-branch. Dev-branch would be the branch we would merge the diffrent things we made each week when we had meetings. We would make other branches as insinuated by the last sentence from dev-branch for the diffrent things we made that week. When we in the meeting be finished merging and testing and checking the to see if everything works and looks good, then we would merge to main since main would be an allways working branch. We would repeat the prosess then to branch out again from main and contiune working. This prosess work very well and it made sure that we allways had some code that worked even if we managed to corrupt of delete a lot from dev-branch.

Under development we heard that we should use the issue board on gitlab since then:
- easy to track tasks
- makes accountability so its easier to prove if someone isn't working
- good development prosess that companies uses

Even so when we heard that we should use it we were already over half way through the semester, so we didn't adopt having the issue board as a ticket tracking. We just contiuned by finding out what needed to be done each meeting and taking some of the tasks of what needed to be done and try to do it by next meeting. Of course we know that this isn't a good system or at least a good routine for later, but it worked for us well enough this semester. The closest thing we had to a ticket system at all was making a list of what needed to be done on discord closer to the delivery date.