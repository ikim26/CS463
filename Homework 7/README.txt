Homework 7 Write-up by Isaac Kim

1. The shared resource that represents the music is MCRoom class since this class contains the wait() and notifyAll() methods that represent turning the music on and off respectively.
The Emcee turns on the music in Emcee.java line 70, where the Emcee thread goes to sleep to allow the player threads to start (play musical chairs).
The Emcee turns off the music in Emcee.java line 77 by calling the musicOff method in MCRoom.java line 64. The notifyAll() call signifies the actual music turning off, which is in MCRoom.java line 66.
I created a Chair object to represent each Chair, with an id and taken attribute, representing the id of the chair and a boolean representing whether or not a chair has been sat in, respectively.
The chairs themselves are created in a for loop in H7.java in lines 38-41.

2. The strategy for finding chairs was simply to loop through the arraylist of chairs and find the first empty chair. Since every player starts their search at the beginning of the array and ends at the end of the array, every chair is guaranteed to get a player since num_of_players = num_of_chairs + 1.

3. Some challenges I faced were:
	- Figuring out how to implement the simulation without having a global lock on ALL the chairs at one given time. I initially created a lock for the entire array of chairs, but since that would violate that requirement, I resorted to a synchronized method that "claimed" a chair as soon as a player attempted to sit in it, fixing the global lock part since multiple players could access different chairs at the same time now (just not the same chair).
	- Deciding what the "shared resource" would be. However, since I took OS (CS 471) before this class, I remembered quickly that the wait() and notify() methods should be part of the same lock structure. I decided that this would be the MCRoom class.
	- Designing my classes/objects structure and how they would interact. I started with creating default constructors, getter and setters, and for threads, their run() methods, and then pieced the rest of the assignment together.
	- Having a blank terminal after running. This wasn't a big challenge, but it was something that I had to deal with because I didn't end the last "player" thread when they had won, hence the hanging thread and blank terminal.
The competition for resources happened mainly in different threads being scheduled to run (concurrency) and players (threads) trying to sit in chairs. 
There was a need for cooperation when trying to find an empty chair because when a player "claims" a chair, they cannot "claim" the same chair as another player.
There was also some cooperation between the Emcee and player threads, since the player threads all had to wait before being signaled that they can find chairs. Also, the emcee had to wait before all the players found their chairs to wake up again.

4. Because of my previous experience in concurrency from classes like CS 367 and CS 471, identifying how I wanted to make the player threads run concurrently was rather straightforward to me. I knew that the concurrent part of the code would be roughly equivalent to the wait() and signal() calls in C. The "players can access different chairs at the same time" requirement was straightforward because of this.
Another thing that was straightforward was creating the player, emcee and chair objects since it was clearly stated in the instructions that these had to be separate, and that really helped in my final design/structure of the assignment.
Most of the difficult or laborious tasks I faced were outlined in question 3. It goes without saying that debugging was a laborious task (as always with concurrency).

5. Some bugs I ran into were players sitting in the same chair (which was solved by players "claiming" or locking a chair if the chair they checked was empty) and hanging threads/blank terminal. Another bug I had was an ordering issue where the music would turn off before players would actually start(). However, I never ran into a deadlock since I had experience in concurrency before and had a good idea of where to put my wait() and notifyAll() calls. 
When I ran into the hanging thread issue, I recognized that it was simply a thread that was still "waiting" and didn't get to finish. Because of the way I handle eliminating players once they lose a round, it was not very difficult to find that the problem was that the winner player ("thread") would not properly terminate (it was still waiting for the next round). This was a simple fix by notifying the thread one more final time.
As for the ordering issue, I fixed that by putting the Emcee thread to sleep once I had finished calling start() on all the players so the players would get a chance to run.
I did not use any debuggers when debugging my code, just simple debug print statements.

6. Nope :)




