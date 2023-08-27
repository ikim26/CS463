//Author: Isaac Kim
//CS 463
//Professor Mark Snyder
//April 8, 2023

import java.util.ArrayList;
import java.util.concurrent.*;

//class for Emcee
public class Emcee extends Thread{
	//class vars
	private MCRoom mcroom;

	//default constructor
	public Emcee(MCRoom mcroom){
		this.mcroom = mcroom;
	}

	//helper method to retrieve index of player by id
	private int getIndexOfPlayer(String id){
		for (int i = 0; i < mcroom.getPlayers().size(); i++){
        	if (mcroom.getPlayers().get(i).getID() == id){
            	return i;
        	}
		}
		//if we reach here, player doesnt exist
		return -1;
	}

	//helper method to reset chairs and players
	private void reset(){
		//get size of chairs and players
		int num_chairs = this.mcroom.getChairs().size();
		int num_players = this.mcroom.getPlayers().size();

		//reset chairs status
		for(int i = 0; i < num_chairs; i++){
			//false = no player is sitting in chair
			this.mcroom.getChairs().get(i).setStatus(false);
		}
		//clear chairs from players
		for(int i = 0; i < num_chairs; i++){
			this.mcroom.getPlayers().get(i).setChair(null);
		}
	}

	//run method
	public void run(){
        //start players
        int num_players = this.mcroom.getPlayers().size();
        for(int j = 0; j < num_players; j++){
        	//System.out.println("started Player " + (j+1));
        	this.mcroom.getPlayers().get(j).start();

        	//Note: Players only need to start once since the thread loops while they are still in the game
        	//      and must not be recreated each round (requirement)
        }

        //start round
		int num_chairs = this.mcroom.getChairs().size();

        //play as many rounds as there are chairs
        for(int i = 1; i <= num_chairs; i++){
            //print round number
            System.out.println("round "+i);

            //MUSIC TURNS ON NOW----------------------------------------------------
            //put emcee thread to sleep
        	try {
        	    sleep(1000); //wait 1000ms for "music to play" (player threads to start)
        	}
        	catch (InterruptedException e) {
        	    e.printStackTrace();
        	}

            //TURN MUSIC OFF---------------------------------------------------------
            this.mcroom.musicOff();

            //put emcee thread to sleep
        	try {
        	    sleep(1000); //wait 1000ms for players to find their chairs
        	}
        	catch (InterruptedException e) {
        	    e.printStackTrace();
        	}

        	//once we are here, every player should have found their chair
        	//print out the loser of the round
        	System.out.println(mcroom.getEliminated() + " lost\n");

        	//remove the loser
        	mcroom.getPlayers().remove(getIndexOfPlayer(mcroom.getEliminated()));

        	//reset chairs and players
        	reset();

        	//remove a chair (namely the last chair)
        	this.mcroom.getChairs().remove(this.mcroom.getChairs().size()-1);
        }

        //if we reach here, only one player should remain
        //end the last remaining player's thread and print out winner
        mcroom.getPlayers().get(0).end();
        System.out.println(mcroom.getPlayers().get(0).getID() + " wins!");
        System.out.println("END");
	}
}