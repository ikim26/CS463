//Author: Isaac Kim
//CS 463
//Professor Mark Snyder
//April 8, 2023

import java.util.ArrayList;
import java.util.concurrent.*;

//class for shared resource for emcee and players (simulate the musical chair room)
public class MCRoom{
	//class vars
	private ArrayList<Player> players;
    private ArrayList<Chair> chairs;
    private volatile String eliminated;

	//default constructor
	public MCRoom(ArrayList<Player> players, ArrayList<Chair> chairs){
		this.players = players;
		this.chairs = chairs;
	}

	//getter and setter for players-----------------------------------------------------
	public ArrayList<Player> getPlayers(){
		return this.players;
	}

	public void setPlayers(ArrayList<Player> players){
		this.players = players;
	}

	//getter and setter for chairs------------------------------------------------------
	public ArrayList<Chair> getChairs(){
		return this.chairs;
	}

	public void setChairs(ArrayList<Chair> chairs){
		this.chairs = chairs;
	}

	//getter and setter for eliminated player--------------------------------------------
	public String getEliminated(){
		return this.eliminated;
	}

	public void setEliminated(String id){
		this.eliminated = id;
	}

	//method to put player thread to sleep (play musical chairs while music is on)
    //(since wait() can only be called from a synchronized context)
    public synchronized void waitForMusicOff(){
        try {
            wait(); //(this is essentially "playing musical chairs" when music is on)
        }
        catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    //method to turn music off and wake up player threads
	//(since notifyAll() has to be called from a synchronized context)
	public synchronized void musicOff(){
		System.out.println("music off");
		notifyAll();
	}

	//method to wake up last player remaining
	public synchronized void congratulations(){
		notifyAll();
	}
}