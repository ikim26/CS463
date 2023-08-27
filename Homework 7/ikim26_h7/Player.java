//Author: Isaac Kim
//CS 463
//Professor Mark Snyder
//April 8, 2023

import java.util.ArrayList;
import java.util.concurrent.*;

//class for player
public class Player extends Thread{
    //player attributes
    private Chair chair;
    private String id;
    private MCRoom mcroom;
    private volatile int cont;

    //default constructor (takes in int for id and makes string name out of it)
    public Player(int id, MCRoom mcroom){
        this.id = "P"+id;
        this.mcroom = mcroom;
        this.chair = null;
        this.cont = 1;
    }

    //getter for id----------------------------------------------------------------
    //(no need for setter since player ID shouldn't change)
    public String getID(){
        return this.id;
    }

    //getter and setter for chair--------------------------------------------------
    public Chair getChair(){
        return this.chair;
    }

    public void setChair(Chair chair){
        this.chair = chair;
    }

    //method to find chair once music stops
    //synchronized bc this is the method that uses concurrency
    //(players looking for chairs at the same time)
    public synchronized Chair findChair(){
        int index = 0;
        //find first available chair
        while(index < mcroom.getChairs().size()){

            //if chair we try to sit in is free (not taken)
            //sit in chair (first come, first serve)
            if(!mcroom.getChairs().get(index).sit()){

                //return the chair we sit in
                return mcroom.getChairs().get(index);
            }
            index++;
        }
        //if we reach here, we looked at all chairs, but all were taken
        return null;
    }

    //method to stop running the last player thread
    public void end(){
        this.mcroom.congratulations();  //call music off one more final time to wake up last player
        this.cont = 0;
    }

    //run method
    public void run(){
        while(this.cont == 1){
            //(this is essentially "playing musical chairs" when music is on)
            this.mcroom.waitForMusicOff();

            //if this player is last man standing
            if(this.cont == 0){
                break;
            }

            //once we reach here, music is turned off. Find a chair!
            setChair(findChair());

            //if we didn't find a chair, send player id to eliminated and stop thread
            if(getChair() == null){
                mcroom.setEliminated(getID());
                break;
            }
            //otherwise, print we found a chair
            else{
                System.out.println(this.id + " sat in " + this.chair.getID());
            }
        }
    }
}