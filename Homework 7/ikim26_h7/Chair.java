//Author: Isaac Kim
//CS 463
//Professor Mark Snyder
//April 8, 2023

import java.util.ArrayList;
import java.util.concurrent.*;

//class for chair
public class Chair{
    private String id;
    private boolean taken;

    //default construstor (takes in int for id and makes string name out of it)
    public Chair(int id){
    	this.id = "C"+id;
    	this.taken = false;
    }

    //getter for Chair id--------------------------------------------------------
    //(no need for setter since chair ID shouldn't change)
    public String getID(){
    	return this.id;
    }

    //methods to change taken status-----------------------------------------
    //note: false = no player is sitting in chair, true = player is sitting in chair
    //Also, if sit() is called for first time by a player, said player commits to sitting in chair ("claims" chair)
    public synchronized boolean sit(){
    	if(!this.taken){
    		this.taken = true;
    		return false;
    	}
    	return this.taken;
    }

    public void setStatus(boolean status){
    	this.taken = status;
    }
}
