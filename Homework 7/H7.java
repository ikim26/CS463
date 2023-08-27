//Author: Isaac Kim
//CS 463
//Professor Mark Snyder
//April 8, 2023

import java.util.ArrayList;
import java.util.concurrent.*;

//main class
public class H7 {
    //

    //main loop
    public static void main(String[] args) throws InterruptedException {
        int num_players;
        int num_chairs;

        //default parameter is 10 players
        if (args.length == 0){
            num_players = 10;
        }
        else{
            num_players = Integer.parseInt(args[0]);
        }
        num_chairs = num_players-1; //num of chairs is one less than players

        //first print statement
        System.out.println("BEGIN "+num_players+" players");

        //make shared resource for players and emcee (Musical Chair Room Object)
        MCRoom mcroom = new MCRoom(null, null);

        //arraylist of players and chairs
        ArrayList<Player> players = new ArrayList<Player>();
        ArrayList<Chair> chairs = new ArrayList<Chair>();

        //create our chairs and players------------------------------------------
        for(int i = 1; i <= num_chairs; i++){
            Chair chair = new Chair(i);
            chairs.add(chair);
        }

        for(int i = 1; i <= num_players; i++){
            Player player = new Player(i,mcroom);
            players.add(player);
        }

        //add chairs and players to our room-------------------------------------
        mcroom.setPlayers(players);
        mcroom.setChairs(chairs);

        //make emcee class for separate thread requirement
        Emcee emcee = new Emcee(mcroom);
        emcee.start();
    }
}
