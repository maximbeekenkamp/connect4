# connect4
Connect 4 game with Human vs Human, Human vs AI, and AI vs AI functionality.

Instructions for use, describing how a user would interact with your program (how would someone play your game against a friend? against the AI?)

To use this program, a user would have to open up a terminal and navigate to the source code folder within the game directory.  Then, they would enter the command ‘node Referee.bs.js’ and a 5x7 game board would appear. They would then be prompted to enter their moves on the screen. 

If the user wanted to play against an AI or against another player or pit an AI against an AI, they would need to go into the Referee.re file and change the last few lines of code...

To do Human vs Human:
module R1 = Referee(Connect4.Connect4,
 (HumanPlayer.HumanPlayer(Connect4.Connect4)),
 (HumanPlayer.HumanPlayer(Connect4.Connect4)));
 
R1.playGame();

To do Human vs AI

module R1 = Referee(Connect4.Connect4,
 (HumanPlayer.HumanPlayer(Connect4.Connect4)),
 (AIPlayer.AIPlayer(Connect4.Connect4)));
 
R1.playGame();

To do AI vs AI

module R1 = Referee(Connect4.Connect4,
 (AIPlayer.AIPlayer(Connect4.Connect4)),
 (AIPlayer.AIPlayer(Connect4.Connect4)));
 
R1.playGame();


If the user really wanted to change the game board size, that can also be done within the Referee.re file by changing the final string in line 42 of code

try (gameLoop(CurrentGame.initialState("5 7")))









An overview of how your program functions, including how all of the pieces fit together  

The Referee code is essentially the ‘top-level’ module. Within it, our whole game is running. It’s like the conductor of the orchestra. It instantiates an instance of the game, starting with an empty board, and then runs the game on a loop, always querying the state of the game to ensure that the game isn’t in a terminal state, then transferring the power to take a turn from one instance of a player (Human or AI)  module to another instance of a player module. Before allowing any player to take control of the board, the Referee checks to see the status of the game. After a player makes a move, the Referee generates the next state of the game before transferring to the other player through a recursive call to the gameLoop procedure. 

Our game functions by being able to create a board, access certain areas (columns, rows, cells) and being able to manipulate them. The basic functions of our game allow for the creation of the board, the tokens to be placed in the desired location (abiding by the game mechanics), and for the game to check for whether the game is won, drawn or ongoing. In order to check these different winning conditions we had to write programs which split our board into lists of integers representing rows, columns and diagonals. 




A description of any possible bugs or problems with your program 
 
As far as we can tell, we don’t have bugs/problems.
One idea we had to improve was in estimateValue. We didn’t love how in each estimateValue call, the whole board is assessed. For a giant board (1000x1000), that would take a while...  A better estimateValue would look only at the 7x7 section of the board around the piece that was placed and increment past estimateValues up based on the change in that area of board. That would likely make it more efficient. 


A list of the people with whom you collaborated 
Maxim Beekenkamp, Chance Emerson   


A description of any extra features you chose to implement 
We did not implement any extra features.
