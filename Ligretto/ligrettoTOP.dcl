definition module ligrettoTOP

import ligretto
import iTasks

derive class iTask Player, Color, Hand, Card

/**	play_ligretto = task: 
		@task plays a single game of Ligretto.
		The result task value is the winner of that game.
		This task is suited as an element of BasicAPIExamples in which it is assumed that each player is invited by
		the person starting up this task.
*/
play_ligretto :: Task Color

/** play_ligretto_as nr_of_players color = task:
		@task is a game of Ligretto for a player with given @color.
		@nr_of_players is the number of participating players in the game.
		This task is suited as a stand-alone version of Ligretto in which it is assumed that each player enters the
		game on their own initiative.
*/
play_ligretto_as :: NrOfPlayers Color -> Task Color
