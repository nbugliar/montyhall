#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details The game setup replicates the game
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'    indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}

#' @title
#'  Selecting the Door
#' @description
#'  By entering in `select_door()` a number is returned showing the door selection.
#' @details
#'  The number is the first door selection out of three available doors which the
#'  contestent selects to win the prize
#' @param
#'  Three numbers stored in a container to `doors` vector, with randomized selection
#'  and one returned number.
#' @return
#'  The function returns a length 1 number indicating the door selection number.
#' @examples
#'  select_door()
#' @export
select_door <- function()
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Open the Goat Door
#' @description
#'  The contestant is given the opportunity to stay  with their initial choice of door, or
#'  switch. But before that the Host, Monty Hall, eliminates one of the three doors by revealing
#'  one of the two goats `open_goat_door()`.
#' @details
#'  The code diverges, because if the contestants have already  and unknowingly selected the car,
#'  the game will drop either of the remaining doors which contain goats.  If they have unknowingly
#'  not selected the car from the first pick, then the game selects only the one remaining door with
#'  a goat to reveal - they will never reveal the car!
#' @param
#'  Two if statements govern the selection of the doors: One if the initial pick was a car, the other
#'  for when the initial pick is not.
#' @return
#'  A number is returned as the goat door number which has been opened
#' @examples
#'  open_goat_door()
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'  Changing the Door
#' @description
#'  `change_door()` governs making a new door selection after 1/2 goats has been revealed, and two doors remain.
#' @details
#'  The contestant has the option to change their selection, or stay with their initial selection.
#' @param
#'  Two if statements govern the final pick vector based on the selection to stay or not.
#' @return
#'  A final pick is returned, a number between 1 and 3.
#' @examples
#'  change_door()
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#'  Determining the Winner
#' @description
#'  `determine_winner()` lets the contestant know whether they have won the car, or lost the game
#'  with a goat waiting for them at their door of choice.
#' @details
#'  Using the previous information for the final pick, and the creation of the game, the if statements make the
#'  determination of whether or not the door corresponds with the car or goat.
#' @param
#'  final.pick is used from the previous `change_door()` command.
#' @return
#'  "WIN" or "LOSE" depending on the outcome
#' @examples
#'  determine_winner()
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'  Play the Entire Game
#' @description
#'  `play_game()` runs an entire start-to-finish iteration of Let's Make a Deal.
#' @details
#'  The entire iteration in one run command allows someone to explore immediately
#'  if staying or switching door choices was in their benefit.
#' @param
#'  The iteration features create_game(), select_door(), open_goat_door(), change_door(),
#'  and determine_winner() to fill vectors related to switching or staying
#' @return
#'  A dataframe is returned with two columns for strategy and outcome, and two rows for STAY/SWITCH
#'  and WIN/LOSE
#' @examples
#'  play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Play the Entire Game 100 Iterations
#' @description
#'  `play_n_games()` runs an entire start-to-finish iteration of Let's Make a Deal for 100 times.
#' @details
#'  The entire iteration in one run command allows someone to explore immediately
#'  if staying or switching door choices was in their benefit over 100 iterations to
#'  draw conclusions on which strategy was better.
#' @param
#'  The iteration features create_game(), select_door(), open_goat_door(), change_door(),
#'  and determine_winner() to fill vectors related to switching or staying
#' @return
#'  A dataframe is returned with two columns for strategy and outcome, and 200 rows for STAY/SWITCH
#'  and WIN/LOSE
#' @examples
#'  play_n_games()
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
