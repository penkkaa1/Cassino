package Casino

import scala.io.StdIn.*
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ListBuffer




@main def run() =



  var startingInput = readLine(s"\nHello, welcome to Cassino. \nNOTE: This version of Cassino is called 'deck-cassino'. Find out more at 'https://plus.cs.aalto.fi/studio_2/k2023/studioproject/109/' \nStart a new game by entering '/start'\nLoad a previous game by entering '/load'\n-> ")

  def isMatch(input: String): String =
    input.split(" ").headOption match
      case Some("/load")  => "load"
      case Some("/start") => "mozambique"
      case _ => "invalid"

  var matchTest = isMatch(startingInput)

  while matchTest == "invalid" do
    startingInput = readLine(s"\nUnknown command '$startingInput'\nStart a new game by entering '/start'\nLoad a previous game by entering '/load'\n-> ")
    matchTest = isMatch(startingInput)

  if matchTest == "load" then
    loadGameFromFile()
    System.exit(0)

  println("\nStarting a new game!\n")                                   // everything below here is for starting a new game, for loaded games check 'loader.scala'

  var originalDeck = allCards
  //var originalDeck = Deck(Buffer(twoH,threeC,fourH,fiveD,sixD,sevenC,eightS,nineS,tenH,jackC,queenS,kingH,aceD, aceC, eightD, fourS, twoC))
  //val originalDeck = Deck(Buffer(twoH, twoD, twoC, twoS, threeH, threeD, threeS, threeC, sixC, sixD, sixS, sixH, sixC, sixD, sixS, sixH))           //different Decks for testing purposes
  var deck = Deck(originalDeck.cards.toBuffer)

  var playerCountInput = readLine("Please input how many players do you want? (At least 2)\n-> ")

  def tryToGetNumber(input: String) = Try {
    input.toInt                                         //helper function for exception handling
  }

  def falseTest(input: String) = Try {
    input.toInt < 2                                     //helper function for exception handling
  }

  def findMax(playerNum: Buffer[(Player, Int)]) =
    var max = 0
    for pair <- playerNum do
      if pair._2 > max then                             // helper function for finding the maximum points
        max = pair._2
    playerNum.filter( p => p._2 == max ).map( pair => pair._1 )

  var playerCountTest = tryToGetNumber(playerCountInput)
  var playerCountFalseTest = falseTest(playerCountInput)

  while playerCountTest.isFailure || playerCountFalseTest.get do
    playerCountInput = readLine("Please input how many players do you want? (At least 2)\n-> ")                // checking if user input is valid (2-N players)
    playerCountTest = tryToGetNumber(playerCountInput)
    playerCountFalseTest = falseTest(playerCountInput)
  var playerCount = playerCountInput.toInt

  var players = Buffer[Player]()
  var playerName = readLine(s"Input the name of the 1. player : ")
  players += Player(playerName, Hand(Buffer[Card]()), Stack(Buffer[Card]()) )

  var counter = 2
  for i <- 0 until (playerCount - 1) do
    var playerName = readLine(s"Input the name of the $counter. player : ")
    players += Player(playerName, Hand(Buffer[Card]()), Stack(Buffer[Card]()) )
    counter += 1

  var playTableCards = Buffer[Card]()
  var playTable = Table(playTableCards, players)
  var currentGame = Game(playTable, deck)                                                                     // creating actual game etc.
  var playersPoints = players.map( p => (p, p.points) )

  var turn = -1
  var previousPlayer = currentGame.table.players.head

  println("\n------------------------------------------------------------------------------------")
  println("NOTE : You can pause and save the game at anytime with '/halt'")
  println("------------------------------------------------------------------------------------")


    while !{currentGame.table.players.exists( _.points >= 16)} do {       // a new round if no player has 16 points
      playersPoints = players.map( p => (p, p.points) )                   // update player points
      deck = Deck(originalDeck.cards.toBuffer)                            // reset deck
      turn += 1                                                           // change starting player
      playTableCards = deck.selectRandomCards(4)                          // reset the table cards
      playTable = Table(playTableCards, players)                          // reset table
      currentGame = Game(playTable, deck)                                 // reset the game
      println(s"\nA new round begins! Current points: \n${playersPoints.map( p => (p._1.name, p._2)).mkString("\n")}")
      for player <- players do
        player.stack = Stack(Buffer[Card]())                              // reset players stack so that they dont get multiple points
        player.hand = Hand(deck.selectRandomCards(4))                     // reset players hand

      while currentGame.table.players.exists( p => p.hand.cards.nonEmpty ) do   // do until cards run out
        breakable {
          for i <- 0 until 1 do

            val currentPlayer = currentGame.table.players(turn % currentGame.table.players.length)
            if currentPlayer.hand.cards.isEmpty then
              turn += 1                                     // if for some reason current player does not have any cards, it skips them. mainly used for loaded games.
              break
            println(s"\nPlayer is now : ${currentPlayer.name}")

            if currentGame.deck.cards.nonEmpty && previousPlayer.hand.cards.length < 4 then
              previousPlayer.hand.addCard(currentGame.deck.selectRandomCards(1).head)               // as long as there are cards in the deck, add one to the previous player's hand

            val handCardsIndexed = currentPlayer.hand.cards.zipWithIndex
            var handCardIndex = 0                                                                   // temporary placeholder
            var userInput = readLine(s"What card do you want to play? Input the number next to the card you want to play from your hand.\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${currentGame.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "")

            while userInput == "/halt" do
              halt(players, turn, deck, originalDeck, currentGame.table.cards)
              println(s"\nPlayer is now : ${currentPlayer.name}")
              userInput = readLine(s"What card do you want to play? Input the number next to the card you want to play from your hand.\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${currentGame.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "")

            def tryIsThisInRange(input: String) = Try {
              currentPlayer.hand.cards(input.toInt)                                                 // helper function for exceptions
            }

            var numberTest = tryToGetNumber(userInput)
            var rangeTest = tryIsThisInRange(userInput)

            while numberTest.isFailure || rangeTest.isFailure do                                    // exception handling. cases when input is not a number, or number is not in accetable range
              if numberTest.isFailure then
                userInput = readLine(s"\nUnknown number : '$userInput'. Please input the number next to the card you want to play from your hand.\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${currentGame.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "")
              else if rangeTest.isFailure then
                userInput = readLine(s"\nGiven number '$userInput' is not in the acceptable range. Please input the number next to the card you want to play from your hand.\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${currentGame.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "")
              while userInput == "/halt" do
                halt(players, turn, deck, originalDeck, currentGame.table.cards)
                println(s"\nPlayer is now : ${currentPlayer.name}")
                userInput = readLine(s"What card do you want to play? Input the number next to the card you want to play from your hand.\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${currentGame.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "")
              numberTest = tryToGetNumber(userInput)
              rangeTest = tryIsThisInRange(userInput)

            handCardIndex = userInput.toInt

            val handCard = handCardsIndexed.filter( pairs => pairs._2 == handCardIndex).map( pair => pair._1 ).head

            println(s"\nYou selected : $handCard\n")

            if currentGame.table.cards.isEmpty then
              currentPlayer.hand.removeCard(handCard)
              currentGame.table.addCardToTable(handCard)                                              // if table is empty, simply places the chosen card on the table
              println(s"Player ${currentPlayer.name} places $handCard on the table!")
            else
              val tableCardsIndexed = currentGame.table.cards.zipWithIndex
              var tableCardsIndexes = Buffer[Int]()                                                   // placeholder

              var input = readLine(s"What card(s) do you want in return? Separate multiple numbers with a comma ','\nCurrent table : ${tableCardsIndexed.mkString(" | ")}\n-> ").trim.replace(" ", "").split(",")

              def tryToGetNumbers(input: Array[String]) = Try {
                input.map( _.toInt)
              }
                                                                                                      // more helper functions for exceptions
              def tryIfTheseAreInRange(input: Array[String]) = Try {
                input.map( _.toInt).foreach( i => tableCardsIndexed.apply(i))
              }

              var numbersTest = tryToGetNumbers(input)
              var rangeTests = tryIfTheseAreInRange(input)

              while numbersTest.isFailure || rangeTests.isFailure do                                   // exception handling. cases where input is not a number, and when atleast one number is not in acceptable range
                if numbersTest.isFailure then
                  input = readLine(s"\nUnknown numbers : (${input.mkString(", ")}). Please only input the numbers next to the cards you want to pick up.\nWhat card(s) do you want in return? Separate multiple numbers with a comma ','\nCurrent table : ${tableCardsIndexed.mkString(" | ")}\n-> ").trim.replace(" ", "").split(",")
                else if rangeTests.isFailure then
                  input = readLine(s"\nAtleast one of the given numbers was not in the acceptable range: (${input.mkString(", ")}).\nWhat card(s) do you want in return? Separate multiple numbers with a comma ','\nCurrent table : ${tableCardsIndexed.mkString(" | ")}\n-> ").trim.replace(" ", "").split(",")
                numbersTest = tryToGetNumbers(input)
                rangeTests = tryIfTheseAreInRange(input)
              tableCardsIndexes = input.map(_.toInt).toBuffer


              val tableCards = Buffer[Card]()
              tableCardsIndexes.foreach( index => tableCards += tableCardsIndexed.filter( _._2 == index).map( pair => pair._1).head )     // add chosen cards to tableCards for the humanAlgorithm

              currentGame.humanAlgorithm(currentGame.table, currentPlayer, tableCards, handCard)        


            println("\n------------------------------------------------------------------------------------")
            previousPlayer = currentPlayer                                                    // update previous player for picking up new cards
            turn += 1
        }
      currentGame.lastPickUpPlayer.addMultipleCardsToStack(playTable.cards)                             // gives the last player to pick up cards the rest of the cards on the table
      println(s"\nPlayer ${currentGame.lastPickUpPlayer.name} was the last player to pick up cards, and so recieved all the rest of the cards from the table!\n")

      var playerAces = players.map( p => (p, p.stack.countAces() ) )                                  // finding which player has the most aces, who has the most cards, spades... etc
      if !playerAces.forall( p => p._2 == 0) then findMax(playerAces).foreach( p => p.addPoints(1) )

      var playerCards = players.map( p => (p, p.stack.countCards() ) )
      if !playerCards.forall( p => p._2 == 0 ) then findMax(playerCards).foreach( p => p.addPoints(1) )

      var playerSpades = players.map( p => (p, p.stack.countSpades() ) )
      if !playerSpades.forall( p => p._2 == 0 ) then findMax(playerSpades).foreach( p => p.addPoints(2) )

      players.foreach( p => if p.stack.hasCard(tenD) then p.addPoints(2) )           // checking for ten of diamonds, and for two of spades
      players.foreach( p => if p.stack.hasCard(twoS) then p.addPoints(1) )

      players.foreach( p => println(s"Player ${p.name} stack : ${p.stack.gatheredCards.mkString(", ")}\nPoints: ${p.points}\n") )   // currently for testing, might keep it in anyway

    }

    playersPoints = players.map( p => (p, p.points) )                   // update player points one final time

    var winners = findMax(playersPoints)

    if winners.length > 1 then
      println(s"\n\nPlayers ${winners.map(_.name).mkString(" and ")} have points ${winners.map( p => p.points).mkString(", ")}\nITS A TIE!")
    else
      println(s"\n\nAnd the winner is: ${winners.map(_.name).mkString}!")

end run



