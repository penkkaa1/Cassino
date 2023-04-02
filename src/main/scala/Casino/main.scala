package Casino

import scala.io.StdIn.*
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._
import scala.util.{Try, Success, Failure}




@main def run() =
  // GAME STILL NEEDS TO BE PLAYED WITH ROUNDS, 16 POINTS ETC.
  // MULTIPLE DECKS IF MORE THAN ~3 PLAYERS
  // SAVING AND LOADING
  // GUI

  val deck = allCards
  //val deck = Deck(Buffer(twoH, twoD, twoC, twoS, threeH, threeD, threeS, threeC, sixC, sixD, sixS, sixH, sixC, sixD, sixS, sixH))           //different Decks for testing purposes
  //val deck = Deck(Buffer(twoH,threeC,fourH,fiveD,sixD,sevenC,eightS,nineS,tenH,jackC,queenS,kingH,aceD, aceC, eightD, fourS, twoC))

  var playerCountInput = readLine("Please input how many players do you want? (At least 2)\n-> ")

  def tryToGetNumber(input: String) = Try {
    input.toInt                                         //helper function for exception handling
  }

  def falseTest(input: String) = Try {
    input.toInt < 2                                     //helper function for exception handling
  }

  var playerCountTest = tryToGetNumber(playerCountInput)
  var playerCountFalseTest = falseTest(playerCountInput)

  while playerCountTest.isFailure || playerCountFalseTest.get do
    playerCountInput = readLine("Please input how many players do you want? (At least 2)\n-> ")                // checking if user input is valid (2-N players)
    playerCountTest = tryToGetNumber(playerCountInput)
    playerCountFalseTest = falseTest(playerCountInput)
  var playerCount = playerCountInput.toInt

  var players = Buffer[Player]()
  var counter = 1
  for i <- 0 until 1 do
    var playerName = readLine(s"Input the name of the $counter. player : ")
    players += Player(playerName, Hand(deck.selectRandomCards(3)), Stack(Buffer[Card]()) ) // first player gets 3 cards, because they get a card when the game is initialized
    counter += 1

  for i <- 0 until (playerCount - 1) do
    var playerName = readLine(s"Input the name of the $counter. player : ")
    players += Player(playerName, Hand(deck.selectRandomCards(4)), Stack(Buffer[Card]()) ) // rest of players get the average 4 cards
    counter += 1

  val playTableCards = deck.selectRandomCards(4)
  val playTable = Table(playTableCards, players)
  val currentGame = Game(playTable, deck)

  var turn = 0
  var previousPlayer = currentGame.table.players.head
  //!{currentGame.table.players.exists( _.points >= 16 )}

  while currentGame.table.players.exists( p => p.hand.cards.nonEmpty ) do
    breakable {
      for i <- 0 until 1 do

        val currentPlayer = currentGame.table.players(turn % currentGame.table.players.length)
        if currentPlayer.hand.cards.isEmpty then break                                          // if for some reason current player does not have any cards, it skips them
        println(s"\nPlayer is now : ${currentPlayer.name}")

        if currentGame.deck.cards.nonEmpty then
          previousPlayer.hand.addCard(currentGame.deck.selectRandomCards(1).head)                 // as long as there are cards in the deck, add one to the previous players hand

        val handCardsIndexed = currentPlayer.hand.cards.zipWithIndex
        var handCardIndex = 0                                                                   // temporary placeholder
        var userInput = readLine(s"What card do you want to play? Input the number next to the card.\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${currentGame.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "")

        def tryIsThisInRange(input: String) = Try {
          currentPlayer.hand.cards(input.toInt)                                                 // helper function for exceptions
        }

        var numberTest = tryToGetNumber(userInput)
        var rangeTest = tryIsThisInRange(userInput)

        while numberTest.isFailure || rangeTest.isFailure do
          if numberTest.isFailure then
            userInput = readLine(s"\nUnknown number : '$userInput'. Please input the integer next to the card you want to play.\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${currentGame.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "")
          else if rangeTest.isFailure then
            userInput = readLine(s"\nGiven number '$userInput' is not in the acceptable range. Please input the integer next to the card you want to play.\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${currentGame.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "")
          numberTest = tryToGetNumber(userInput)
          rangeTest = tryIsThisInRange(userInput)

        handCardIndex = userInput.toInt

        val handCard = handCardsIndexed.filter( pairs => pairs._2 == handCardIndex).map( pair => pair._1 ).head

        println(s"\nYou selected : $handCard\n")

        if currentGame.table.cards.isEmpty then
          currentPlayer.hand.removeCard(handCard)
          currentGame.table.addCardToTable(handCard)                                              // if table is empty, simply places the chosen card on the table
          println(s"\nPlayer ${currentPlayer.name} places $handCard on the table!")
        else
          val tableCardsIndexed = currentGame.table.cards.zipWithIndex
          var tableCardsIndexes = Buffer[Int]()                                                 //placeholder

          var input = readLine(s"\nWhat card(s) do you want in return? Separate multiple indexes with a comma ','\nCurrent table : ${tableCardsIndexed.mkString(" | ")}\n-> ").trim.replace(" ", "").split(",")

          def tryToGetNumbers(input: Array[String]) = Try {
            input.map( _.toInt)
          }
                                                                                                // more helper functions for exceptions
          def tryIfTheseAreInRange(input: Array[String]) = Try {
            input.map( _.toInt).foreach( i => tableCardsIndexed.apply(i))
          }

          var numbersTest = tryToGetNumbers(input)
          var rangeTests = tryIfTheseAreInRange(input)

          while numbersTest.isFailure || rangeTests.isFailure do
            if numbersTest.isFailure then
              input = readLine(s"\nUnknown numbers : (${input.mkString(", ")}). Please only input the integers next to the cards you want to pick up.\nWhat card(s) do you want in return? Separate multiple card indexes with a comma ','\nCurrent table : ${tableCardsIndexed.mkString(" | ")}\n-> ").trim.replace(" ", "").split(",")
            else if rangeTests.isFailure then
              input = readLine(s"\nAtleast one of the given numbers was not in the acceptable range: (${input.mkString(", ")}).\nWhat card(s) do you want in return? Separate multiple indexes with a comma ','\nCurrent table : ${tableCardsIndexed.mkString(" | ")}\n-> ").trim.replace(" ", "").split(",")
            numbersTest = tryToGetNumbers(input)
            rangeTests = tryIfTheseAreInRange(input)
          tableCardsIndexes = input.map(_.toInt).toBuffer

          //STILL GOT TO IMPLEMENT PICKING UP MULTIPLE PAIRS OF CARDS

          val tableCards = Buffer[Card]()
          tableCardsIndexes.foreach( index => tableCards += tableCardsIndexed.filter( _._2 == index).map( pair => pair._1).head )

          currentGame.humanAlgorithm(currentGame.table, currentPlayer, handCard, tableCards)


        previousPlayer = currentPlayer
        turn += 1
    }

  currentGame.lastPickUpPlayer.addMultipleCardsToStack(playTable.cards)                             // gives the last player to pick up cards the rest of the cards on the table
  println(s"\nPlayer ${currentGame.lastPickUpPlayer.name} was the last player to pick up cards, and so recieved all the rest of the cards from the table!\n")

  def findMax(playerNum: Buffer[(Player, Int)]) =
    var max = 0
    for pair <- playerNum do
      if pair._2 > max then                                                                      // helper function for finding the maximum points
        max = pair._2
    playerNum.filter( p => p._2 == max ).map( pair => pair._1 )

  var playerAces = players.map( p => (p, p.stack.countAces() ) )                                  // finding which player has the most aces, who has the most cards, spades... etc
  if !playerAces.forall( p => p._2 == 0) then findMax(playerAces).foreach( p => currentGame.table.addPoints(p,1) )

  var playerCards = players.map( p => (p, p.stack.countCards() ) )
  if !playerCards.forall( p => p._2 == 0 ) then findMax(playerCards).foreach( p => currentGame.table.addPoints(p,1) )

  var playerSpades = players.map( p => (p, p.stack.countSpades() ) )
  if !playerSpades.forall( p => p._2 == 0 ) then findMax(playerSpades).foreach( p => currentGame.table.addPoints(p,2) )


  players.foreach( p => if p.stack.hasCard(tenD) then currentGame.table.addPoints(p, 2) )           // checking for ten of diamonds, and for two of spades
  players.foreach( p => if p.stack.hasCard(twoS) then currentGame.table.addPoints(p, 1) )

  players.foreach( p => println(s"\nPlayer ${p.name} stack : ${p.stack.gatheredCards.mkString(", ")}\nPoints: ${p.points}") )   // currently for TESTING

  var playersPoints = players.map( p => (p, p.points) )
  var winners = findMax( playersPoints )

  if winners.length > 1 then
    println(s"\n\nPlayers ${winners.map(_.name).mkString(" and ")} have points ${winners.map( p => p.points).mkString(", ")}\nITS A TIE!")
  else
    println(s"\n\nAnd the winner is: ${winners.map(_.name).mkString}!")

end run
