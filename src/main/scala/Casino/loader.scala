package Casino

import scala.io.StdIn.*
import scala.collection.mutable.Buffer
import scala.util.control.Breaks.*
import scala.util.{Failure, Success, Try}
import java.io.{BufferedReader, FileReader, IOException, Reader, FileNotFoundException, FileWriter}

class CorruptedFileException(message: String) extends Exception(message)


var players = Buffer[Player]()
var turn = 0
var deck = Deck(Buffer[Card]())
var tableCards = Buffer[Card]()
var origDeck = Deck(Buffer[Card]())


def loadGame(input: Reader) =
  var atLeast2players = false
  var turnDecided = false
  var deckAdded = false
  var origDeckAdded = false

  val lineReader = BufferedReader(input)

  try

    var currentLine = lineReader.readLine().trim.toLowerCase

    if !(currentLine.startsWith("save")) then
      throw new CorruptedFileException("\nReason : Unable to read save file, did not find 'save' keyword")

    currentLine = lineReader.readLine()
    while currentLine != null do
      var currentChunk = Array[String]()
        if currentLine.startsWith("#") then
          currentChunk = currentChunk :+ currentLine
          currentLine = lineReader.readLine()
          while currentLine != null && !currentLine.startsWith("#") do
            currentChunk = currentChunk :+ currentLine
            currentLine = lineReader.readLine()
          currentChunk = currentChunk.filter( !_.isBlank ).map( _.trim )
          val chunkHeader = currentChunk.headOption
          chunkHeader.foreach({
            _.toLowerCase match
              case "#game metadata" =>  var playerAmount = countPlayers(currentChunk)
                                        var playersAndNum = getPlayerNamesAndNumbers(currentChunk)
                                        for p <- playersAndNum do
                                           players += Player(p._1, Hand(Buffer[Card]()), Stack(Buffer[Card]()))
                                        if players.length >= 2 then atLeast2players = true else throw CorruptedFileException(s"\nReason : Did not find 2 players\nCurrent players : (${players.map(_.name).mkString(", ")}).")
              case "#turn"          =>  val numOption = currentChunk.last.toIntOption       // maybe an int, check if it is
                                        numOption match
                                          case n: Some[Int] => turn = n.get                 // if an int, get it
                                          case _            => throw CorruptedFileException(s"\nReason : Turn number invalid, '$numOption'")
                                        if turn < 0 then throw CorruptedFileException(s"\nReason : Turn number cannot be negative : ${turn}")   // turn cannot be negative
                                        turnDecided = true
              case "#deck"          =>  val cards = getCards(currentChunk)
                                        deck = Deck(cards)
                                        deckAdded = true
              case "#origdeck"      =>  val cards = getCards(currentChunk)
                                        if cards.length < ((players.length * 4) + 4) then throw CorruptedFileException(s"\nReason : Not enough cards in original deck to start a new round!\nOriginal deck length : ${cards.length}\nAmount needed to start a new round : ${players.length * 4} + 4 for the table.")
                                        origDeck = Deck(cards)
                                        origDeckAdded = true
              case "#table"          => val cards = getCards(currentChunk)
                                        tableCards = cards
              case "#player1"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack1"        =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points1"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player2"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack2"        =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points2"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player3"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack3"        =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points3"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player4"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack4"        =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points4"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player5"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack5"        =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points5"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player6"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack6"        =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points6"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player7"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack7"        =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points7"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player8"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack8"        =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points8"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player9"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack9"        =>  var num = getPlayerNum(currentChunk.head, false)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points9"       =>  var num = getPlayerNum(currentChunk.head, false)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player10"       =>  var num = getPlayerNum(currentChunk.head, true)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack10"        =>  var num = getPlayerNum(currentChunk.head, true)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points10"       =>  var num = getPlayerNum(currentChunk.head, true)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player11"       =>  var num = getPlayerNum(currentChunk.head, true)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack11"        =>  var num = getPlayerNum(currentChunk.head, true)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points11"       =>  var num = getPlayerNum(currentChunk.head, true)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player12"       =>  var num = getPlayerNum(currentChunk.head, true)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack12"        =>  var num = getPlayerNum(currentChunk.head, true)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points12"       =>  var num = getPlayerNum(currentChunk.head, true)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case "#player13"       => var num = getPlayerNum(currentChunk.head, true)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).hand = Hand(cards)
              case "#stack13"        => var num = getPlayerNum(currentChunk.head, true)
                                        val cards = getCards(currentChunk)
                                        players(num - 1).stack = Stack(cards)
              case "#points13"       => var num = getPlayerNum(currentChunk.head, true)
                                        if currentChunk.last.toIntOption.isEmpty then
                                          throw CorruptedFileException(s"\nReason : Not a number: '${currentChunk.last}'")
                                        else players(num - 1).points = currentChunk.last.toInt
              case _                => throw CorruptedFileException(s"\nUnknown chunk header: ${currentChunk.head}\nThe whole chunk : (${currentChunk.mkString(", ")}.")
          })
        else currentLine = lineReader.readLine()

      def countPlayers(input: Array[String]) =
        input.count( _.toLowerCase.contains("player"))

      def getPlayerNum(input: String, twoNum: Boolean) =
        var playerNumOp = Option(0)
        if !twoNum then     // if it is a one character number eg "2"
          playerNumOp = input.takeRight(1).toIntOption
        else                // if it is a two character number eg "13"
          playerNumOp = input.takeRight(2).toIntOption
        var num = 0
        if playerNumOp.isEmpty then throw CorruptedFileException(s"\nReason : Not a number: '$playerNumOp'") else num = playerNumOp.get
        if num > players.length then throw CorruptedFileException(s"\nReason : Trying to access information on player number '$num', but that player does not exist\nCurrent players and their numbers: ${players.zipWithIndex.map( p => (p._1.name, p._2 + 1)).mkString(", ")}")
        num

      def getPlayerNamesAndNumbers(input: Array[String]) =
        var playerRows = input.filter( _.toLowerCase.contains("player"))
        playerRows = playerRows.map( _.replaceAll(" ", "").trim)
        if playerRows.isEmpty then throw CorruptedFileException(s"\nReason : No players found: '${playerRows.mkString("\n")}'")
        val result = Buffer[(String, Int)]()
        for s <- playerRows do
          val name = s.split(":").last.trim
          val numOption = s.split(":").head.takeRight(1).toIntOption
          if numOption.isEmpty then throw CorruptedFileException(s"\nReason : Player number not found: '$s'")
          else result += ((name, numOption.get))
        result

      def getCards(input: Array[String]) =
        val result = Buffer[Card]()
        for str <- input.tail do
          str.trim.toLowerCase match
                case "02d" => result += twoD
                case "02s" => result += twoS
                case "02c" => result += twoC
                case "02h" => result += twoH
                case "03d" => result += threeD
                case "03s" => result += threeS
                case "03c" => result += threeC
                case "03h" => result += threeH
                case "04d" => result += fourD
                case "04s" => result += fourS
                case "04c" => result += fourC
                case "04h" => result += fourH
                case "05d" => result += fiveD
                case "05s" => result += fiveS
                case "05c" => result += fiveC
                case "05h" => result += fiveH
                case "06d" => result += sixD
                case "06s" => result += sixS
                case "06c" => result += sixC
                case "06h" => result += sixH
                case "07d" => result += sevenD
                case "07s" => result += sevenS
                case "07c" => result += sevenC
                case "07h" => result += sevenH
                case "08d" => result += eightD
                case "08s" => result += eightS
                case "08c" => result += eightC
                case "08h" => result += eightH
                case "09d" => result += nineD
                case "09s" => result += nineS
                case "09c" => result += nineC
                case "09h" => result += nineH
                case "10d" => result += tenD
                case "10s" => result += tenS
                case "10c" => result += tenC
                case "10h" => result += tenH
                case "jkd" => result += jackD
                case "jks" => result += jackS
                case "jkc" => result += jackC
                case "jkh" => result += jackH
                case "qnd" => result += queenD
                case "qns" => result += queenS
                case "qnc" => result += queenC
                case "qnh" => result += queenH
                case "knd" => result += kingD
                case "kns" => result += kingS
                case "knc" => result += kingC
                case "knh" => result += kingH
                case "acd" => result += aceD
                case "acs" => result += aceS
                case "acc" => result += aceC
                case "ach" => result += aceH
                case _     => throw CorruptedFileException(s"\nReason : Unknown card: '$str'")
        result


    // If something missing, throwing exception
    if !turnDecided then throw CorruptedFileException("\nReason : Turn undecided")
    if !deckAdded then throw CorruptedFileException("\nReason : Current deck not added")
    if !origDeckAdded then throw CorruptedFileException("\nReason: Original deck not added")

    val result = Vector(players.map(_.name), players.map(_.hand) , players.map(_.stack), players.map(_.points), turn, tableCards, deck, origDeck)

def loadGameFromFile() =

  println("\nTrying to load game from save file...\n")

  val fileName = "save.txt"
  val fileIn = FileReader(fileName)
  val linesIn = BufferedReader(fileIn)
  try
    val result = loadGame(linesIn)
    println("Game loaded successfully!\n")
    var playTable = Table(tableCards, players)
    var currentGame = Game(playTable, deck)
    var playersPoints = players.map( p => (p, p.points) )
    turn = if (turn - 1) < 0 then (players.length - 1) else ((turn - 1) % players.length)    // if turn is 0, previous player is the player at the last index, else its the previous player by index

    def findMax(playerNum: Buffer[(Player, Int)]) =
      var max = 0
      for pair <- playerNum do
        if pair._2 > max then                             // helper function for finding the maximum points
          max = pair._2
      playerNum.filter( p => p._2 == max ).map( pair => pair._1 )

    def tryToGetNumber(input: String) = Try {
      input.toInt                                         //helper function for exception handling
    }

    var previousPlayer = if turn == 0 then currentGame.table.players.last else currentGame.table.players( turn - 1)

    println(s"The game continues! Current points: \n${playersPoints.map( p => (p._1.name, p._2)).mkString("\n")}\n")

    def newRound() =
      playersPoints = players.map( p => (p, p.points) )                   // update player points
      deck = Deck(origDeck.cards.toBuffer)                                // reset deck to original
      turn += 1                                                           // change starting player
      tableCards = deck.selectRandomCards(4)                              // reset the table cards
      playTable = Table(tableCards, players)                              // reset table
      currentGame = Game(playTable, deck)                                 // reset the game
      println(s"\nA new round begins! Current points: \n${playersPoints.map( p => (p._1.name, p._2)).mkString("\n")}\n")
      for player <- players do
        player.stack = Stack(Buffer[Card]())                              // reset players stack so that they dont get multiple points
        player.hand = Hand(deck.selectRandomCards(4))                     // reset players hand

    while !{currentGame.table.players.exists( _.points >= 16)} do {             // continue while no player has atleast 16 points
      while currentGame.table.players.exists( p => p.hand.cards.nonEmpty ) do   // progress if atleast one player still has cards in their hand
        breakable {
            for i <- 0 until 1 do
                val currentPlayer = currentGame.table.players(turn % currentGame.table.players.length)
                if currentPlayer.hand.cards.isEmpty then
                  turn += 1
                  break                                          // if for some reason current player does not have any cards, it skips them
                println(s"\nPlayer is now : ${currentPlayer.name}")

                if currentGame.deck.cards.nonEmpty && previousPlayer.hand.cards.length < 4 then
                  previousPlayer.hand.addCard(currentGame.deck.selectRandomCards(1).head)                 // as long as there are cards in the deck, add one to the previous players hand

                val handCardsIndexed = currentPlayer.hand.cards.zipWithIndex
                var handCardIndex = 0                                                                   // temporary placeholder
                var userInput = readLine(s"What card do you want to play? Input the number next to the card.\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${currentGame.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "")

                while userInput == "/halt" do
                  halt(players, turn, deck, origDeck, currentGame.table.cards)
                  println(s"\nPlayer is now : ${currentPlayer.name}")
                  userInput = readLine(s"What card do you want to play? Input the number next to the card.\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${currentGame.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "")


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
                  while userInput == "/halt" do
                    halt(players, turn, deck, origDeck, currentGame.table.cards)
                    println(s"\nPlayer is now : ${currentPlayer.name}")
                    userInput = readLine(s"What card do you want to play? Input the number next to the card.\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${currentGame.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "")
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
                  var tableCardsIndexes = Buffer[Int]()                                                 //placeholder

                  var input = readLine(s"What card(s) do you want in return? Separate multiple indexes with a comma ','\nCurrent table : ${tableCardsIndexed.mkString(" | ")}\n-> ").trim.replace(" ", "").split(",")

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


                  val tableCards = Buffer[Card]()
                  tableCardsIndexes.foreach( index => tableCards += tableCardsIndexed.filter( _._2 == index).map( pair => pair._1).head )

                  currentGame.humanAlgorithm(currentGame.table, currentPlayer, handCard, tableCards)

                println("\n------------------------------------------------------------------------------------")
                previousPlayer = currentPlayer
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

      players.foreach( p => println(s"Player ${p.name} stack : ${p.stack.gatheredCards.mkString(", ")}\nPoints: ${p.points}\n") )   // currently for TESTING

      if !{currentGame.table.players.exists( _.points >= 16)} then newRound()     // check for the 16 points, if no player has them start a new round

    }
    playersPoints = players.map( p => (p, p.points) )                   // update player points one final time

    var winners = findMax(playersPoints)

    if winners.length > 1 then
      println(s"\n\nPlayers ${winners.map(_.name).mkString(" and ")} have points ${winners.map( p => p.points).mkString(", ")}\nITS A TIE!")
    else
      println(s"\n\nAnd the winner is: ${winners.map(_.name).mkString}!")

  finally
    fileIn.close()        // close the streams
    linesIn.close()
