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


def loadGame(input: Reader) =
  var atLeast2players = false
  var turnDecided = false
  var deckAdded = false

  val lineReader = BufferedReader(input)

  try

    var currentLine = lineReader.readLine().trim.toLowerCase

    if !(currentLine.startsWith("save")) then
      throw new CorruptedFileException("\nReason : Unable to read save file")

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
              case "#turn"          =>  val numOption = currentChunk.last.toIntOption
                                        numOption match
                                          case n: Some[Int] => turn = n.get
                                          case _            => throw CorruptedFileException(s"\nReason : Turn number invalid, '$numOption'")
                                        if turn > players.length then throw CorruptedFileException("\nReason : Turn number higher than player amount")    // MAYBE NOT NEEDED?
                                        turnDecided = true
              case "#deck"          =>  val cards = getCards(currentChunk)
                                        deck = Deck(cards)
                                        deckAdded = true
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
    if !deckAdded then throw CorruptedFileException("\nReason : Deck not added")

    //println(s"\nPlayers: ${players.mkString}")
//    println(s"\nStacks: ")
//    players.foreach( p => println((p.name, p.stack)))
//    println(s"\nPoints: ")
//    players.foreach( p => println((p.name, p.points)))
//    println(s"\nTurn: $turn")
//    println(s"\nDeck: ")
//    deck.cards.mkString("\n")

    val result = Vector(players.map(_.name), players.map(_.hand) , players.map(_.stack), players.map(_.points), turn, deck)
    println(result)



@main def testi() =

  val fileName = "save.txt"
  val fileIn = FileReader(fileName)
  val linesIn = BufferedReader(fileIn)
  try
    loadGame(linesIn)
  finally
    fileIn.close()
    linesIn.close()


