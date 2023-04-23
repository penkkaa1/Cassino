package Casino

import scala.io.StdIn.*
import scala.collection.mutable.Buffer
import scala.util.control.Breaks.*
import scala.util.{Failure, Success, Try}
import java.io.{BufferedReader, FileReader, IOException, Reader, FileNotFoundException, FileWriter, BufferedWriter}

def writeToFile(fileName: String, arr: Seq[String]) =
    try
      val writeFile = FileWriter(fileName)
      val writeLine = BufferedWriter(writeFile)
      try
        for i <- arr.indices do
          writeLine.write(arr(i))                                 // preparing writing to save file
          writeLine.newLine()
      finally
        writeLine.close()
        writeFile.close()
    catch
      case notFound: FileNotFoundException => println("File not found")
      case e: IOException => println("There was a problem writing this file")

def lineBeforeHashtag(input: Seq[String]): Seq[String] =
  var resList = Seq[String]()
  for w <- input do
    if w.startsWith("#") && (!w.contains("game") && !w.contains("stack") && !w.contains("points")) then     // mainly for style purposes, if word starts with '#', add a empty space
      resList = resList :+ "\n"
    resList = resList :+ w
  resList

def halt(players: Buffer[Player], turn: Int, deck: Deck, originalDeck: Deck, tableCards: Buffer[Card]) =    // when the player inputs '/halt'
    var input = readLine(s"\nGame halted.\nSave current game with '/save'\nContinue current game with '/continue'\nQuit current game with '/quit'\n-> ")

    def inputMatch(input: String) =
      input.split(" ").headOption match
        case Some("/save")     => "save"
        case Some("/continue") => "continue"
        case Some("/quit")     => "quit"
        case _ => "invalid"

    var inputTest = inputMatch(input)

    while inputTest == "invalid" do                         // exception handling
      input = readLine(s"\nUnknown command '$input'\nSave current game with '/save'\nContinue current game with '/continue'\nQuit current game with '/quit'\n-> ")
      inputTest = inputMatch(input)

    if inputTest == "quit" then
      println("\nGoodbye and thanks for playing!\n")        // quits the game
      System.exit(0)

    if inputTest == "save" then
      saveToFile(players, turn, deck, originalDeck, tableCards)     // saves current game, starts writing to file

    println(s"\nContinuing game!\n")                                // if input is '/continue'


def saveToFile(players: Buffer[Player], turn: Int, deck: Deck, originalDeck: Deck, tableCards: Buffer[Card]) =

  var counter = 1                                                           // counter for the player numbers

  var result = Seq[String]()
  var curPlayers = players

  result = "#game metadata" +: result
  for p <- curPlayers do
    result = result :+ s"Player${counter.toString}: ${p.name}"              // adding game metadata, aka the players
    counter += 1

  result = result :+ "#turn"
  result = result :+ turn.toString                                          // adding turn

  counter = 1
  for p <- curPlayers do
    result = result :+ s"#player${counter.toString}"                        // adding player hands
    val hand = p.hand.cards
    for c <- hand do
      result = result :+ c.toOriginalForm

    result = result :+ s"#stack${counter.toString}"
    val stack = p.stack.gatheredCards                                       // adding player stacks
    for c <- stack do
      result = result :+ c.toOriginalForm

    result = result :+ s"#points${counter.toString}"                        // adding player points
    result = result :+ s"${p.points.toString}"

    counter += 1

  result = result :+ "#table"
  for c <- tableCards do                                                    // adding table cards
    result = result :+ c.toOriginalForm

  result = result :+ "#deck"
  for c <- deck.cards do                                                    // adding current deck cards
    result = result :+ c.toOriginalForm

  result = result :+ "#origDeck"
  for c <- originalDeck.cards do                                            // adding original deck cards
    result = result :+ c.toOriginalForm

  result = lineBeforeHashtag(result)                                        // add empty space before hashtags, prepend with "save", write to the save file
  result = "save" +: result
  writeToFile("save.txt", result)
  println("\nSaving the game was successful!\n")

