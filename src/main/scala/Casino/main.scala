package Casino

import scala.io.StdIn.*
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._

@main def run() =


  //val testDeck = Deck(allCards)
  val testDeck = Deck(Buffer(twoH, twoD, twoC, twoS, threeH, threeD, threeS, threeC, sixC, sixD, sixS, sixH, sixC, sixD, sixS, sixH))
  //val testDeck = Deck(Buffer(twoH,threeC,fourH,fiveD,sixD,sevenC,eightS,nineS,tenH,jackC,queenS,kingH,aceD, aceC, eightD, fourS, twoC))
  val player1Stack = Stack(Buffer[Card]())
  val player2Stack = Stack(Buffer[Card]())
  val player1Hand = Hand(testDeck.selectRandomCards(3)) //3 because first player gets a card when the game is initialized
  val player2Hand = Hand(testDeck.selectRandomCards(4))

  val player1 = Player("Aaron", player1Hand, player1Stack)
  val player2 = Player("Joni", player2Hand, player2Stack)
  val players = Buffer(player1, player2)

  val testTableCards = testDeck.selectRandomCards(4)
  val testTable = Table(testTableCards, players)
  val testiPeli = Game(testTable, testDeck)

  var turn = 0
  var previousPlayer = testiPeli.table.players.head
  //!{testiPeli.table.players.exists( _.points >= 16 )} ||
  //(testiPeli.deck.cards.nonEmpty &&
  while testiPeli.table.players.exists( p => p.hand.cards.nonEmpty ) do
    breakable {
      for i <- 0 until 1 do

        val currentPlayer = testiPeli.table.players(turn % testiPeli.table.players.length)
        if currentPlayer.hand.cards.isEmpty then break
        println(s"\nPlayer is now : ${currentPlayer.name}")

        if testiPeli.deck.cards.nonEmpty then
          previousPlayer.hand.addCard(testiPeli.deck.selectRandomCards(1).head)

        val handCardsIndexed = currentPlayer.hand.cards.zipWithIndex
        val handCardIndex = readLine(s"What card do you want to play?\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${testiPeli.table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "").toInt
        //EXCEPTION NEEDED: index out of range / index not found / what if it is not a int, but a string
        val handCard = handCardsIndexed.filter( pairs => pairs._2 == handCardIndex).map( pair => pair._1 ).head
        //EXCEPTION NEEDED: what if not found
        println(s"\nYou selected : $handCard\n")

        val tableCardsIndexed = testiPeli.table.cards.zipWithIndex //WHAT IF TABLE IS EMPTY
        val tableCardsIndexes = readLine(s"\nWhat card(s) do you want in return? Separate multiple indexes with a comma ','\nCurrent table : ${tableCardsIndexed.mkString(" | ")}\n-> ").trim.replace(" ", "").split(",").map( _.toInt)
        //WHAT ABOUT MULTIPLE PAIRS
        val tableCards = Buffer[Card]()
        tableCardsIndexes.foreach( index => tableCards += tableCardsIndexed.filter( _._2 == index).map( pair => pair._1).head )

        testiPeli.humanAlgorithm(testiPeli.table, currentPlayer, handCard, tableCards)

        previousPlayer = currentPlayer


        turn += 1}

  def findMax(playerNum: Buffer[(Player, Int)]) =
    var max = 0
    for pair <- playerNum do
      if pair._2 > max then
        max = pair._2
    playerNum.filter( p => p._2 == max ).map( pair => pair._1 )

  var playerAces = players.map( p => (p, p.stack.countAces() ) )
  if !playerAces.forall( p => p._2 == 0) then findMax(playerAces).foreach( p => testiPeli.table.addPoints(p,1) )

  var playerCards = players.map( p => (p, p.stack.countCards() ) )
  if !playerCards.forall( p => p._2 == 0 ) then findMax(playerCards).foreach( p => testiPeli.table.addPoints(p,1) )


  var playerSpades = players.map( p => (p, p.stack.countSpades() ) )
  if !playerSpades.forall( p => p._2 == 0 ) then findMax(playerSpades).foreach( p => testiPeli.table.addPoints(p,2) )


  players.foreach( p => if p.stack.hasCard(tenC) then testiPeli.table.addPoints(p, 2) )
  players.foreach( p => if p.stack.hasCard(twoS) then testiPeli.table.addPoints(p, 1) )

  players.foreach( p => println(s"\nPlayer ${p.name} stack : ${p.stack.gatheredCards.mkString(", ")}\nPoints: ${p.points}") )

  var playersPoints = players.map( p => (p, p.points) )
  var winners = findMax( playersPoints )

  if winners.length > 1 then
    println(s"\n\nPlayers ${winners.map(_.name).mkString(" and ")} have points ${winners.map( p => p.points).mkString(", ")}\nITS A TIE!")
  else
    println(s"\n\nAnd the winner is: ${winners.map(_.name).mkString}!")

end run
