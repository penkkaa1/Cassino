package Casino
import scala.io.StdIn.*
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._

//NEEDS TO BE PLAYED WITH ROUNDS

class Game(table: Table, deck: Deck):

  var turn = 0
  var previousPlayer = table.players.head

  while !{table.players.exists( _.points >= 16 )} || (deck.cards.nonEmpty && !table.players.exists( p => p.hand.cards.nonEmpty )) do
    breakable {
      for i <- 0 until 1 do

        val currentPlayer = table.players(turn % table.players.length)
        if currentPlayer.hand.cards.isEmpty then break
        println(s"\nPlayer is now : ${currentPlayer.name}")

        if deck.cards.nonEmpty then
          previousPlayer.hand.addCard(deck.selectRandomCards(1).head)

        val handCardsIndexed = currentPlayer.hand.cards.zipWithIndex
        val handCardIndex = readLine(s"What card do you want to play?\nYour hand : ${handCardsIndexed.mkString(" | ")}\nCurrent table : ${table.cards.mkString(" | ")}\n-> ").trim.replaceAll(" ", "").toInt
        //EXCEPTION NEEDED: index out of range / index not found / what if it is not a int, but a string
        val handCard = handCardsIndexed.filter( pairs => pairs._2 == handCardIndex).map( pair => pair._1 ).head
        //EXCEPTION NEEDED: what if not found
        println(s"\nYou selected : $handCard\n")

        val tableCardsIndexed = table.cards.zipWithIndex //WHAT IF TABLE IS EMPTY
        val tableCardsIndexes = readLine(s"\nWhat card(s) do you want in return? Separate multiple indexes with a comma ','\nCurrent table : ${tableCardsIndexed.mkString(" | ")}\n-> ").trim.replace(" ", "").split(",").map( _.toInt)
        val tableCards = Buffer[Card]()
        tableCardsIndexes.foreach( index => tableCards += tableCardsIndexed.filter( _._2 == index).map( pair => pair._1).head )

        humanAlgorithm(table, currentPlayer, handCard, tableCards)

        previousPlayer = currentPlayer

        turn += 1}

  println(s"\n\nGAME FINISHED, WINNER ")

  override def toString() = table.toString()

end Game


def humanAlgorithm(table: Table, player: Player, playedCard: Card, tableCards: Buffer[Card]) =
  breakable {
    for tableCard <- tableCards do
      if playedCard.valueInHand == tableCards.foldLeft(0)( (card1, card2) => card1 + card2.valueInTable) then
        table.removeCardsFromTable(tableCards)
        player.addMultipleCardsToStack(tableCards)
        player.addSingleCardToStack(playedCard)
        player.hand.removeCard(playedCard)
        println(s"\nPlayer ${player.name} played $playedCard, and recieved cards: ${tableCards.mkString(" and ")}!")
        break
      else if playedCard.valueInHand == tableCard.valueInTable then
        table.removeSingleCard(tableCard)
        player.addSingleCardToStack(tableCard)
        player.addSingleCardToStack(playedCard)
        player.hand.removeCard(playedCard)
        println(s"\nPlayer ${player.name} played $playedCard, and recieved ${tableCard}!")
        break
      else
        player.hand.removeCard(playedCard)
        table.addCardToTable(playedCard)
        println(s"\nPlayer ${player.name} places $playedCard on the table!")
        break
  }


@main def testProgram() =

  val twoH = Card("02H", 2, 2)
  val threeH  = Card("03H", 3, 3)
  val fourH = Card("04H", 4, 4)
  val fiveH = Card("05H", 5, 5)
  val sixH = Card("06H", 6, 6)
  val sevenH = Card("07H", 7, 7)
  val eightH = Card("08H", 8, 8)
  val nineH = Card("09H", 9, 9)
  val tenH = Card("10H", 10, 10)
  val jackH = Card("JkH", 11, 11)
  val queenH = Card("QnH", 12, 12)
  val kingH = Card("KnH", 13, 13)
  val aceH = Card("AcH", 14, 1)

  //val testDeck = Deck(allCards)
  val testDeck = Deck(Buffer(twoH,threeH,fourH,fiveH,sixH,sevenH,eightH,nineH,tenH,jackH,queenH,kingH,aceH))
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

//NEEDS A MAIN CLASS / STARTER CLASS
