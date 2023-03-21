package Casino
import scala.io.StdIn.*
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._

//NEEDS TO BE PLAYED WITH ROUNDS; DECK

class Game(table: Table, deck: Deck):

  var turn = 0

  while !{table.players.exists( _.points >= 16 )} do
    val currentPlayer = table.players(turn % table.players.length)
    println(s"Player is now : $currentPlayer\nHand contains : ${currentPlayer.hand.cards.mkString(" | ")}\nCurrent table : ${table.cards.mkString(" | ")}\n\n")
    val handCardsIndexed = currentPlayer.hand.cards.zipWithIndex
    val handCardIndex = readLine(s"What card do you want to play?\nYour deck : ${handCardsIndexed.mkString(" | ")}\n-> ").trim.replaceAll(" ", "").toInt
    val handCard = handCardsIndexed.filter( pairs => pairs._2 == handCardIndex).map( pair => pair._1 ).head
    val tableCardsIndexed = table.cards.zipWithIndex
    val tableCardsIndexes = readLine(s"What card(s) do you want in return? Separate multiple indexes with a colon ','\nThe table : ${tableCardsIndexed.mkString(" | ")}\n-> ").trim.replace(" ", "").split(",").map( _.toInt)
    val tableCards = Buffer[Card]()
    tableCardsIndexes.foreach( index => tableCards += tableCardsIndexed.filter( _._2 == index).map( pair => pair._1).head )
    humanAlgorithm(table, currentPlayer, handCard, tableCards)

    turn += 1

  override def toString() = table.toString()

end Game


def humanAlgorithm(table: Table, player: Player, playedCard: Card, tableCards: Buffer[Card]) =
  breakable {
    for tableCard <- tableCards do
      if playedCard.valueInHand == tableCards.foldLeft(0)( (card1, card2) => card1 + card2.valueInTable) then
        table.removeCardsFromTable(tableCards)
        break
      else if playedCard.valueInHand == tableCard.valueInTable then
        table.removeSingleCard(tableCard)
      else
        player.hand.removeCard(playedCard)
        table.addCardToTable(tableCard)
  }


@main def testiOhjelma() =
  val testipelaaja1 = Player("Aaron", Hand(Buffer(twoC, threeD, fourH, fiveS)), Stack(Buffer[Card]()))
  val testipelaaja2 = Player("Joni", Hand(Buffer(queenD, sevenH, sixD, tenS)), Stack(Buffer[Card]()))
  val testiPöytä = Table(Buffer(aceC, fiveD, jackC, twoH, threeS), Buffer(testipelaaja1, testipelaaja2))
  val testiPeli = Game(testiPöytä, Deck(allCards))


