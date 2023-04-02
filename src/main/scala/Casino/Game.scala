package Casino
import scala.io.StdIn.*
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._

//NEEDS TO BE PLAYED WITH ROUNDS

class Game(val table: Table, val deck: Deck):

  def humanAlgorithm(table: Table, player: Player, playedCard: Card, tableCards: Buffer[Card]) =
  breakable {
    for tableCard <- tableCards do
      if playedCard.valueInHand == tableCards.foldLeft(0)( (card1, card2) => card1 + card2.valueInTable) then
        table.removeCardsFromTable(tableCards)
        player.addMultipleCardsToStack(tableCards)
        player.addSingleCardToStack(playedCard)
        player.hand.removeCard(playedCard)
        if table.cards.isEmpty then
          table.addPoints(player, 1)
          println(s"\nPlayer ${player.name} played $playedCard, and recieved cards: ${tableCards.mkString(" and ")}.\nThey also got an extra point for sweeping the table!")
        else
          println(s"\nPlayer ${player.name} played $playedCard, and recieved cards: ${tableCards.mkString(" and ")}!")
        break
      else if playedCard.valueInHand == tableCard.valueInTable then
        table.removeSingleCard(tableCard)
        player.addSingleCardToStack(tableCard)
        player.addSingleCardToStack(playedCard)
        player.hand.removeCard(playedCard)
        if table.cards.isEmpty then
          table.addPoints(player, 1)
          println(s"\nPlayer ${player.name} played $playedCard, recieved ${tableCard}, and got an extra point for sweeping the table!")
        else
          println(s"\nPlayer ${player.name} played $playedCard, and recieved ${tableCard}!")
        break
      else
        player.hand.removeCard(playedCard)
        table.addCardToTable(playedCard)
        println(s"\nPlayer ${player.name} places $playedCard on the table!")
        break
  }


  override def toString() = table.toString()

end Game


