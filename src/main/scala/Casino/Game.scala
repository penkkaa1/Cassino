package Casino
import scala.io.StdIn.*
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._


class Game(val table: Table, val deck: Deck):

  var lastPickUpPlayer = table.players.head
  
  def humanAlgorithm(table: Table, player: Player, playedCard: Card, tableCards: Buffer[Card]) =
  breakable {
    for tableCard <- tableCards do
      if playedCard.valueInHand == tableCards.foldLeft(0)( (card1, card2) => card1 + card2.valueInTable) then
        table.removeCardsFromTable(tableCards)
        player.addMultipleCardsToStack(tableCards)
        player.addSingleCardToStack(playedCard)                       // case if multiple cards are chosen to be picked up from the table
        player.hand.removeCard(playedCard)
        if table.cards.isEmpty then                                   // checking if last pickup was a sweep
          player.addPoints(1)
          println(s"\nPlayer ${player.name} played $playedCard, and recieved cards: ${tableCards.mkString(" and ")}.\nThey also got an extra point for sweeping the table!")
        else
          println(s"\nPlayer ${player.name} played $playedCard, and recieved cards: ${tableCards.mkString(" and ")}!")
        lastPickUpPlayer = player
        break
      else if playedCard.valueInHand == tableCard.valueInTable then
        table.removeSingleCard(tableCard)
        player.addSingleCardToStack(tableCard)
        player.addSingleCardToStack(playedCard)                       // case if single card is chosen to be picked up from the table
        player.hand.removeCard(playedCard)
        if table.cards.isEmpty then                                   // checking if last pickup was a sweep
          player.addPoints(1)
          println(s"\nPlayer ${player.name} played $playedCard, recieved ${tableCard}, and got an extra point for sweeping the table!")
        else
          println(s"\nPlayer ${player.name} played $playedCard, and recieved ${tableCard}!")
        lastPickUpPlayer = player
        break
      else
        player.hand.removeCard(playedCard)
        table.addCardToTable(playedCard)                              // case if playedCard does not match anything, e.g. card is placed on the table
        println(s"\nPlayer ${player.name} places $playedCard on the table!")
        break
  }


  override def toString() = table.toString()

end Game


