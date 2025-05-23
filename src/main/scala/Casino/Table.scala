package Casino
import scala.collection.mutable.Buffer

class Table(val cards: Buffer[Card], val players: Buffer[Player]):

  def addCardToTable(card: Card) =
    cards.append(card)

  def removeCardsFromTable(cards: Buffer[Card]) =
    for card <- cards do
      removeSingleCard(card)

  def removeSingleCard(card: Card) =
    val found = cards.find( c => c.suit == card.suit && c.value == card.value)
    if found.isDefined then
      found.foreach( c => cards.remove(cards.indexOf(c) ) )
    else
      println(s"Card '$card' not found is not on the table")


  override def toString() =
    s"\nPlayers: $players\nCards on table: $cards"
end Table
