package Casino
import scala.collection.mutable.Buffer

class Hand(var cards: Buffer[Card]):
  def removeCard(card: Card) =
    val found = cards.find( c => c.suit == card.suit && c.value == card.value)
    if found.isDefined then
      found.foreach( c => cards.remove(cards.indexOf(c) ) )
      println("Card deleted from hand")
    else
      println(s"Card '$card' not found in this hand")

  def addCard(card: Card) =
    cards.append(card)

  override def toString() =
    cards.toString()

