package Casino
import scala.collection.mutable.Buffer
import scala.util.Random

class Deck(var cards: Buffer[Card]):
  def removeCard(card: Card) =
    val found = cards.find( c => c.suit == card.suit && c.value == card.value)
    if found.isDefined then
      found.foreach( c => cards.remove(cards.indexOf(c) ) )
      println("Card deleted from deck")
    else
      println(s"Card '$card' not found in this deck")

  def shuffle() =
    this.cards = Random.shuffle(this.cards)

  override def toString() =
    cards.toString()