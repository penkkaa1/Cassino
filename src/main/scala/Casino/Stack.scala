package Casino
import scala.collection.mutable.Buffer

class Stack(val gatheredCards: Buffer[Card]):

  def countAces() =
    this.gatheredCards.count( _.value == "Ace" )

  def countCards() =
    this.gatheredCards.length

  def countSpades() =
    this.gatheredCards.count( _.suit == "Spades" )

  def hasCard(card: Card) =
    this.gatheredCards.contains(card)

  override def toString() =
    gatheredCards.toString()

end Stack