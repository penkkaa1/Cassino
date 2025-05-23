package Casino
import scala.collection.mutable.Buffer

class Player(val name: String, var hand: Hand, var stack: Stack):

  var points = 0

  def playCard(card: Card) =
    if !this.hand.cards.contains(card) then
      println(s"You do not have card '$card'")
    else
      this.hand.removeCard(card)
      
  def addSingleCardToStack(card: Card) =
    this.stack.gatheredCards += card
    
  def addMultipleCardsToStack(cards: Buffer[Card]) =
    for currentCard <- cards do 
      this.stack.gatheredCards += currentCard
      
  def listStack() =
    println(s"${this.stack.gatheredCards.mkString(", ")}.")

  override def toString() =
    s"\n$name, '$hand'"
    
  def addPoints(amount: Int) =
    this.points += amount

end Player