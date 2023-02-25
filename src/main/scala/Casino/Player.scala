package Casino

class Player(name: String, hand: Hand, stack: Stack):

  var points = 0

  def playCard(card: Card) =
    if !this.hand.cards.contains(card) then
      println(s"You do not have card '$card'")
    else
      this.hand.removeCard(card)
      println(s"Card '$card' removed!")

  override def toString() =
    s"\n$name, '$hand'"

end Player