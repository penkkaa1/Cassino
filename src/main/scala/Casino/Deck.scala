package Casino
import scala.collection.mutable.Buffer
import scala.util.Random

class Deck(var cards: Buffer[Card]):
  def removeCard(card: Card) =
    val found = cards.find( c => c.suit == card.suit && c.value == card.value)
    if found.isDefined then
      found.foreach( c => cards.remove(cards.indexOf(c) ) )
      //println("Card deleted from deck")
    else
      println(s"Card '$card' not found in this deck")

  def selectRandomCards(amount: Int): Buffer[Card] =
    var output = Buffer[Card]()
    for i <- 0 until amount do
      var randomCard = cards(Random.nextInt(cards.length))
      output = output += randomCard
    for card <- output do
      this.removeCard(card)
    //COULD HAVE A BETTER IMPLEMENTATION, COULD HAVE MULTIPLES OF CARDS
    output
    

  

  def shuffle() =
    this.cards = Random.shuffle(this.cards)

  override def toString() =
    cards.toString()

end Deck

//@main def pakkaTestaus() =
//  val testiPakka = Deck(allCards)
//  println(testiPakka.cards.length)
//  val pöytä = testiPakka.selectRandom4()
//  println(testiPakka.cards.length)
//  println(pöytä.mkString(", "))
//  for card <- pöytä do
//    testiPakka.cards.contains(card)


//All of the cards individually
//hearts
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

//spades
val twoS = Card("02S", 15, 2)
val threeS  = Card("03S", 3, 3)
val fourS = Card("04S", 4, 4)
val fiveS = Card("05S", 5, 5)
val sixS = Card("06S", 6, 6)
val sevenS = Card("07S", 7, 7)
val eightS = Card("08S", 8, 8)
val nineS = Card("09S", 9, 9)
val tenS = Card("10S", 10, 10)
val jackS = Card("JkS", 11, 11)
val queenS = Card("QnS", 12, 12)
val kingS = Card("KnS", 13, 13)
val aceS = Card("AcS", 14, 1)

//diamonds
val twoD = Card("02D", 2, 2)
val threeD  = Card("03D", 3, 3)
val fourD = Card("04D", 4, 4)
val fiveD = Card("05D", 5, 5)
val sixD = Card("06D", 6, 6)
val sevenD = Card("07D", 7, 7)
val eightD = Card("08D", 8, 8)
val nineD = Card("09D", 9, 9)
val tenD = Card("10D", 16, 10)
val jackD = Card("JkD", 11, 11)
val queenD = Card("QnD", 12, 12)
val kingD = Card("KnD", 13, 13)
val aceD = Card("AcD", 14, 1)


//clubs
val twoC = Card("02C", 2, 2)
val threeC  = Card("03C", 3, 3)
val fourC = Card("04C", 4, 4)
val fiveC = Card("05C", 5, 5)
val sixC = Card("06C", 6, 6)
val sevenC = Card("07C", 7, 7)
val eightC = Card("08C", 8, 8)
val nineC = Card("09C", 9, 9)
val tenC = Card("10C", 10, 10)
val jackC = Card("JkC", 11, 11)
val queenC = Card("QnC", 12, 12)
val kingC = Card("KnC", 13, 13)
val aceC = Card("AcC", 14, 1)

val allCards = Deck(Buffer(
  twoH, threeH, fourH, fiveH,
  sixH, sevenH, eightH, nineH,
  tenH, jackH, queenH, kingH, aceH,
  
  twoS, threeS, fourS, fiveS,
  sixS, sevenS, eightS, nineS,
  tenS, jackS, queenS, kingS, aceS,
  
  twoD, threeD, fourD, fiveD,
  sixD, sevenD, eightD, nineD,
  tenD, jackD, queenD, kingD, aceD,
  
  twoC, threeC, fourC, fiveC,
  sixC, sevenC, eightC, nineC,
  tenC, jackC, queenC, kingC, aceC
))