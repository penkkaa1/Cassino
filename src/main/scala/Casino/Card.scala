package Casino

class Card(val name: String, val valueInHand: Int, val valueInTable: Int):

  val firstTwo = name.take(2)
  val last = name.takeRight(1)
  
  val toOriginalForm = firstTwo + last

  var value = ""
  var suit = ""

  firstTwo.toUpperCase match
    case "02" => value = "Two"
    case "03" => value = "Three"
    case "04" => value = "Four"
    case "05" => value = "Five"
    case "06" => value = "Six"
    case "07" => value = "Seven"
    case "08" => value = "Eight"
    case "09" => value = "Nine"
    case "10" => value = "Ten"
    case "JK" => value = "Jack"
    case "QN" => value = "Queen"
    case "KN" => value = "King"
    case "AC" => value = "Ace"
    case _    => value = "UNKNOWN CARD VALUE"

    last.toUpperCase match
      case "H" => suit = "Hearts"
      case "S" => suit = "Spades"
      case "C" => suit = "Clubs"
      case "D" => suit = "Diamonds"
      case _   => suit = "UNKNOWN CARD SUIT"


  override def toString() = s"$value of $suit"

end Card

