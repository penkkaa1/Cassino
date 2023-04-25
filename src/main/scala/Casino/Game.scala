package Casino
import scala.io.StdIn.*
import scala.collection.mutable.Buffer
import scala.util.control.Breaks._


class Game(val table: Table, val deck: Deck):

  var lastPickUpPlayer = table.players.head

  def humanAlgorithm(table: Table, player: Player, tableCards: Buffer[Card], playedCard: Card) : Unit =

    var result = Buffer[Buffer[Card]]()                                       // tracking the matched combinations
    var toDelete = Buffer[Card]()                                             // cards to be removed

                                                     // case if table is empty is handled in 'main.scala'

    for i <- tableCards do
      if i.valueInTable == playedCard.valueInHand then                        // add table cards with the same value as the played one to be removed
        toDelete += i

    for i <- toDelete do
      player.addSingleCardToStack(i)
      table.removeSingleCard(i)                                               // deleting table cards with same value
      result += Buffer(i)
      tableCards -= i

    for time <- 2 to tableCards.length do                                     // do this for combinations of 2, 3, ... tableCards.length
      var combinations = tableCards.combinations(time)
      for comb <- combinations do
        if comb.map( _.valueInTable).sum == playedCard.valueInHand && comb.forall( c => tableCards.contains(c)) then    // extra check to make sure these cards actually still exist in tableCards
          result += comb
          table.removeCardsFromTable(comb.toBuffer)                           // remove found combinations cards from table
          for c <- comb do
            tableCards -= c                                                   // remove found combiantions cards from future combination checks
          player.addMultipleCardsToStack(comb.toBuffer)

    if result.isEmpty then
      println(s"\nPlayer ${player.name} places $playedCard on the table!")
      player.hand.removeCard(playedCard)                                      // case if no combination matches were found
      table.addCardToTable(playedCard)
    else                                                                      // matches found, accounting for sweeps
      if table.cards.isEmpty then
        println(s"\nPlayer ${player.name} played the ${playedCard}, and received cards: ${result.flatten.mkString(", ")}!\nThey also got an extra point for sweeping the table!")
        player.addPoints(1)
      else
        println(s"\nPlayer ${player.name} played the ${playedCard}, and received cards: ${result.flatten.mkString(", ")}!")

      player.addSingleCardToStack(playedCard)
      player.hand.removeCard(playedCard)              // removing the played card from the hand and adding it to the stack
      lastPickUpPlayer = player                       // updating last player to pick up cards


  override def toString() = table.toString()

end Game



