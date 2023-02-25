package Casino

class Game(table: Table, deck: Deck):

  var turn = 0

  while !{table.players.exists( _.points <= 16 )} do
    val currentPlayer = table.players(turn % table.players.length)





    turn += 1

  override def toString() = table.toString()

end Game