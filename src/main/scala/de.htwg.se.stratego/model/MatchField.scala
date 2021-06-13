package de.htwg.se.stratego.model


case class MatchField(fields: Matrix[Field]) {

  def this(rowSize: Int, colSize: Int, isSet: Boolean) = this(new Matrix[Field](rowSize, colSize, Field(isSet)))

  def addChar(row: Int, col: Int, char: GameCharacter): MatchField = copy(fields.updateField(row, col, Field(true, Some(char))))

  def removeChar(row: Int, col: Int): MatchField = copy(fields.updateField(row, col, Field(false)))

  def legend():String = {
    val welcome = "**********  WELCOME TO STRATEGO  **********\n\n"
    val n = "n:   create a new empty machtfield\n"
    val i = "i:   set all character on the matchfield\n"
    val q = "q:   quit the programm\n"
    val u = "u:   move one character up\n"
    val d = "d:   move one character down\n"
    val r = "r:   move one character to the right\n"
    val l = "l:   move one character to the left\n"
    val a = "a:   attack the character next to you\n"
    val o = "o:   player one can set his characters\n"
    val t = "t:   player two can set his characters\n"
    welcome + n + i + u + d + r + l + a + o + t + q
  }

  def frame(row:Int): String = {
    val plus = "+"
    val line = "-"
    val combine = (plus + line * 5) * row + plus
    combine
  }

  override def toString:String = {
    val col = fields.matrixSize
    val row = fields.matrixSize
    val n = fields.matrixSize - 1
    val pipe = "|"
    val new_line = "\n"
    var matchField = ""

    for(rowNumbers <- 0 until row) matchField += "   " + rowNumbers + "  "
    matchField += new_line + frame(fields.matrixSize) + new_line
    for { row <- 0 until row
          col <- 0 until col }
    {
      if (fields.field(row, col).isSet) {
        matchField += "|  " + fields.field(row,col) + "  "
      } else {
        matchField += "|     "
      }
      if (col == n) {
        matchField += pipe + " " + row + new_line
        matchField += frame(fields.matrixSize) + new_line
      }
    }
    matchField += legend()
    matchField
  }
}
