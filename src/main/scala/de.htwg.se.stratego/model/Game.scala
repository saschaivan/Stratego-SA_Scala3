package de.htwg.se.stratego.model
import de.htwg.se.stratego.model
import de.htwg.se.stratego.model.{Game, MatchField, Player}


case class Game(playerA: Player, playerB: Player, size: Int, var matchField: MatchField) {
  val bList = playerA.characterList
  val rList = playerB.characterList


  def init(): MatchField = {

    //sets Characters of PlayerA
    var row = 0
    var col = 0
    // shuffle(aList)
    for (charac <- bList) {
      if (row.equals(size)) {
        col += 1
        row = 0
      }
      matchField = matchField.addChar(col, row, charac)
      row += 1
    }

    //Sets Characters of PlayerB
    row = 0
    col = size - 1
    // shuffle(bList)
    for (charac <- rList) {
      if (row.equals(size)) {
        col -= 1
        row = 0
      }
      matchField = matchField.addChar(col, row, charac)
      row += 1
    }
    matchField
  }

  def characValue(charac:String): Int = {
    if (charac.matches("[1-9]")) {
      return charac.toInt
    }
    charac match {
      case "B" => 11
      case "M" => 10
      case "F" => 0
    }
  }

  def isBlueField(row:Int): Boolean = {
    matchField.fields.matrixSize match {
      case 4 | 5 => if(row > 0) false else true
      case 6 | 7 => if(row > 1) false else true
      case 8 | 9 => if(row > 2) false else true
      case 10    => if(row > 3) false else true
    }
  }

  def isRedField(row:Int): Boolean = {
    matchField.fields.matrixSize match {
      case 4 | 5 => if(row < 3) false else true
      case 6 | 7 => if(row < 4) false else true
      case 8 | 9 => if(row < 5) false else true
      case 10    => if(row < 6) false else true
    }
  }

  def setBlue(row:Int, col:Int, charac: String): MatchField = {
    if (isBlueChar(charac) && isBlueField(row) && !matchField.fields.field(row,col).isSet) {
      matchField = matchField.addChar(row,col,bList(bList.indexOf(GameCharacter(Figure.FigureVal(charac,characValue(charac))))))
      return matchField
    }
    matchField
  }

  def isBlueChar(charac:String): Boolean = {
    bList.foreach(GameCharacter =>
      if(GameCharacter.toString().equals(charac)) {
        return true
      }
    )
    false
  }

  def set(player: Int, row:Int, col:Int, charac: String): MatchField = {
    player match{
      case 1 => return setBlue(row, col, charac)
      case 2 => return setRed(row, col, charac)
    }
    matchField
  }

  def setRed(row:Int, col:Int, charac: String): MatchField = {
    if (isRedChar(charac) && isRedField(row) && !matchField.fields.field(row,col).isSet) {
      matchField = matchField.addChar(row,col,rList(rList.indexOf(GameCharacter(Figure.FigureVal(charac,characValue(charac))))))
      //bList.updated(idx, GameCharacter(Figure.FigureVal(charac,value)))
      return matchField
    }
    matchField
  }

  def isRedChar(charac:String): Boolean = {
    bList.map(GameCharacter => if(GameCharacter.figure.name.equals(charac)) return true else false)
    false
  }

  def aChar(matchfield: MatchField, idx: Int, row: Int, col: Int): MatchField = matchfield.addChar(row, col, bList(idx))

  def bChar(matchfield: MatchField, idx: Int, row: Int, col: Int): MatchField = matchfield.addChar(row, col, rList(idx))

  def move(direction: Char, matchField: MatchField, row: Int, col: Int): MatchField = {
    direction match {
      case 'u' => return moveUp(matchField, row, col)
      case 'd' => return moveDown(matchField, row, col)
      case 'r' => return moveRight(matchField, row, col)
      case 'l' => return moveLeft(matchField, row, col)
    }
    matchField
  }

  def moveDown(matchField: MatchField, row: Int, col: Int): MatchField = {
    if (row == size - 1) {
      println("The Figure can not set out of bounds!")
      return matchField
    }
    if (isFlagOrBomb(matchField, row,col)) {
      println("Flag and Bombs can't move!")
      return matchField
    }
    if (matchField.fields.field(row + 1, col).isSet.equals(false)) {
      matchField.removeChar(row, col).addChar(row + 1, col, matchField.fields.field(row, col).character.get)
    } else {
      val f = matchField.fields.field(row + 1, col)
      println(s"Field (${row + 1},$col) is set with Figure $f!")
      matchField
    }
  }

  def moveUp(matchField: MatchField, row: Int, col: Int): MatchField = {
    if (row == 0) {
      println("The Figure can not set out of bounds!")
      return matchField
    }
    if (isFlagOrBomb(matchField, row,col)) {
      println("Flag and Bombs can't move!")
      return matchField
    }
    if (matchField.fields.field(row - 1, col).isSet.equals(false)) {
      matchField.removeChar(row, col).addChar(row - 1, col, matchField.fields.field(row, col).character.get)
    } else {
      val f = matchField.fields.field(row - 1, col)
      println(s"Field (${row - 1},$col) is set with Figure $f!")
      matchField
    }
  }

  def moveLeft(matchField: MatchField, row: Int, col: Int): MatchField = {
    if (col == 0) {
      println("The Figure can not set out of bounds!")
      return matchField
    }
    if (isFlagOrBomb(matchField, row,col)) {
      println("Flag and Bombs can't move!")
      return matchField
    }
    if (matchField.fields.field(row, col - 1).isSet.equals(false)) {
      matchField.removeChar(row, col).addChar(row, col - 1, matchField.fields.field(row, col).character.get)
    } else {
      val f = matchField.fields.field(row, col - 1)
      println(s"Field ($row,${col - 1}) is set with Figure $f!")
      matchField
    }
  }

  def moveRight(matchField: MatchField, row: Int, col: Int): MatchField = {
    if (col == size - 1) {
      println("The Figure can not set out of bounds!")
      return matchField
    }

    if (isFlagOrBomb(matchField,row,col)) {
      println("Flag and Bombs can't move!")
      return matchField
    }

    if (matchField.fields.field(row, col + 1).isSet.equals(false)) {
      matchField.removeChar(row, col).addChar(row, col + 1, matchField.fields.field(row, col).character.get)
    } else {
      val f = matchField.fields.field(row, col + 1)
      println(s"Field ($row,${col + 1}) is set with Figure $f!")
      matchField
    }
  }

  def isFlagOrBomb(matchField: MatchField, row: Int,col: Int): Boolean = if(matchField.fields.field(row,col).character.get.figure.value == 0 ||
    matchField.fields.field(row,col).character.get.figure.value == 11) true else false

  def figureHasValue(matchF: MatchField, row: Int,col: Int): Int = {
    matchF.fields.field(row,col).character.get.figure.value
  }

  def attackToFarAway(rowA: Int, colA: Int, rowD: Int, colD: Int): Boolean = {
    if(((Math.abs(rowA-rowD)>1)||(Math.abs(colA-colD)>1))||((Math.abs(rowA-rowD)==1)&&(Math.abs(colA-colD)==1)))
      return true
    false
  }

  def fieldIsSet(rowA: Int, colA: Int, rowD: Int, colD: Int): Boolean = {
    if(matchField.fields.field(rowA, colA).isSet.equals(true) && matchField.fields.field(rowD, colD).isSet.equals(true))
      return true
    false
  }

  def unableToAttack(rowA: Int, colA: Int, rowD: Int, colD: Int) : Boolean = {
    if(figureHasValue(matchField, rowA,colA).equals(11)|figureHasValue(matchField,rowA,colA).equals(0))
      return true
    false
  }

  def spyAttackMarshal(rowA: Int, colA: Int, rowD: Int, colD: Int): Boolean = {
    if((figureHasValue(matchField, rowA,colA) == 1) && (figureHasValue(matchField, rowD, colD) == 10))
      return true
    false
  }

  def attackIsStronger(rowA: Int, colA: Int, rowD: Int, colD: Int): Boolean = {
    if(figureHasValue(matchField, rowA,colA) > figureHasValue(matchField,rowD, colD))
      return true
    false
  }

  def attackFlag(rowD:Int, colD:Int): Boolean = {
    if(figureHasValue(matchField,rowD, colD)==0)
      return true
    false
  }

  def attackTheBomb(matchField:MatchField, rowD:Int, colD:Int):Boolean = {
    if(figureHasValue(matchField, rowD, colD).equals(11))
      return true
    false
  }

  def minerAttackTheBomb(matchField:MatchField, rowA:Int, colD:Int): Boolean = {
    if(figureHasValue(matchField, rowA, colD) == 3)
      return true
    false
  }

  def defenceIsStronger(rowA: Int, colA: Int, rowD: Int, colD: Int): Boolean = {
    if (figureHasValue(matchField, rowA,colA) < figureHasValue(matchField,rowD, colD))
      return true
    false
  }

  def attack(matchField: MatchField, rowA: Int, colA: Int, rowD: Int, colD: Int): MatchField = {

    if(attackToFarAway(rowA,colA,rowD,colD)) { //field of attacked character is too far away
      return matchField
    }
    if(fieldIsSet(rowA: Int, colA: Int, rowD: Int, colD: Int)){ //both fields are set
      if(unableToAttack(rowA,colA,rowD,colD)){ //flag and bomb are unable to attack
        return matchField
      }
      if(spyAttackMarshal(rowA,colA,rowD,colD)){ //attacker is spy and attacked figure is marshal
        return matchField.removeChar(rowD, colD).addChar(rowD, colD, matchField.fields.field(rowA,colA).character.get).removeChar(rowA,colA)
      }
      if(attackIsStronger(rowA,colA,rowD,colD)) {
        if(attackFlag(rowD,colD)){ //attacked figure is flag
          println("Flag has been found! Game finished!")
        }
        return matchField.removeChar(rowD, colD).addChar(rowD, colD, matchField.fields.field(rowA,colA).character.get).removeChar(rowA,colA)
      }
      if(attackTheBomb(matchField,rowD,colD)){ //attacked figure is a bomb
        if(minerAttackTheBomb(matchField,rowA,colA)) { //attacker is miner => just remove bomb
          return matchField.removeChar(rowD, colD)
        }
      }
      if (defenceIsStronger(rowA,colA,rowD,colD)) {
        return matchField.removeChar(rowA, colA)
      }
      return matchField.removeChar(rowA, colA).removeChar(rowD, colD) //else remove both figures
    }
    matchField
  }
}

