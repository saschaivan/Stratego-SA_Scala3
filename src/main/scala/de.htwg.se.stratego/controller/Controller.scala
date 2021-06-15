package de.htwg.se.stratego.controller

import de.htwg.se.stratego.util.Observable
import de.htwg.se.stratego.model.{CharacterList, Game, GameCharacter, MatchField, Player}

class Controller(var matchField:MatchField) extends Observable :
  val list = CharacterList(matchField.fields.matrixSize)
  val playerBlue = Player("PlayerBlue", list.getCharacterList())
  val playerRed = Player("PlayerRed", list.getCharacterList())
  val game = Game(playerBlue, playerRed, matchField.fields.matrixSize, matchField)


  def createEmptyMatchfield(size:Int): Unit = {
    matchField = new MatchField(size, size, false)
    notifyObservers()
  }

  def initMatchfield(): Unit = {
    matchField = game.init()
    notifyObservers()
  }

  def attack(rowA: Int, colA: Int, rowD:Int, colD:Int): Unit ={
    matchField = game.attack(matchField, rowA, colA, rowD, colD)
    notifyObservers()
  }

  def set(player: Int, row:Int, col:Int, charac:String): Unit = {
    matchField = game.set(player, row,col,charac)
    notifyObservers()
  }

  def move(dir: Char, row:Int, col:Int): Unit = {
    matchField = game.move(dir, matchField, row, col)
    notifyObservers()
  }

  def matchFieldToString: String = matchField.toString

