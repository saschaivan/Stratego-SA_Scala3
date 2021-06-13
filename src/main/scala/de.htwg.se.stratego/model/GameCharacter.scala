package de.htwg.se.stratego.model

case class GameCharacter(figure: Figure.FigureVal, positionX : Int = 0, positionY : Int = 0, show: Boolean = true) {
  override def toString: String = figure.toString
}
