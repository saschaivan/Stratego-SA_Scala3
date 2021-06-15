package de.htwg.se.stratego.model

case class Player(name: String, characterList: Seq[GameCharacter]) :
  override def toString: String = name

