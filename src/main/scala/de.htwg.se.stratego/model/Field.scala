package de.htwg.se.stratego.model


case class Field(isSet:Boolean, character: Option[GameCharacter] = None) {

  override def toString: String = character.fold(" ")("".+)

}
