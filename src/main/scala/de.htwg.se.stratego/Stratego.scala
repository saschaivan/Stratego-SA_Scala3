package de.htwg.se.stratego

import de.htwg.se.stratego.aview.Tui
import de.htwg.se.stratego.controller.Controller
import de.htwg.se.stratego.model.MatchField

import scala.io.StdIn.readLine

@main def stratego = 
  val controller = Controller(new MatchField(4, 4, false))
  val tui = new Tui(controller)
  controller.notifyObservers()
  var input = ""
  while (!input.equals("q"))  
    input = readLine()
    tui.processInputLine(input)

