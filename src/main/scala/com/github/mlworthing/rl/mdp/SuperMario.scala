package com.github.mlworthing.rl
package mdp

import utils.{BoardEnvironment, S0FGXFormat, UpRightDownLeft}

case class SuperMario(gamma: Double = 0.9) extends BoardEnvironment[Int, String] with UpRightDownLeft with S0FGXFormat {

  override lazy val layout: String =
    s"""
       |0 0 0 G
       |0 X 0 F
       |S 0 0 0
     """.stripMargin

  override lazy val actionMoves: Map[String, ActionMoves] = Map(
    "N" -> (Up, 0.8, Map(Right   -> 0.1, Left -> 0.1)),
    "E" -> (Right, 0.8, Map(Up   -> 0.1, Down -> 0.1)),
    "S" -> (Down, 0.8, Map(Right -> 0.1, Left -> 0.1)),
    "W" -> (Left, 0.8, Map(Up    -> 0.1, Down -> 0.1))
  )

  override def stateAt(row: Int, col: Int): Int = row * 4 + col

}
