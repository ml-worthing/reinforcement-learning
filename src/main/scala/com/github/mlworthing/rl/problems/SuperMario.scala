package com.github.mlworthing.rl.problems

import com.github.mlworthing.rl.environments.BoardEnvironment
import com.github.mlworthing.rl.utils.{S0FGXFormat, UpRightDownLeft}

object SuperMario extends BoardEnvironment[Int, String] with UpRightDownLeft with S0FGXFormat {

  override lazy val layout: String =
    s"""
       |0 0 0 G
       |0 X 0 F
       |S 0 0 0
     """.stripMargin

  override lazy val actionMoves: Map[String, ActionMoves] = Map(
    "↑" -> (Up, 0.8, Map(Right   -> 0.1, Left -> 0.1)),
    "→" -> (Right, 0.8, Map(Up   -> 0.1, Down -> 0.1)),
    "↓" -> (Down, 0.8, Map(Right -> 0.1, Left -> 0.1)),
    "←" -> (Left, 0.8, Map(Up    -> 0.1, Down -> 0.1))
  )

  override def stateAt(row: Int, col: Int): Int = row * 4 + col
  override def positionOf(state: Int): (Int, Int) = (state / 4, state % 4)
}
