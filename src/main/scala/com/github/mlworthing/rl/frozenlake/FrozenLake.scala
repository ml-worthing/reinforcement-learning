package com.github.mlworthing.rl.frozenlake

import com.github.mlworthing.rl.utils.BoardEnvironment

/**
  * The Frozen Lake environment as described in the book
  * <https://www.manning.com/books/grokking-deep-reinforcement-learning>
  *
  * The 4x4 board consists of 16 tiles having 4 ice holes rewarding -1
  * and one terminal state rewarding 1. The probability of moving on ice in the selected
  * direction should be less than 1.
  */
case class FrozenLake(gamma: Double) extends BoardEnvironment[Int, String] {

  override lazy val layout: String =
    s"""
       |S 0 0 0
       |0 F 0 F
       |0 0 0 F
       |F 0 0 G
     """.stripMargin

  lazy val Up: Move = (-1, 0)
  lazy val Right: Move = (0, 1)
  lazy val Down: Move = (1, 0)
  lazy val Left: Move = (0, -1)

  override lazy val actionMoves: Map[String, ActionMoves] = Map(
    "N" -> (Up, 0.34, Map(Right   -> 0.33, Left -> 0.33)),
    "E" -> (Right, 0.34, Map(Up   -> 0.33, Down -> 0.33)),
    "S" -> (Down, 0.34, Map(Right -> 0.33, Left -> 0.33)),
    "W" -> (Left, 0.34, Map(Up    -> 0.33, Down -> 0.33))
  )

  override def stateAt(row: Int, col: Int): Int = row * 4 + col

  override def rewardFor(tile: String): Reward = tile match {
    case "F" => -1
    case "G" => 1
    case _   => 0
  }
  override def isAccessible(tile: String): Boolean = tile != "X"
  override def isStart(tile: String): Boolean = tile == "S"
  override def isTerminal(tile: String): Boolean = tile == "F" || tile == "G"
}
