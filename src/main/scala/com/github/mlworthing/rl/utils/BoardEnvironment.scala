/*
 * Copyright 2019 ml-worthing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.mlworthing.rl
package utils

import scala.util.Random

/**
  * Board-like environment consisting of adjacent square tiles
  * and set of actions resulting in one of possible moves
  * having known probabilities of success.
  */
trait BoardEnvironment[State, Action] extends Environment[State, Action] {

  type Reward = Double
  type Probability = Double

  /** Board represents map of states to map of actions to the sequence of possible move results */
  type MoveResult = (State, Probability, Reward)
  type Board = Map[State, Map[Action, Seq[MoveResult]]]

  /** Main action move, its probability and related unlucky moves */
  type Move = (Int, Int)
  type ActionMoves = (Move, Probability, Map[Move, Probability])

  /** Textual layout definition, space and newline separated */
  def layout: String

  lazy val tiles: Array[Array[String]] = layout.lines
    .map(_.trim)
    .filterNot(_.isEmpty)
    .toArray
    .map(_.split("\\s+"))

  /** Possible moves on the board and their respected results */
  def actionMoves: Map[Action, ActionMoves]

  def stateAt(row: Int, col: Int): State
  def positionOf(state: State): (Int, Int)

  def rewardFor(tile: String): Reward
  def isAccessible(tile: String): Boolean
  def isStart(tile: String): Boolean
  def isTerminal(tile: String): Boolean

  val actions: Set[Action] = actionMoves.keySet
  val (board, initialStates, terminalStates) = parseLayout

  override val initial: (State, Set[Action]) = (initialStates(Random.nextInt(initialStates.size)), actions)

  @volatile private var currentState: State = initial._1

  override def send(action: Action): Observation = {
    val moveResults: Seq[MoveResult] = board(currentState)(action)
    val random = Random.nextDouble()
    val (newState, instantReward) = moveResults
      .collectFirst {
        case (state, probabilityLimit, reward) if random <= probabilityLimit => (state, reward)
      }
      .getOrElse((moveResults.head._1, moveResults.head._3))

    currentState = newState

    Observation(
      newState,
      instantReward,
      actions,
      initialStates.contains(currentState) || terminalStates.contains(currentState))
  }

  /** Parses square tiles board */
  private def parseLayout: (Board, Seq[State], Seq[State]) = {

    val tileAt: Map[(Int, Int), String] =
      (for ((row, r) <- tiles.zipWithIndex; (tile, c) <- row.zipWithIndex)
        yield (r, c) -> tile).toMap

    def fit(i: Int, minInc: Int, maxExc: Int): Int = if (i < minInc) minInc else if (i >= maxExc) maxExc - 1 else i

    def computeMoveResult(position: (Int, Int), move: (Int, Int), probability: Double): Option[MoveResult] = {
      val target =
        (fit(position._1 + move._1, 0, tiles.length), fit(position._2 + move._2, 0, tiles(position._1).length))
      val tile = tileAt(target)
      if (isAccessible(tile)) {
        val reward = rewardFor(tile)
        val state = stateAt(target._1, target._2)
        Some((state, probability, reward))
      } else None
    }

    val initialStates: Seq[State] = tileAt.collect { case ((r, c), tail) if isStart(tail)     => stateAt(r, c) }.toSeq
    val terminalStates: Seq[State] = tileAt.collect { case ((r, c), tail) if isTerminal(tail) => stateAt(r, c) }.toSeq

    val board: Board = tileAt.map {
      case (position, _) =>
        stateAt(position._1, position._2) -> {
          actionMoves.map {
            case (action, (move, probability, slips)) =>
              action -> (computeMoveResult(position, move, probability) :: slips.map {
                case (m, p) => computeMoveResult(position, m, p)
              }.toList)
                .collect { case Some(m) => m }
          }
        }
    }

    (board, initialStates, terminalStates)
  }

  override def description: String = layout

  def show[V](values: State => Option[V], format: (State, V) => String): String =
    (for ((row, r) <- tiles.zipWithIndex)
      yield
        (for ((tile, c) <- row.zipWithIndex)
          yield values(stateAt(r, c)).map(v => format(stateAt(r, c), v)).getOrElse(tile))
          .map(t => f"$t%10s")
          .mkString(" | "))
      .mkString("\r\n")

}

trait UpRightDownLeft {

  lazy val Up = (-1, 0)
  lazy val Right = (0, 1)
  lazy val Down = (1, 0)
  lazy val Left = (0, -1)
}

trait S0FGXFormat {

  def rewardFor(tile: String): Double = tile match {
    case "F" => -1
    case "G" => 1
    case _   => 0
  }

  def isAccessible(tile: String): Boolean = tile != "X"
  def isStart(tile: String): Boolean = tile == "S"
  def isTerminal(tile: String): Boolean = tile == "F" || tile == "G"
}
