package com.github.mlworthing.rl
package utils

import scala.util.Random

/**
  * Board-like environment consisting of adjacent square tiles
  * and set of actions resulting in one of possible moves
  * having known probabilities of success.
  */
trait BoardEnvironment[State, Action] extends Environment[State, Action] {

  /** Reward discount factor. Makes reward value decline with the passing search time. */
  val gamma: Double

  type Reward = Double
  type Probability = Double
  type MoveResult = (State, Probability, Reward)
  type Board = Map[State, Map[Action, Seq[MoveResult]]]
  type Move = (Int, Int)

  /** Main action move, its probability and related unlucky moves */
  type ActionMoves = (Move, Probability, Map[Move, Probability])

  /** Textual layout definition, space and newline separated */
  def layout: String

  /** Possible moves on the board and their respected results */
  def actionMoves: Map[Action, ActionMoves]

  def stateAt(row: Int, col: Int): State
  def rewardFor(tile: String): Reward
  def isAccessible(tile: String): Boolean
  def isStart(tile: String): Boolean
  def isTerminal(tile: String): Boolean

  val actions: Set[Action] = actionMoves.keySet
  val (board, initialStates, terminalStates) = parseLayout

  override val initial: (State, Set[Action]) = (initialStates(Random.nextInt(initialStates.size)), actions)

  @volatile private var currentState: State = initial._1
  @volatile private var discount: Double = 0d

  override def send(action: Action): Observation = {
    val moveResults: Seq[MoveResult] = board(currentState)(action)
    val random = Random.nextDouble()
    val (newState, instantReward) = moveResults
      .collectFirst {
        case (state, probabilityLimit, reward) if random <= probabilityLimit => (state, reward)
      }
      .getOrElse((moveResults.head._1, moveResults.head._3))

    currentState = newState
    discount = if (gamma == 1d) 1d else if (discount == 0d) 1d else discount * gamma

    Observation(
      newState,
      discount * instantReward,
      actions,
      initialStates.contains(currentState) || terminalStates.contains(currentState))
  }

  private def parseLayout: (Board, Seq[State], Seq[State]) = {

    val tiles: Array[Array[String]] = layout.lines
      .map(_.trim)
      .filterNot(_.isEmpty)
      .toArray
      .map(_.split("\\s+"))

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
                .foldLeft(List.empty[MoveResult])((list, moveResult) =>
                  list match {
                    case Nil       => moveResult :: list
                    case head :: _ => (moveResult._1, moveResult._2 + head._2, moveResult._3) :: list
                })
                .reverse
          }
        }
    }

    (board, initialStates, terminalStates)
  }

  override def description: String = layout

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
