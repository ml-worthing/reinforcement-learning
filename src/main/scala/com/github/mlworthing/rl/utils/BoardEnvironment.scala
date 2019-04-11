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
  type MoveResult = (State, Probability, Reward)
  type Board = Map[State, Map[Action, Seq[MoveResult]]]
  type Move = (Int, Int)
  type ActionMoves = (Move, Probability, Map[Move, Probability])

  def layout: String
  def actionMoves: Map[Action, ActionMoves]

  def stateAt(row: Int, col: Int): State
  def rewardFor(tile: String): Reward
  def isAccessible(tile: String): Boolean
  def isStart(tile: String): Boolean
  def isTerminal(tile: String): Boolean

  val gamma: Double

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
