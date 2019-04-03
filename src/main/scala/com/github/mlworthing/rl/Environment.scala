package com.github.mlworthing.rl

/**
  * Reinforcement learning environment API, parametrised by
  * the State and Action types. The type of reward is fixed to be Double.
  * The time flow and other features are hidden from the Agent, but should
  * be taken into account when calculating state and reward.
  */
trait Environment[State, Action] {

  /**
    * The primary way for an Agent to interact with an Environment.
    * Agent sends an action (or none) and receives:
    * - state observed after action
    * - reward gained for this action
    * - set of the next possible actions (can be static or dynamic).
    * Sending None action is a way to discover initial state and possible actions.
    **/
  def send(action: Option[Action]): (State, Double, Set[Action])

  /** Is given state terminal or not? */
  def isTerminal(state: State): Boolean

  /** Environment description */
  def description: String
}

/**
  * Simplified stationary (stateless, non-terminal) environment having static set of actions.
  */
trait StationaryEnvironment[Action] extends Environment[Unit, Action] {

  val actions: Set[Action]

  def reward(action: Action): Double

  final override def send(action: Option[Action]): (Unit, Double, Set[Action]) =
    ((), reward(action.getOrElse(actions.head)), actions)

  final override def isTerminal(state: Unit): Boolean = false

}
