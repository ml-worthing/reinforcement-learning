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
    * - list of the next possible actions (can be static or dynamic).
    * Should return current or some initial state if action is None
    * */
  def send(action: Option[Action]):(State, Double, Seq[Action])

  /** Is given state terminal or not? */
  def isTerminal(state: State): Boolean
}
