package com.github.mlworthing.rl.environment

import com.github.mlworthing.rl.Environment

/**
  * Infinite stateless, non-terminal
  * environment with static set of actions.
  */
trait StatelessEnvironment[Action] extends Environment[Unit, Action] {

  type Frame = Unit

  val actions: Set[Action]
  def reward(action: Action): Double

  final override def initial: (Unit, Set[Action], Unit) = ((), actions, ())
  final override def send(action: Action, frame: Frame): (Observation, Frame) =
    (Observation((), reward(action), actions, isTerminal = false), ())

}
