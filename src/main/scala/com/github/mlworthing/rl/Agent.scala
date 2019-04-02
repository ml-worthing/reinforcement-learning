package com.github.mlworthing.rl

/**
  * Reinforcement learning agent API.
  * Solves the environment finding the chain of actions which gives the best reward.
  */
trait Agent {

  def solve[State, Action](environment: Environment[State, Action]): Seq[Action]

}
