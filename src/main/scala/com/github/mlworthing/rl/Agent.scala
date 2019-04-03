package com.github.mlworthing.rl

/**
  * Reinforcement learning agent API.
  * Solves the environment finding the chain of actions which gives the best reward.
  */
trait Agent[State, Action, E <: Environment[State, Action]] {

  def solve(environment: E): Seq[Action]

}
