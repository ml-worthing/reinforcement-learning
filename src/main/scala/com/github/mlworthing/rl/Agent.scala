package com.github.mlworthing.rl

/**
  * Reinforcement learning Agent API.
  * Parametrised by the State, Action and targeted environment type.
  * Solves the environment finding the best policy.
  */
trait Agent[State, Action, E <: Environment[State, Action]] {

  /** Algorithm finding the best policy given the constraints */
  def solve(environment: E): Policy[State, Action]

  /** Override to provide better human-readable description */
  def description: String = toString
}
