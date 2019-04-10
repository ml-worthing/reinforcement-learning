package com.github.mlworthing.rl
package frozenlake

class AgentMDP[State, Action] extends Agent[State, Action, Environment[State, Action]] {

  override def solve(environment: Environment[State, Action]): Deterministic[State, Action] =
    ???
}
