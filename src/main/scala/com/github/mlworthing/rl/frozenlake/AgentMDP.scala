package com.github.mlworthing.rl
package frozenlake

class AgentMDP[State, Action] extends Agent[State, Action, Environment[State, Action]] {

  override def solve(environment: Environment[State, Action]): Deterministic[State, Action] = {
    val (state, actions) = environment.initial
    val V: Map[State, Double] = Map().withDefault(_ => 0.0)

    def result(V: Map[State, Double], n: Map[State, Set[State]]): Map[State, Double] =
      /*V.map {
        case (state, old) =>
          val newValue = actions.map { action =>
            val observation = environment.send(action)
            if (observation.isTerminal) {
              observation.reward
            } else {
              observation.reward + n(state).map()
            }
          }
          (state -> newValue)
      }
    }

    actions.map(a => V.get(a))
    (0 to 100).foreach { episodeNum =>
      environment.send(actions.head)
      .

    }*/
      //choose an action
      //apply action in environemnt
      //read the reward, observation
      //update your V
      ???

    Deterministic(Map())
  }

}
