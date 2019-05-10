/*
 * Copyright 2019 ml-worthing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.mlworthing.rl
package mdp

import com.github.mlworthing.rl.utils.BoardEnvironment

import scala.collection.mutable
import scala.util.Random

class AgentSimpleMDP[State, Action](gamma: Double = 1d, theta: Double = 1e-10, maxIterations: Int = 100)
    extends Agent[State, Action, BoardEnvironment[State, Action]] {

  type StateValue = mutable.Map[State, Double]

  override def solve(environment: BoardEnvironment[State, Action]): Deterministic[State, Action] = {

    val P: environment.Board = environment.board

    //initial state value function
    val V = mutable.Map(P.keys.map(s => (s, 0d)).toSeq: _*)

    var delta = 0d
    var counter = 0

    // selecting random policy
    val policy: Map[State, Action] = P.filterNot(_._2.isEmpty).map {
      case (state, actionsMap) =>
        (state, actionsMap.keys.zip(Stream.continually(Random.nextDouble())).minBy(_._2)._1)
    }

    println(environment.layout)
    println(s"Evaluating policy:")
    println()
    println(
      environment
        .show(policy.get, (_: State, action: Action) => action.toString, cellLength = 1, showForTerminalTiles = false))
    println()

    do {

      val old_V = copy(V)

      // for each state on the board
      P.keys
        .foreach { state =>
          // initialize state value to be 0
          V(state) = 0d
          // then move following the actual policy
          val moves: Seq[environment.MoveResult] =
            policy.get(state).map(m => P(state)(m)).getOrElse(Seq.empty)

          moves.foreach {
            case (newState, probability, reward) =>
              val value =
                if (environment.terminalStates.contains(newState)) reward
                else reward + gamma * old_V(newState)

              // update the state value
              V(state) = V(state) + probability * value
          }

          delta = Math.abs(difference(old_V, V))

        }

      counter = counter + 1

    } while (delta > theta && counter < maxIterations)

    println(s"After $counter iterations state-value function converged to:")
    println()
    println(
      environment
        .show(V.get, (_: State, d: Double) => f"$d%+2.4f", cellLength = 10, showForTerminalTiles = true))
    println()

    Deterministic(policy)
  }

  def difference(m1: StateValue, m2: StateValue): Double =
    m1.map { case (k, v) => v - m2(k) }.toSeq.sum

  def copy(map: StateValue): StateValue = mutable.Map(map.toSeq: _*)

}
