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

class AgentSimpleMDP[State, Action](theta: Double = 1e-10)
    extends Agent[State, Action, BoardEnvironment[State, Action]] {

  type StateValue = mutable.Map[State, Double]

  override def solve(environment: BoardEnvironment[State, Action]): Deterministic[State, Action] = {

    val P: environment.Board = environment.board

    val V = mutable.Map(P.keys.map(s => (s, 0d)).toSeq: _*)

    var max_delta = 0d
    var counter = 0

    val policy = P.map {
      case (state, actionsMap) =>
        (state, actionsMap.keys.zip(Stream.continually(Random.nextDouble())).minBy(_._2)._1)
    }

    println(policy)

    do {

      val old_V = copy(V)

      P.keys.foreach { state =>
        V(state) = 0d

        P(state)(policy(state)).foreach {
          case (newState, probability, reward) =>
            val value =
              if (environment.terminalStates.contains(newState)) reward
              else reward + environment.gamma * old_V(newState)

            V(newState) = V(newState) + probability * value
        }

      }

      val new_delta = Math.abs(difference(old_V, V))
      println(new_delta)
      max_delta = Math.max(max_delta, new_delta)

      println(max_delta, V)

      counter = counter + 1

    } while (max_delta > theta && counter < 10)

    Deterministic(policy)
  }

  def difference(m1: StateValue, m2: StateValue): Double =
    m1.map { case (k, v) => v - m2(k) }.toSeq.sum

  def copy(map: StateValue): StateValue = mutable.Map(map.toSeq: _*)

}
