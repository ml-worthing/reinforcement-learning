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

  type Reward = Double
  type Probability = Double
  type StateValue = mutable.Map[State, Reward]
  type Policy = Map[State, Action]

  override def solve(environment: BoardEnvironment[State, Action]): Deterministic[State, Action] = {

    val P: environment.Board = environment.board

    println(environment.layout)

    // first select random policy
    val initialPolicy: Policy = P.map {
      case (state, actionsMap) =>
        (state, actionsMap.keys.zip(Stream.continually(Random.nextDouble())).minBy(_._2)._1)
    }

    println(s"Initial random policy:")
    println()
    println(
      environment
        .show(
          initialPolicy.get,
          (_: State, action: Action) => action.toString,
          cellLength = 1,
          showForTerminalTiles = false))
    println()

    var policy = initialPolicy
    var newPolicy = initialPolicy

    var policyCounter = 0

    do {
      policy = newPolicy

      val (v, counter) = evaluatePolicy(environment)(policy)

      println(s"State-value function after $counter iterations converged to:")
      println()
      println(
        environment
          .show(v.get, (_: State, d: Double) => f"$d%+2.4f", cellLength = 10, showForTerminalTiles = true))
      println()

      newPolicy = improvePolicy(environment)(v)
      policyCounter = policyCounter + 1

      println(s"Improved policy no. $policyCounter:")
      println()
      println(
        environment
          .show(
            newPolicy.get,
            (_: State, action: Action) => action.toString,
            cellLength = 1,
            showForTerminalTiles = false))
      println()

    } while (!isStable(policy, newPolicy))

    Deterministic(newPolicy)
  }

  def evaluatePolicy(environment: BoardEnvironment[State, Action])(policy: Policy): (StateValue, Int) = {

    val P: environment.Board = environment.board
    val states = P.keys.toSeq

    var delta = 0d
    var counter = 0

    //initialize State-Value function to zero
    val V = mutable.Map(P.keys.toSeq.map(s => (s, 0d)): _*)

    do {

      val old_V = copy(V)

      // for each possible state
      for (state <- states) {
        // initialize state value to be 0
        V(state) = 0d
        // then move following the actual policy
        val moves: Seq[(State, Probability, Reward)] =
          policy.get(state).map(m => P(state)(m)).getOrElse(Seq.empty)

        for ((newState, probability, reward) <- moves) {
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

    (V, counter)
  }

  def improvePolicy(environment: BoardEnvironment[State, Action])(V: StateValue): Policy = {

    val P: environment.Board = environment.board
    val states = P.keys.toSeq

    var newPolicy: Policy = Map.empty

    // for each possible state
    for (state <- states) {
      val actions = P(state).keys
      // initialize action value function to zero
      val Qs = mutable.Map(actions.toSeq.map(a => (a, 0d)): _*)
      // and loop through all actions available
      for (action <- actions) {
        for ((newState, probability, reward) <- P(state)(action)) {
          val value =
            if (environment.terminalStates.contains(newState)) reward
            else reward + gamma * V(newState)
          // calculate action value function for all actions in this state
          Qs(action) = Qs(action) + probability * value
        }
      }
      // select max action in this state
      val actionSelected = Qs.maxBy(_._2)._1
      newPolicy = newPolicy.updated(state, actionSelected)
    }

    newPolicy
  }

  def isStable(policy: Policy, newPolicy: Policy): Boolean = policy == newPolicy

  def difference(m1: StateValue, m2: StateValue): Double =
    m1.map { case (k, v) => v - m2(k) }.toSeq.sum

  def copy(map: StateValue): StateValue = mutable.Map(map.toSeq: _*)

}
