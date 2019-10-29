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
package agents

import com.github.mlworthing.rl.environments.FiniteEnvironment
import com.github.mlworthing.rl.utils.Printer

import scala.collection.mutable
import scala.util.Random

/**
  * A MDP Agent applying dynamic programming (tabular) method.
  * Requires a complete and accurate model of the environment.
  * @param gamma future rewards discount factor
  * @param theta state-value convergence threshold
  * @param maxIterations max number of iterations
  * @tparam State states represents anything we can know that might be useful in making decisions
  * @tparam Action actions represents decisions we want to learn how to make
  */
final class DynamicProgrammingEvaluateImproveAgent[State, Action](
  gamma: Double = 1d,
  theta: Double = 1e-10,
  maxIterations: Int = 100)
    extends Agent[State, Action, FiniteEnvironment[State, Action]] with Printer {

  type Reward = Double
  type Probability = Double
  type StateValue = mutable.Map[State, Reward]
  type Policy = Map[State, Action]

  def solve(environment: FiniteEnvironment[State, Action]): Deterministic[State, Action] = {

    println(environment.description)

    // first select random policy
    val initialPolicy: Policy = environment.states.map { state =>
      (state, environment.actions(state).zip(Stream.continually(Random.nextDouble())).minBy(_._2)._1)
    }.toMap

    printPolicy(s"Initial random policy:", initialPolicy, environment)

    var currentPolicy = initialPolicy
    var nextPolicy = initialPolicy

    var globalCounter = 0
    var policyCounter = 0

    //initialize State-Value function to zero
    val stateValue = mutable.Map(environment.states.toSeq.map(s => (s, 0d)): _*)

    do {
      currentPolicy = nextPolicy

      val (nextStateValue, counter) = evaluatePolicy(environment)(stateValue, currentPolicy)
      globalCounter = globalCounter + counter

      printStateValue(s"State-value function after $globalCounter iterations converged to:", stateValue, environment)

      nextPolicy = improvePolicy(environment)(nextStateValue, currentPolicy)
      policyCounter = policyCounter + 1

      printPolicy(s"Improved policy no. $policyCounter:", nextPolicy, environment)

    } while (nextPolicy ne currentPolicy)

    Deterministic(nextPolicy)
  }

  def evaluatePolicy(
    environment: FiniteEnvironment[State, Action])(stateValue: StateValue, currentPolicy: Policy): (StateValue, Int) = {

    var delta = 0d
    var counter = 0

    do {
      delta = 0d
      // for each possible state
      for (state <- environment.states) {
        val previousStateValue = stateValue(state)
        // initialize state value to be 0
        stateValue(state) = 0d
        // then follow the actual policy
        val possibleTransitions: Seq[(State, Probability, Reward)] =
          currentPolicy.get(state).map(m => environment.transitions(state)(m)).getOrElse(Seq.empty)
        // for each possible transition
        for ((nextState, probability, reward) <- possibleTransitions) {
          val value =
            if (environment.terminalStates.contains(nextState)) reward
            else reward + gamma * stateValue(nextState)
          // and update the state value
          stateValue(state) = stateValue(state) + probability * value
          delta = Math.max(delta, Math.abs(previousStateValue - stateValue(state)))
        }
      }
      counter = counter + 1
    } while (delta > theta && counter < maxIterations)

    (stateValue, counter)
  }

  def improvePolicy(
    environment: FiniteEnvironment[State, Action])(stateValue: StateValue, currentPolicy: Policy): Policy = {

    var stable = true
    var nextPolicy: Policy = Map.empty

    // initialize state-action values to zero
    val stateActionValue = mutable.Map[State, mutable.Map[Action, Reward]]()
    for (state <- environment.states) {
      stateActionValue(state) = mutable.Map(environment.actions(state).toSeq.map(a => (a, 0d)): _*)
    }

    // then for each possible state
    for (state <- environment.states) {
      val actionValues = stateActionValue(state)
      // loop through all actions available
      for (action <- environment.actions(state)) {
        // and for each possible transition
        for ((nextState, probability, reward) <- environment.transitions(state)(action)) {
          val value =
            if (environment.terminalStates.contains(nextState)) reward
            else reward + gamma * stateValue(nextState)
          // update action value
          actionValues(action) = actionValues(action) + probability * value
        }
      }
      // then select an action producing max value in this state
      val actionSelected = actionValues.maxBy(_._2)._1
      // and update policy
      nextPolicy = nextPolicy.updated(state, actionSelected)
      stable = stable && currentPolicy(state) == actionSelected
    }

    if (stable) currentPolicy else nextPolicy
  }

}
