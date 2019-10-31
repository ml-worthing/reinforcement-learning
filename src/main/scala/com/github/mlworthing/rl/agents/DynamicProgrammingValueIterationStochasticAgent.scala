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

package com.github.mlworthing.rl.agents

import com.github.mlworthing.rl.environments.FiniteEnvironment
import com.github.mlworthing.rl.utils.Printer
import com.github.mlworthing.rl.{Agent, Stochastic}

import scala.collection.mutable

/**
  * MDP Agent applying dynamic programming (tabular) value iteration method.
  * Requires a complete and accurate model of the environment.
  * Calculates stochastic policy.
  *
  * @param gamma future rewards discount factor
  * @param theta state-value convergence threshold
  * @param maxIterations max number of iterations
  * @tparam State states represents anything we can know that might be useful in making decisions
  * @tparam Action actions represents decisions we want to learn how to make
  */
final class DynamicProgrammingValueIterationStochasticAgent[State, Action](
  gamma: Double = 1d,
  theta: Double = 1e-10,
  maxIterations: Int = 100)
    extends Agent[State, Action, FiniteEnvironment[State, Action]] with Printer {

  type Reward = Double
  type Probability = Double
  type StateValue = mutable.Map[State, Reward]
  type Policy = mutable.Map[State, mutable.Map[Action, Double]]

  def solve(environment: FiniteEnvironment[State, Action]): Stochastic[State, Action] = {

    println(environment.description)

    var delta = 0d
    var counter = 0
    var policyCounter = 0

    // first select random policy
    var policy: Policy = mutable.Map(environment.states.map { state =>
      (state, mutable.Map(environment.actions(state).map(a => (a, 1d / environment.actions(state).size)).toSeq: _*))
    }.toSeq: _*)

    printStochasticPolicy(s"Initial policy:", policy, environment)

    //initialize state values to zero
    val stateValue = mutable.Map(environment.states.toSeq.map(s => (s, 0d)): _*)

    // initialize state-action values to zero
    val stateActionValue = mutable.Map[State, mutable.Map[Action, Reward]]()
    for (state <- environment.states) {
      stateActionValue(state) = mutable.Map(environment.actions(state).toSeq.map(a => (a, 0d)): _*)
    }

    do {
      var stable = true
      delta = 0d
      // then for each possible state
      for (state <- environment.states) {
        val previousStateValue = stateValue(state)
        val actionValue = stateActionValue(state)
        // loop through all actions available
        for (action <- environment.actions(state)) {
          actionValue(action) = 0d
          // and for each possible transition
          for ((nextState, probability, reward) <- environment.transitions(state)(action)) {
            val value =
              if (environment.isTerminalState(nextState)) reward
              else reward + gamma * stateValue(nextState)
            // update action value
            actionValue(action) = actionValue(action) + probability * value
          }
        }
        // then prefer actions producing max value in this state
        val (_, isStable) = updateActionPreferences(actionValue, policy(state), 1e-10)
        stable = stable && isStable
        // and update state value
        stateValue(state) = actionValue.map { case (a, r) => policy(state)(a) * r }.sum
        delta = Math.max(delta, Math.abs(previousStateValue - stateValue(state)))
      }

      counter = counter + 1
      if (!stable) {
        policyCounter = policyCounter + 1
        printStateValue(s"State-value function after $counter iterations:", stateValue, environment)
        printStochasticPolicy(s"Improved policy no. $policyCounter:", policy, environment)
      }

    } while (delta > theta && counter < maxIterations)

    printStateValue(
      f"Resulting state-value function has converged after $counter iterations with max delta=$delta%4.8f",
      stateValue,
      environment)

    printStochasticPolicy(s"Final policy no. $policyCounter:", policy, environment)

    Stochastic(policy.mapValues(_.toMap).toMap)
  }

  def updateActionPreferences(
    actionValue: mutable.Map[Action, Reward],
    actionPreferences: mutable.Map[Action, Probability],
    tolerance: Double): (Double, Boolean) = {
    val max = actionValue.maxBy(_._2)._2
    val count = actionValue.count { case (_, v) => Math.abs(max - v) < tolerance }
    val probability = 1d / count
    var stable = true
    actionPreferences.keysIterator.foreach { a =>
      val p = actionPreferences(a)
      actionPreferences(a) = if (Math.abs(max - actionValue(a)) < tolerance) probability else 0d
      stable = stable && actionPreferences(a) == p
    }
    (max, stable)
  }

}
