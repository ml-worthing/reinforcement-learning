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

import scala.collection.mutable
import scala.util.Random

class AgentMDP[State, Action](gamma: Double = 1d, theta: Double = 1e-10, maxIterations: Int = 100)
    extends Agent[State, Action, Environment[State, Action]] {

  type Reward = Double
  type Probability = Double
  type StateValue = mutable.Map[State, Reward]
  type Policy = mutable.Map[State, Action]
  type Knowledge = mutable.Map[State, mutable.Map[Action, Seq[(State, Int, Reward, Boolean)]]]

  override def solve(environment: Environment[State, Action]): Deterministic[State, Action] = {

    val P: Knowledge = mutable.Map.empty.withDefaultValue(mutable.Map.empty.withDefaultValue(Seq.empty))

    val (initialState, initialActions) = environment.initial

    // first select random policy
    val initialPolicy: Policy =
      mutable.Map(initialState -> selectRandom(initialActions))

    // incentive to explore
    val incentive = 1d

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

      val (v, counter) = evaluatePolicy(environment, policy, P, incentive)

      println(s"State-value function after $counter iterations converged to:")
      println()
      println(
        environment
          .show(v.get, (_: State, d: Double) => f"$d%+2.4f", cellLength = 10, showForTerminalTiles = true))
      println()

      newPolicy = improvePolicy(v, policy, P)
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
      println(P)
      println()

    } while (!isStable(policy, newPolicy) && policyCounter < maxIterations)

    Deterministic(newPolicy.toMap)
  }

  def evaluatePolicy(
    environment: Environment[State, Action],
    policy: Policy,
    P: Knowledge,
    incentive: Double): (StateValue, Int) = {

    val states = policy.keys.toSeq

    var delta = 0d
    var counter = 0

    //initialize State-Value function to zero
    val V: StateValue = mutable.Map().withDefaultValue(0d)

    do {

      val old_V = copy(V).withDefaultValue(incentive)
      // for each possible state
      for (state <- states) {
        // initialize state value to be 0
        V(state) = 0d
        // then move following the actual policy
        val action = policy(state)
        val environment.Observation(newState, reward, newActions, isTerminal) = environment.send(action)

        // update knowledge of the current state with the new discovery
        P(state) = P.get(state) match {
          case Some(actionMap) =>
            actionMap(action) = actionMap.get(action) match {
              case Some(moves) =>
                moves.find(_._1 == newState) match {
                  //update information and number of hits for a given move (s,a,ś)
                  case Some((_, p, _, _)) => moves.filterNot(_._1 == newState) :+ (newState, p + 1, reward, isTerminal)
                  //add new move (s,a,ś)
                  case _ => moves :+ (newState, 1, reward, isTerminal)
                }
              case _ =>
                Seq((newState, 1, reward, isTerminal))
            }
            actionMap
          case _ =>
            //add new discovered action result to the knowledge
            mutable.Map(action -> Seq((newState, 1, reward, isTerminal)))
        }

        //update knowledge of the new state with new possible actions
        P(newState) = P.get(newState) match {
          case Some(actionMap) => actionMap
          case _ =>
            mutable.Map(newActions.map(action => action -> Seq.empty).toSeq: _*)
        }

        // maybe update policy with new state
        policy(newState) = policy.get(newState) match {
          case Some(a) => a
          case None    => selectRandom(newActions)
        }

        // shuffle policy if returns to the same state
        if (newState == state || isTerminal) {
          policy(state) = selectRandom(newActions.filterNot(_ == action))
        }

        val value =
          if (isTerminal) reward
          else reward + gamma * old_V(newState)

        V(state) = V(state) + probabilityOf(P, state, action, newState) * value

        delta = Math.abs(difference(old_V, V))
      }

      counter = counter + 1

    } while (delta > theta && counter < maxIterations)

    (V, counter)
  }

  def improvePolicy(V: StateValue, policy: Policy, P: Knowledge): Policy = {

    val states = P.keys.toSeq
    val newPolicy: Policy = copy(policy)

    // for each possible state
    for (state <- states) {
      val actions = P(state).keys
      // initialize action value function to zero
      val Qs = mutable.Map(actions.toSeq.map(a => (a, 0d)): _*)
      // and loop through all actions available
      for (action <- actions) {
        val moves = P(state)(action)
        if (moves.isEmpty) {
          Qs(action) = 1d + Random.nextDouble()
        } else
          for ((newState, _, reward, isTerminal) <- moves) {
            val value =
              if (isTerminal) reward
              else reward + gamma * V(newState)
            // calculate action value function for all actions in this state
            Qs(action) = Qs(action) + probabilityOf(P, state, action, newState) * value
          }
      }
      // select max action in this state
      newPolicy(state) = Qs.maxBy(_._2)._1
    }

    newPolicy
  }

  def probabilityOf(P: Knowledge, state: State, action: Action, newState: State): Probability = {
    val moves = P(state)(action)
    val allHits = moves.map(_._2).sum
    val hits = moves.find(_._1 == newState).map(_._2).getOrElse(0)
    if (allHits == 0) 0d else hits / allHits.toDouble
  }

  def isStable(policy: Policy, newPolicy: Policy): Boolean = policy == newPolicy

  def difference(m1: StateValue, m2: StateValue): Double =
    m1.map { case (k, v) => v - m2(k) }.toSeq.sum

  def copy[K, V](map: mutable.Map[K, V]): mutable.Map[K, V] = mutable.Map(map.toSeq: _*)

  def selectRandom[A](items: Set[A]): A = items.zip(Stream.continually(Random.nextDouble())).minBy(_._2)._1

}
