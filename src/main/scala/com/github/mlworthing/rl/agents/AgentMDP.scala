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

import com.github.mlworthing.rl.utils.Printer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

class AgentMDP[State, Action](gamma: Double = 1d, theta: Double = 1e-10, maxIterations: Int = 100)
    extends Agent[State, Action, Environment[State, Action]] with Printer {

  type Reward = Double
  type Probability = Double
  type StateValue = mutable.Map[State, Reward]
  type Policy = mutable.Map[State, Action]
  type Knowledge = mutable.Map[State, mutable.Map[Action, Seq[(State, Int, Reward, Boolean)]]]

  override def solve(environment: Environment[State, Action]): Deterministic[State, Action] = {

    val knowledge: Knowledge = mutable.Map.empty

    val initialPolicy = createInitialPolicy(environment, knowledge)

    printDeterministicPolicy(s"Initial random policy:", initialPolicy, environment)

    var policy = initialPolicy
    var newPolicy = initialPolicy

    var policyCounter = 0

    do {
      policy = newPolicy

      val (v, moves, iterations) = evaluatePolicy(environment, policy, knowledge)

      printStateValue(s"State-value function converged after $moves moves in $iterations iterations:", v, environment)

      newPolicy = improvePolicy(v, policy, knowledge)
      policyCounter = policyCounter + 1

      printDeterministicPolicy(s"Improved policy no. $policyCounter:", newPolicy, environment)

    } while (!isStable(policy, newPolicy) && policyCounter < maxIterations)

    Deterministic(newPolicy.toMap)
  }

  def createInitialPolicy(environment: Environment[State, Action], knowledge: Knowledge): Policy = {
    val (initialState, initialActions, _) = environment.initial
    // select random policy at first
    val initialPolicy: Policy = mutable.Map(initialState -> selectRandom(initialActions))

    //update knowledge with possible initial state and actions
    knowledge(initialState) = mutable.Map.empty
    initialActions.foreach(action => knowledge(initialState)(action) = Seq((initialState, 1, 0d, false)))
    initialPolicy
  }

  def evaluatePolicy(
    environment: Environment[State, Action],
    policy: Policy,
    knowledge: Knowledge): (StateValue, Int, Int) = {

    @tailrec
    def evaluate(state: State, V: StateValue, old_V: StateValue, frame: environment.Frame, counter: Int): Int = {

      // move following the actual policy
      val action = policy(state)
      val (environment.Observation(newState, reward, newActions, isTerminal), nextFrame) =
        environment.step(action, frame)

      // update knowledge of the current state with the new discovery
      if (knowledge.contains(state)) {
        knowledge(state)(action) = {
          val moves = knowledge(state)(action)
          moves.find(_._1 == newState) match {
            //update information and number of hits for a given move (s,a,ś)
            case Some((_, p, _, _)) => moves.filterNot(_._1 == newState) :+ (newState, p + 1, reward, isTerminal)
            //add new move (s,a,ś)
            case _ => moves :+ (newState, 1, reward, isTerminal)
          }
        }
      }

      //update knowledge of the new state with new possible actions
      if (!knowledge.contains(newState)) {
        knowledge(newState) = mutable.Map(
          newActions
            .map(action =>
              action -> (if (isTerminal) Seq.empty
                         else Seq((newState, 1, 0d, isTerminal))))
            .toSeq: _*)
      }

      // maybe update policy with a new state
      policy(newState) = policy.get(newState) match {
        case Some(a) => a
        case None    => selectRandom(newActions)
      }

      val value =
        if (isTerminal) reward
        else reward + gamma * old_V(newState)

      V(state) = V(state) + probabilityOf(knowledge, state, action, newState) * value //FIXME

      if (isTerminal || counter >= maxIterations) counter
      else evaluate(newState, V, old_V, nextFrame, counter + 1)
    }

    val (initialState, _, initialFrame) = environment.initial

    //initialize State-Value function to zero
    val V: StateValue = mutable.Map().withDefaultValue(0d)

    var moves = 0
    var iterations = 0
    var delta = 0d

    do {
      val old_V = copy(V).withDefaultValue(0d)
      moves = evaluate(initialState, V, old_V, initialFrame, moves)
      delta = Math.abs(difference(V, old_V))
      iterations = iterations + 1
      printDeterministicPolicy(s"After $moves moves:", policy, environment)
      println(V)
      println(delta)
    } while (delta > theta && iterations < maxIterations)

    (V, moves, iterations)
  }

  def improvePolicy(V: StateValue, policy: Policy, knowledge: Knowledge): Policy = {

    val states = knowledge.keys.toSeq
    val newPolicy: Policy = copy(policy)

    // for each possible state
    for (state <- states) {
      val actions = knowledge(state).keys
      // initialize action value function to zero
      val Qs = mutable.Map(actions.toSeq.map(a => (a, 0d)): _*)
      // and loop through all actions available
      for (action <- actions) {
        val moves = knowledge(state)(action)
        if (moves.nonEmpty) {
          for ((newState, _, reward, isTerminal) <- moves) {
            val value =
              if (isTerminal) reward
              else reward + gamma * V(newState)
            // calculate action value function for all actions in this state
            Qs(action) = Qs(action) + probabilityOf(knowledge, state, action, newState) * value
          }
        } else {
          Qs(action) = 1d
        }
      }
      // select max action in this state
      val bestAction = Qs.maxBy(_._2)._1
      newPolicy(state) = bestAction
    }

    newPolicy
  }

  def probabilityOf(knowledge: Knowledge, state: State, action: Action, newState: State): Probability = {
    val moves = knowledge(state)(action)
    val allHits = moves.map(_._2).sum
    val hits = moves.find(_._1 == newState).map(_._2).getOrElse(0)
    if (allHits == 0) 0 else hits / allHits.toDouble
  }

  def isStable(policy: Policy, newPolicy: Policy): Boolean = policy.equals(newPolicy)

  def difference(m1: StateValue, m2: StateValue): Double =
    m1.map { case (k, v) => v - m2(k) }.toSeq.sum

  def copy[K, V](map: mutable.Map[K, V]): mutable.Map[K, V] = mutable.Map(map.toSeq: _*)

  def selectRandom[A](items: Set[A]): A = items.zip(Stream.continually(Random.nextDouble())).minBy(_._2)._1

}
