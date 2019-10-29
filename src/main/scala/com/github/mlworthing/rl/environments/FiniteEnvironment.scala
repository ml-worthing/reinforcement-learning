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

package com.github.mlworthing.rl.environments

import com.github.mlworthing.rl.Environment

import scala.util.Random

/**
  * In a finite MDP environment, the sets of states, actions, and rewards
  * all have a finite number of elements and that its dynamics are given
  * by a set of probabilities (complete transition graph).
  */
trait FiniteEnvironment[State, Action] extends Environment[State, Action] {

  type Frame = State

  type Reward = Double
  type Probability = Double

  type Response = (State, Probability, Reward)
  type TransitionGraph = Map[State, Map[Action, Seq[Response]]]

  val initialStates: Seq[State]
  val terminalStates: Set[State]
  val transitionGraph: TransitionGraph

  override def initial: (State, Set[Action], State) = {
    val state = initialStates(Random.nextInt(initialStates.size))
    val actions = transitionGraph(state).keySet
    (state, actions, state)
  }

  final override def step(action: Action, frame: Frame): (Observation, Frame) = {
    val possibleResponses: Seq[Response] = transitionGraph(frame)(action)
    val (nextState, _, reward): Response = {
      if (possibleResponses.isEmpty) (frame, 0d, 0d)
      else {
        // select next state based on its probability
        val random = Random.nextDouble()
        var i = 0
        var level = possibleResponses.head._2
        while (random >= level) {
          i = i + 1
          level = if (i == possibleResponses.size) {
            i = i - 1
            1d
          } else level + possibleResponses(i)._2
        }
        possibleResponses(i)
      }
    }
    val nextActions = transitionGraph(nextState).keySet
    (Observation(nextState, reward, nextActions, terminalStates.contains(nextState)), nextState)
  }

}
