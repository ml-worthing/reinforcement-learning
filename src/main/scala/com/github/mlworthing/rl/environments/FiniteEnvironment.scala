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
  * An MDP environment where the sets of states, actions, and rewards
  * all have a finite number of elements.
  */
trait FiniteEnvironment[State, Action] extends Environment[State, Action] {

  type Reward = Double
  type Probability = Double

  type Transition = (State, Probability, Reward)

  val states: Set[State]
  val actions: State => Set[Action]
  val transitions: State => Action => Seq[Transition]

  val initialStates: Set[State]
  val terminalStates: Set[State]

  def stateOf(frame: Frame): State
  def nextFrame(nextState: State, previousFrame: Option[Frame]): Frame

  override def initial: (State, Set[Action], Frame) = {
    val state = initialStates.toSeq(Random.nextInt(initialStates.size))
    (state, actions(state), nextFrame(state, None))
  }

  final override def step(action: Action, frame: Frame): (Observation, Frame) = {
    val possibleTransitions: Seq[Transition] = transitions(stateOf(frame))(action)
    val (nextState, _, reward): Transition = {
      if (possibleTransitions.isEmpty) (stateOf(frame), 0d, 0d)
      else {
        // select next state based on its probability
        val random = Random.nextDouble()
        var i = 0
        var level = possibleTransitions.head._2
        while (random >= level) {
          i = i + 1
          level = if (i == possibleTransitions.size) {
            i = i - 1
            1d
          } else level + possibleTransitions(i)._2
        }
        possibleTransitions(i)
      }
    }
    val nextActions = actions(nextState)
    (Observation(nextState, reward, nextActions, terminalStates.contains(nextState)), nextFrame(nextState, Some(frame)))
  }

}
