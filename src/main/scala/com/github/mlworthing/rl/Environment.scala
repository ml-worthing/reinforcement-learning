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

/**
  * Reinforcement learning Environment API.
  * Parametrised by the State and Action type.
  * The type of reward is fixed to be Double.
  * The time flow and other features are hidden from the Agent, but should
  * be taken into account when calculating state and reward.
  */
trait Environment[State, Action] {

  /** Frame, an opaque internal state of an environment */
  type Frame

  case class Observation(state: State, reward: Double, actions: Set[Action], isTerminal: Boolean)

  /** Some initial state, corresponding set of actions and an initial frame */
  def initial: (State, Set[Action], Frame)

  /**
    * The primary way for an Agent to interact with an Environment.
    *
    * Agent steps into an action at the previous frame
    * and receives back an observation for a current frame:
    *
    * The observation tells you:
    * - state after an action
    * - reward gained for this action
    * - set of the next possible actions (can be static or dynamic)
    * - is the state terminal or not?
    */
  def step(action: Action, previousFrame: Frame): (Observation, Frame)

  /** Human-readable environment description */
  def description: String

  def show[V](
    values: State => Option[V],
    format: (State, V) => String,
    cellLength: Int,
    showForTerminalTiles: Boolean): String
}
