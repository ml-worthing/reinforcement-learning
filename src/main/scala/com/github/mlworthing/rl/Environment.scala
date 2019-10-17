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
  *
  * The thing an Agent interacts with,
  * comprising everything outside the agent.
  *
  * The agent selects actions and the environment responds to
  * these actions and presents new situations to the agent.
  *
  * The environment gives rise to rewards, special numerical values
  * that the agent seeks to maximize over time through its choice of actions.
  *
  * The Environment is parametrised by the `State` and `Action` types.
  * In general, actions can be any decisions we want to learn how to make,
  * and the states can be anything we can know that might be useful in making them.
  *
  * The type of reward is fixed to be Double.
  * The time flow and other features are hidden from the Agent, but should
  * be taken into account when calculating state and reward.
  *
  * Internal `Frame` type represents hidden state of an Environment.
  * The next state may only depends on a precedent state (frame) and an action,
  * it must have so called `Markov property`.
  */
trait Environment[State, Action] {

  /** Frame, an opaque, hidden internal state of an environment */
  type Frame

  /**
    * An observation received after stepping into an action.
    *
    * @param state state after an action
    * @param reward the reward signal is your way of communicating to the agent what you want it to achieve,
    *               not how you want it achieved
    * @param actions set of the next possible actions (can be static or dynamic)
    * @param isTerminal is the state terminal or not?
    */
  case class Observation(state: State, reward: Double, actions: Set[Action], isTerminal: Boolean)

  /** Some initial state, corresponding set of actions and an initial frame */
  def initial: (State, Set[Action], Frame)

  /**
    * The primary way for an Agent to interact with an Environment.
    *
    * Agent steps into an action at the precedent frame
    * and receives back an observation and a new frame:
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
