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
  * Reinforcement learning Agent API.
  *
  * MDPs are meant to be a straightforward framing of the problem of
  * learning from interaction to achieve a goal.
  * The learner and decision maker is called the `Agent`.
  * Anything that cannot be changed arbitrarily by the Agent is considered
  * to be outside of it and thus part of the Environment
  *
  * The Agent is parametrised by the `State`, `Action` and by the targeted `Environment` type.
  * In general, actions can be any decisions we want to learn how to make,
  * and the states can be anything we can know that might be useful in making them.
  *
  * Any problem of learning goal-directed behavior can be reduced to three signals
  * passing back and forth between an agent and its environment:
  * 1) one signal to represent the choices made by the agent (the actions),
  * 2) one signal to represent the basis on which the choices are made (the states),
  * 3) and one signal to define the agent’s goal (the rewards)
  *
  * `Agent` solves the `Environment` finding the best `Policy`.
  */
trait Agent[State, Action, E <: Environment[State, Action]] {

  /** Algorithm finding the best policy given the constraints */
  def solve(environment: E): Policy[State, Action]

  /** Override to provide better human-readable description */
  def description: String = toString
}
