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
  * Parametrised by the State, Action and targeted environment type.
  * Solves the environment finding the best policy.
  */
trait Agent[State, Action, E <: Environment[State, Action]] {

  /** Algorithm finding the best policy given the constraints */
  def solve(environment: E): Policy[State, Action]

  /** Override to provide better human-readable description */
  def description: String = toString
}
