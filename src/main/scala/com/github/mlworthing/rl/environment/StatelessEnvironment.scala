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

package com.github.mlworthing.rl.environment

import com.github.mlworthing.rl.Environment

/**
  * Infinite stateless, non-terminal
  * environment with static set of actions.
  */
trait StatelessEnvironment[Action] extends Environment[Unit, Action] {

  type Frame = Unit

  val actions: Set[Action]
  def reward(action: Action): Double

  final override def initial: (Unit, Set[Action], Unit) = ((), actions, ())
  final override def step(action: Action, frame: Frame): (Observation, Frame) =
    (Observation((), reward(action), actions, isTerminal = false), ())

}
