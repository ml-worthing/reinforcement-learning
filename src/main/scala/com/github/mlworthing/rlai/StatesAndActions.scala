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

package com.github.mlworthing.rlai

//TODO: some better name for it?
trait StatesAndActions[S, A] {

  def states: States[S]

  def actions: Actions[S, A]

  states.nonTerminalStates.foreach(s =>
    require(
      actions(s).nonEmpty,
      s"Non terminal states should have at least one action available. State [state=$s] has no available actions"
    )
  )

  states.terminalStates.foreach(s =>
    require(
      actions(s).isEmpty,
      s"Terminal states should not have any actions available. State [state=$s] had actions [${actions(s).mkString(",")}]"
    )
  )
}
