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

package com.github.mlworthing.rl.mdp.mdpalgorithm

object MdpTestData {

  /**
    *  3x3 grid world, wall at 5, goal at 9, pitfall at 3 and explicit transition probability
    */
  object GridWorld3x3 {
    private implicit def toSeq[T](t: T): Seq[T] = Seq(t)

    type State = Int

    sealed trait Action
    case object Up extends Action
    case object Down extends Action
    case object Left extends Action
    case object Right extends Action

    val state1 = 1
    val state2 = 2
    val state3Terminal = 3
    val state4 = 4
    val state6 = 6
    val state7 = 7
    val state8 = 8
    val state9Terminal = 9

    val states: States[State] = States(
      terminalStates = List(state3Terminal,state9Terminal),
      nonTerminalStates = List(state1,state2,state4,state6,state7,state8)
    )
    val d: MdpDescription[State, Action] = MdpDescription[State, Action](
      states = states,
      actions = _ => List(Up, Down, Left, Right),
      rewards = {
        case (`state2`, Right) => -1.0
        case (`state6`, Down) => -1.0
        case (`state6`, Up) => 1.0
        case (`state8`, Right) => 1.0
        case (_, _) => 0.0
      },
      p = {
        case (`state4`, 0.0, 1, Up) => 1.0
        case (`state2`, 0.0, 1, Right) => 1.0
        case (`state1`, 0.0, 2, Left) => 1.0
        case (`state3Terminal`, -1.0, 2, Right) => 1.0
        case (`state7`, 0.0, 4, Up) => 1.0
        case (`state1`, 0.0, 4, Down) => 1.0
        case (`state9Terminal`, 1.0, 6, Up) => 1.0
        case (`state3Terminal`, -1.0, 6, Down) => 1.0
        case (`state8`, 0.0, 7, Right) => 1.0
        case (`state4`, 0.0, 7, Down) => 1.0
        case (`state9Terminal`, 1.0, 8, Right) => 1.0
        case (`state7`, 0.0, 8, Left) => 1.0
        case _ => 0.0
      }
    )

    //def because it creates random and you don't wan't to reuse the same random in many tests
    def mdpContext: MdpContext[State, Action] = MdpContext(d)
  }




}
