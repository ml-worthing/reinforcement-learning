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

import com.github.mlworthing.rl.UnitSpec
import com.github.mlworthing.rl.utils.ToSeq._

import scala.util.Random

class MdpSpec extends UnitSpec {


  "if you're hungry you should eat" in {

    type State = String
    type Action = String

    val hungryS = "hungryS"
    val fedS = "fedS"

    val states: States[State] = States[State](
      terminalStates = fedS,
      nonTerminalStates = hungryS
    )

    val eatA = "eatA"
    val sitA = "sitA"

    val mdpDescription = MdpDescription[State, Action](
      states = states,
      actions = {
        case `hungryS` => List(eatA, sitA)
        case `fedS` => List()
      },
      rewards = {
        case (`hungryS`, `eatA`) => 1.0
        case (_, _) => 0.0
      },
      p = {
        case (`fedS`,    1.0, `hungryS`, `eatA`) => 0.7
        case (`hungryS`, 0.0, `hungryS`, `eatA`) => 0.3

        case (`hungryS`, 0.0, `hungryS`, `sitA`) => 1.0
        case (_,         _,    _,         _)     => 0.0
      }
    )

    (0 to 10000).foreach {iter =>

      implicit val mdpContext: MdpContext[State, Action] = MdpContext(
        mdpDescription = mdpDescription,
        γ = 0.9,
        random = new Random(iter)
      )

      val policy: Policy[State, Action] = Mdp.iterateValue()
      val whatToDo: Action = policy(hungryS)
      whatToDo shouldBe eatA withClue s"[iter=$iter]"

    }



  }
}
