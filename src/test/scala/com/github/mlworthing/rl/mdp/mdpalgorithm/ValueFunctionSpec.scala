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

import scala.collection.mutable

class ValueFunctionSpec extends UnitSpec {

  "createValueFunction" in {

    import MdpTestData.GridWorld3x3._
    implicit val c = mdpContext

    val v: ValueFunction[State] = ValueFunction.createValueFunction[State, Action](s => s + 10.0)

    states.nonTerminalStates.foreach(s => v(s) shouldBe s + 10.0 withClue s"state was $s")
    states.terminalStates.foreach(s => v(s) shouldBe 0.0 withClue s"state was $s")

    val expectedValueFunction = new ValueFunction(mutable.Map(
      state1 -> 11.0,
      state2 -> 12.0,
      state3Terminal -> 0.0, //terminal states result in value 0
      state4 -> 14.0,
      state6 -> 16.0,
      state7 -> 17.0,
      state8 -> 18.0,
      state9Terminal -> 0.0 //terminal states result in value 0
    ))

    v shouldBe expectedValueFunction
  }

  "createRandomValueFunction" in {

    import MdpTestData.GridWorld3x3._
    implicit val c = mdpContext

    val v: ValueFunction[State] = ValueFunction.createRandomValueFunction[State, Action](2.0)

    v(state1) shouldBe -2.8760986182818136
    v(state2) shouldBe 1.2683901503553607
    v(state4) shouldBe 0.4521240256643285
    v(state6) shouldBe 0.5549200948069762
    v(state7) shouldBe 0.3686383110878779
    v(state8) shouldBe -0.7304275548303855
    v(state3Terminal) shouldBe 0
    v(state9Terminal) shouldBe 0
  }


}
