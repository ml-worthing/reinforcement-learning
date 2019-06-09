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

import com.github.mlworthing.rlai.utils.UnitSpec

class MdpExamples extends UnitSpec {

  "3x3 grid world" in {

    import MdpTestData.GridWorld3x3._
    implicit val c = mdpContext

    //7 8 9
    //4 5 6
    //1 2 3

    val π: Policy[State, Action] = Policy.createRandomPolicy()
    val v: ValueFunction[State] = ValueFunction.createRandomValueFunction[State, Action]()
    Mdp.evaluatePolicy(v, π)
    Mdp.iteratePolicy(v, π)

    π(state2) shouldBe Left
    π(state1) shouldBe Up
    π(state4) shouldBe Up
    π(state7) shouldBe Right
    π(state8) shouldBe Right
    π(state6) shouldBe Up

    val πBest = Mdp.iterateValue()

    πBest(state2) shouldBe Left
    πBest(state1) shouldBe Up
    πBest(state4) shouldBe Up
    πBest(state7) shouldBe Right
    πBest(state8) shouldBe Right
    πBest(state6) shouldBe Up

  }


}
