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

class PolicySpec extends UnitSpec {

  "random policy" in {

    import com.github.mlworthing.rl.mdp.mdpalgorithm.MdpTestData.GridWorld3x3._
    implicit val c = mdpContext

    val π: Policy[State, Action] = Policy.createRandomPolicy()

    π(state1, Up) shouldBe 0.045774920160791814
    π(state1, Down) shouldBe 0.22688968376926685
    π(state1, Right) shouldBe 0.4214923975812657  //the highest probability for state1
    π(state1, Left) shouldBe 0.30584299848867563

    π.greedyAction(state1) shouldBe Right withClue "the highest probability for state1 was for for action Right"
    π.greedyAction(state2) shouldBe Left
    π.greedyAction(state3Terminal) shouldBe Up
    π.greedyAction(state4) shouldBe Down
    π.greedyAction(state6) shouldBe Right
    π.greedyAction(state7) shouldBe Right
    π.greedyAction(state8) shouldBe Right
    π.greedyAction(state9Terminal) shouldBe Left


    //now update
    π(state1) = Down
    π(state1, Up) shouldBe 0.0
    π(state1, Down) shouldBe 1.0
    π(state1, Right) shouldBe 0.0
    π(state1, Left) shouldBe 0.0

    π.greedyAction(state1) shouldBe Down

  }
}
