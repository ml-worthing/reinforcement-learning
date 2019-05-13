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

    val randomπ: Policy[State, Action] = Policy.createRandomPolicy()

    randomπ(state1, Up) shouldBe 0.6939573922955898 //the highest probability for state1
    intercept[UnsupportedOperationException](randomπ(state1, Down)) should have message "In state [1] an action [Down] is not valid"
    randomπ(state1, Right) shouldBe 0.30604260770441016
    intercept[UnsupportedOperationException](randomπ(state1, Left))  should have message "In state [1] an action [Left] is not valid"

    randomπ.greedyAction(state1) shouldBe Up withClue "the highest probability for state1 was for for action Up"
    randomπ.greedyAction(state2) shouldBe Right
    intercept[UnsupportedOperationException](randomπ.greedyAction(state3Terminal)) should have message "No actions available for state [state=3]. Is it terminal state?"
    randomπ.greedyAction(state4) shouldBe Down
    randomπ.greedyAction(state6) shouldBe Up
    randomπ.greedyAction(state7) shouldBe Down
    randomπ.greedyAction(state8) shouldBe Right
    intercept[UnsupportedOperationException](randomπ.greedyAction(state9Terminal)) should have message "No actions available for state [state=9]. Is it terminal state?"


    //now update
    randomπ(state1) = Right //was Up
    randomπ.greedyAction(state1) shouldBe Right

    randomπ(state1, Right) shouldBe 1.0
    randomπ(state1, Up) shouldBe 0.0
    intercept[UnsupportedOperationException](randomπ(state1, Down)) should have message "In state [1] an action [Down] is not valid"
    intercept[UnsupportedOperationException](randomπ(state1, Left))  should have message "In state [1] an action [Left] is not valid"


  }
}
