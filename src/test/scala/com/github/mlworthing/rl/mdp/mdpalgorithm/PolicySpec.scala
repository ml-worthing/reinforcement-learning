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
    intercept[UnsupportedOperationException](randomπ(state1, Left)) should have message "In state [1] an action [Left] is not valid"

    randomπ.greedyAction(state1) shouldBe Up withClue "the highest probability for state1 was for for action Up"
    randomπ.greedyAction(state2) shouldBe Right
    intercept[UnsupportedOperationException](randomπ.greedyAction(state3Terminal)) should have message "No actions available for state [state=3]. Is it terminal state?"
    randomπ.greedyAction(state4) shouldBe Down
    randomπ.greedyAction(state6) shouldBe Up
    randomπ.greedyAction(state7) shouldBe Down
    randomπ.greedyAction(state8) shouldBe Right
    intercept[UnsupportedOperationException](randomπ.greedyAction(state9Terminal)) should have message "No actions available for state [state=9]. Is it terminal state?"
  }

  "update policy" in {
    import com.github.mlworthing.rl.mdp.mdpalgorithm.MdpTestData.GridWorld3x3._
    implicit val c = mdpContext

    val π: Policy[State, Action] = Policy.createRandomPolicy()

    π.asImmutable() shouldBe ImmutablePolicy(Map[(State, Action), P](
      (4, Up) -> 0.3354105723044021,
      (4, Down) -> 0.664589427695598,
      (8, Right) -> 0.579502111211338,
      (1, Up) -> 0.6939573922955898,
      (2, Left) -> 0.4489614868638832,
      (1, Right) -> 0.30604260770441016,
      (2, Right) -> 0.5510385131361167,
      (7, Right) -> 0.16787995031629982,
      (6, Down) -> 0.2099241993057882,
      (8, Left) -> 0.420497888788662,
      (7, Down) -> 0.8321200496837002,
      (6, Up) -> 0.7900758006942119)
    )

    π(4) = Up

    π.asImmutable() shouldBe ImmutablePolicy(Map[(State, Action), P](
      (4, Up) -> 0.3829723669413546,
      (4, Down) -> 0.6170276330586453,
      (8, Right) -> 0.579502111211338,
      (1, Up) -> 0.6939573922955898,
      (2, Left) -> 0.4489614868638832,
      (1, Right) -> 0.30604260770441016,
      (2, Right) -> 0.5510385131361167,
      (7, Right) -> 0.16787995031629982,
      (6, Down) -> 0.2099241993057882,
      (8, Left) -> 0.420497888788662,
      (7, Down) -> 0.8321200496837002,
      (6, Up) -> 0.7900758006942119)
    )

    π(4) = Up

    π.asImmutable() shouldBe ImmutablePolicy(Map[(State, Action), P](
      (4, Up) -> 0.4321454340416437,
      (4, Down) -> 0.5678545659583563,
      (8, Right) -> 0.579502111211338,
      (1, Up) -> 0.6939573922955898,
      (2, Left) -> 0.4489614868638832,
      (1, Right) -> 0.30604260770441016,
      (2, Right) -> 0.5510385131361167,
      (7, Right) -> 0.16787995031629982,
      (6, Down) -> 0.2099241993057882,
      (8, Left) -> 0.420497888788662,
      (7, Down) -> 0.8321200496837002,
      (6, Up) -> 0.7900758006942119)
    )

    π(4) = Up

    π.asImmutable() shouldBe ImmutablePolicy(Map[(State, Action), P](
      (4, Up) -> 0.4820872842662689,
      (4, Down) -> 0.5179127157337311,
      (8, Right) -> 0.579502111211338,
      (1, Up) -> 0.6939573922955898,
      (2, Left) -> 0.4489614868638832,
      (1, Right) -> 0.30604260770441016,
      (2, Right) -> 0.5510385131361167,
      (7, Right) -> 0.16787995031629982,
      (6, Down) -> 0.2099241993057882,
      (8, Left) -> 0.420497888788662,
      (7, Down) -> 0.8321200496837002,
      (6, Up) -> 0.7900758006942119)
    )

    π(4) = Up

    π.asImmutable() shouldBe ImmutablePolicy(Map[(State, Action), P](
      (4, Up) -> 0.531901171184772,
      (4, Down) -> 0.4680988288152279,
      (8, Right) -> 0.579502111211338,
      (1, Up) -> 0.6939573922955898,
      (2, Left) -> 0.4489614868638832,
      (1, Right) -> 0.30604260770441016,
      (2, Right) -> 0.5510385131361167,
      (7, Right) -> 0.16787995031629982,
      (6, Down) -> 0.2099241993057882,
      (8, Left) -> 0.420497888788662,
      (7, Down) -> 0.8321200496837002,
      (6, Up) -> 0.7900758006942119)
    )

    π(4) = Up

    π.asImmutable() shouldBe ImmutablePolicy(Map[(State, Action), P](
      (4, Up) -> 0.5806994655945577,
      (4, Down) -> 0.41930053440544246,
      (8, Right) -> 0.579502111211338,
      (1, Up) -> 0.6939573922955898,
      (2, Left) -> 0.4489614868638832,
      (1, Right) -> 0.30604260770441016,
      (2, Right) -> 0.5510385131361167,
      (7, Right) -> 0.16787995031629982,
      (6, Down) -> 0.2099241993057882,
      (8, Left) -> 0.420497888788662,
      (7, Down) -> 0.8321200496837002,
      (6, Up) -> 0.7900758006942119)
    )
  }

  "update policy should converge" in {
    import com.github.mlworthing.rl.mdp.mdpalgorithm.MdpTestData.GridWorld3x3._
    implicit val c = mdpContext

    val π: Policy[State, Action] = Policy.createRandomPolicy()


    def updatePolicy() = {
      π(state1) = Up
      π(state2) = Right
      π(state4) = Down
      π(state6) = Up
      π(state7) = Right
      π(state8) = Left
    }

    val diffs = (0 to 10000).map(_ => {
      val oldPolicy = π.asImmutable()
      updatePolicy()
      val newPolicy = π.asImmutable()
      val diff = oldPolicy.diff(newPolicy)
      //      println(diff)
      diff
    })

    diffs.zip(diffs.drop(20)).foreach { case (prev, next) =>
      next should be <= prev
    }

    diffs.last shouldBe 0
  }
}
