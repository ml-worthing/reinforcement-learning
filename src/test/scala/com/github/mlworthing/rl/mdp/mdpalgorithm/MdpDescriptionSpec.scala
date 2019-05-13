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
import com.github.mlworthing.rl.mdp.mdpalgorithm.MdpTestData.GridWorld3x3

class MdpDescriptionSpec extends UnitSpec {

  "possible destinations for s,a pair" in {

    import GridWorld3x3._
    implicit def toSeq[T](t: T): Seq[T] = Seq(t)

    val expectedDestinations: Map[(State, Action), Iterable[State]] = Map(
      (state1, Up) -> state4,
      (state1, Right) -> state2,
      (state2, Left) -> state1,
      (state2, Right) -> state3Terminal,
      (state4, Up) -> state7,
      (state4, Down) -> state1,
      (state6, Up) -> state9Terminal,
      (state6, Down) -> state3Terminal,
      (state7, Right) -> state8,
      (state7, Down) -> state4,
      (state8, Left) -> state7,
      (state8, Right) -> state9Terminal
    )

    mdpDescription.ś shouldBe expectedDestinations
  }

  "transition probabilities " in {

    import GridWorld3x3._

    mdpDescription.transitionP(state1, state1, Up) shouldBe 0.0
    mdpDescription.transitionP(state2, state1, Up) shouldBe 0.0
    //... TODO: finish it
  }

}
