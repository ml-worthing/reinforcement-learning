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

package com.github.mlworthing.rl.problems

import com.github.mlworthing.rlai.utils.UnitSpec

class JackCarsRentalTest extends UnitSpec {

  "assert JackCarsRental transitions completeness" in {
    val problem = JackCarsRental()
    for {
      state  <- problem.states.take(5)
      action <- problem.actions(state)
    } {
      checkTransitions(problem.transitions(state)(action))
    }

  }

  def checkTransitions(ts: Seq[((Int, Int), Double, Double)]): Unit = {
    ts.foreach(t => t._2 > 0d shouldBe true withClue s"$t")
    ts.foreach(t => t._2 < 1d shouldBe true withClue s"$t")
    ts.map(_._2).sum === 1d
  }
}
