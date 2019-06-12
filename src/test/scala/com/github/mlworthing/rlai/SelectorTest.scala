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

import com.github.mlworthing.rlai.utils.{Selector, UnitSpec}

class SelectorTest extends UnitSpec {

  "select by level" in {

    val chances = List(10, 20, 30)
    Selector.selectByLevel(chances, 0) shouldBe 0
    Selector.selectByLevel(chances, 1) shouldBe 0
    Selector.selectByLevel(chances, 9) shouldBe 0
    Selector.selectByLevel(chances, 10) shouldBe 1 withClue "the first 'level' for which we select the second index"
    Selector.selectByLevel(chances, 11) shouldBe 1
    Selector.selectByLevel(chances, 29) shouldBe 1 withClue "the last 'level' for which we select the second index"
    Selector.selectByLevel(chances, 30) shouldBe 2 withClue "the first 'level' for which we select the third index"
    Selector.selectByLevel(chances, 59) shouldBe 2 withClue "the first 'level' for which we select the third index"
  }

  "select by chances - sanity check" in {
    val chances = List(10, 20, 30)
    implicit val randomness: Randomness = Randomness()

    (0 to 1000).foreach { _ =>
      val selected = Selector.select(chances)
      selected should be >= 0
      selected should be < 3 withClue "the chances has 3 positions"
    }
  }

  "select by chances - sanity check - double" in {
    val chances = List(10.0, 20.0, 30.0)
    implicit val randomness: Randomness = Randomness()

    (0 to 1000).foreach { _ =>
      val selected = Selector.selectDouble(chances)
      selected should be >= 0
      selected should be < 3 withClue "the chances has 3 positions"
    }
  }

  "demo" in {
    implicit val randomness: Randomness = Randomness()

    val chances = List(60, 20, 10, 10)

    //      (1 to 20).foreach {_ =>
    //        println(select(chances))
    //      }
    val demo = (1 to 10000)
      .map(_ => Selector.select(chances))
      .groupBy(identity)
      .map(x => (x._1, x._2.size))
      .toList
      .sortBy(_._1)
      .map(x => s"Index ${x._1} was selected ~${x._2 / 100.0}% times. It had ${chances(x._1)} chances to be selected")
      .mkString("\n")
    println(demo)
  }

  "demo doubles" in {
    implicit val randomness: Randomness = Randomness()

    val chances = List(60.0, 20.0, 10.0, 10.0)

    //      (1 to 20).foreach {_ =>
    //        println(select(chances))
    //      }
    val demo = (1 to 10000)
      .map(_ => Selector.selectDouble(chances))
      .groupBy(identity)
      .map(x => (x._1, x._2.size))
      .toList
      .sortBy(_._1)
      .map(x => s"Index ${x._1} was selected ~${x._2 / 100.0}% times. It had ${chances(x._1)} chances to be selected")
      .mkString("\n")
    println(demo)
  }

}
