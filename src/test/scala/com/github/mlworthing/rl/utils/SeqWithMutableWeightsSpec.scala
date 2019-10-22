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

package com.github.mlworthing.rl.utils

import org.scalatest.{Matchers, WordSpec}

class SeqWithMutableWeightsSpec extends WordSpec with Matchers {

  "Probabilities" should {
    "keep weights for keys" in {
      val underTest = new SeqWithMutableWeights[Int](Seq(1, 2, 3))
      underTest(1) shouldBe 1d
      underTest(2) shouldBe 1d
      underTest(3) shouldBe 1d
      underTest(4) shouldBe 1d
    }

    "allow to specify initial weight" in {
      val underTest = new SeqWithMutableWeights[Int](Seq(1, 2, 3), initialWeight = 0d)
      underTest(1) shouldBe 0d
      underTest(2) shouldBe 0d
      underTest(3) shouldBe 0d
      underTest(4) shouldBe 0d
    }

    "allow to specify minimal weight" in {
      val underTest = new SeqWithMutableWeights[Int](Seq(1, 2, 3), initialWeight = 0d, minimalWeight = 1d)
      underTest(1) shouldBe 1d
      underTest(2) shouldBe 1d
      underTest(3) shouldBe 1d
      underTest(4) shouldBe 1d
      underTest(3) = 0d
      underTest(3) shouldBe 1d
    }

    "allow to specify transformation function" in {
      val underTest = new SeqWithMutableWeights[Int](Seq(1, 2, 3), boost = Math.exp, initialWeight = 0d)
      underTest.probabilityOf(1) === 1 / 3d
      underTest.probabilityOf(2) === 1 / 3d
      underTest.probabilityOf(3) === 1 / 3d

      underTest(1) = 1d
      underTest(2) = 2d
      underTest(3) = 3d
      val denominator = Math.exp(1) + Math.exp(2) + Math.exp(3)
      val probabilities = underTest.probabilities
      probabilities(1) === Math.exp(1) / denominator
      probabilities(2) === Math.exp(2) / denominator
      probabilities(3) === Math.exp(3) / denominator
    }

    "update weight for a given key" in {
      val underTest = new SeqWithMutableWeights[Int](Seq(1, 2, 3), initialWeight = 0d)
      underTest(1) shouldBe 0d
      underTest(1) = 2.13d
      underTest(1) shouldBe 2.13d
      underTest(2) shouldBe 0d
      underTest(3) shouldBe 0d
    }

    "pass negative weight for a given key" in {
      val underTest = new SeqWithMutableWeights[Int](Seq(1, 2, 3), initialWeight = 0d, minimalWeight = 0d)
      underTest(1) shouldBe 0d
      underTest(2) = 2d
      underTest(1) = -2.13d
      underTest(1) shouldBe 0d
      underTest(2) shouldBe 4.13d
      underTest(3) shouldBe 2.13d
    }

    "calculate probability of an item" in {
      val underTest = new SeqWithMutableWeights[Int](Seq(1, 2, 3))
      underTest.probabilityOf(1) shouldBe 1 / 3d
      underTest(1) = 1d
      underTest(2) = 2d
      underTest(3) = 3d
      underTest.probabilityOf(1) === 1 / 6d
      underTest.probabilityOf(2) === 2 / 6d
      underTest.probabilityOf(3) === 3 / 6d

      underTest(2) = 5d
      underTest.probabilityOf(1) === 1 / 9d
      underTest.probabilityOf(2) === 5 / 9d
      underTest.probabilityOf(3) === 3 / 9d

      underTest(1) = -3d
      underTest.probabilityOf(1) === 0 / 12d
      underTest.probabilityOf(2) === 8 / 12d
      underTest.probabilityOf(3) === 4 / 12d
    }

    "select random key according to weights" in {
      val underTest = new SeqWithMutableWeights[Int](Seq(1, 2, 3))
      underTest(1) = 1d
      underTest(2) = 2d
      underTest(3) = 3d
      val sample = (0 to 6000).map(_ => underTest.selectRandom)
      sample.distinct should have size 3
      val frequencies = sample.groupBy(identity).mapValues(_.size)
      frequencies(1) should (be > 800).and(be < 1200)
      frequencies(2) should (be > 1800).and(be < 2200)
      frequencies(3) should (be > 2800).and(be < 3200)

      underTest(2) = 5d
      val sample2 = (0 to 9000).map(_ => underTest.selectRandom)
      sample2.distinct should have size 3
      val frequencies2 = sample2.groupBy(identity).mapValues(_.size)
      frequencies2(1) should (be > 900).and(be < 1100)
      frequencies2(2) should (be > 4700).and(be < 5300)
      frequencies2(3) should (be > 2800).and(be < 3200)
    }

    "keep score of a max weight item" in {
      val underTest = new SeqWithMutableWeights[Int](Seq(1, 2, 3))
      underTest(1) = 1d
      underTest(2) = 2d
      underTest(3) = 3d
      underTest.max === 3
      underTest(2) = 5d
      underTest.max === 2
    }
  }

}
