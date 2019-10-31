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

class PoissonDistributionSpec extends WordSpec with Matchers {

  "PoissonDistribution" should {
    "calculate probability of an event given expected value" in {
      val pd = PoissonDistribution(2)
      pd(0) shouldBe 0.1353352832366127
      pd(1) shouldBe 0.2706705664732254
      pd(2) shouldBe 0.2706705664732254
      pd(3) shouldBe 0.18044704431548358
      pd(4) shouldBe 0.09022352215774179
      pd(5) shouldBe 0.03608940886309672
    }

    "calculate probability of an event given expected value and capped max value" in {
      val pd = PoissonDistribution(2)
      pd(0, 5) shouldBe 0.1353352832366127
      pd(1, 5) shouldBe 0.2706705664732254
      pd(2, 5) shouldBe 0.2706705664732254
      pd(3, 5) shouldBe 0.18044704431548358
      pd(4, 5) shouldBe 0.09022352215774179
      pd(5, 5) shouldBe 1 - (pd(0) + pd(1) + pd(2) + pd(3) + pd(4))
      pd(6, 5) shouldBe 0.052653017343711084
    }
  }

}
