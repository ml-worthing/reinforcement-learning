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

package com.github.mlworthing.rlai.utils

import com.github.mlworthing.rlai.utils.MathNotation._

class MathNotationSpec extends UnitSpec {

  "Methods tests" - {
    "sum" in {

      val ś = List(1.0, 2.0, 3.0)
      val r = List(10.0, 20.0)

      Σ(ś)(identity) shouldBe 6.0
      Σ(ś)(1 + _) shouldBe 9.0

      Σ(ś, r)(_ + _) shouldBe 102.0
      Σ(ś, r)(_ + _) shouldBe ś.flatMap(x => r.map(x + _)).sum

      Σ(ś, r)((x,y)=> x+2.0/y)
    }


    "argmax" in {
      val args = List(1, 2, 3, 4, 5, 6)

      argmax(args)(_.toDouble) shouldBe 6
      argmax(args)(x => x + 10) shouldBe 6
      argmax(args)(x => x % 3) shouldBe 2 withClue "returns first argument"
    }

    "max" in {
      val args = List(1, 2, 3, 4, 5, 6)
      max_(args)(_ + 10.0) shouldBe 16
      max_(List(9.0))(identity) shouldBe 9.0

    }
  }




}
