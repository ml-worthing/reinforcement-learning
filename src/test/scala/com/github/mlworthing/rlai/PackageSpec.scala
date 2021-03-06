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

class PackageSpec extends UnitSpec {


  "reverseRange" in {

    reverseRange(List(1,2,3)) shouldBe (2 to 0 by -1)
    reverseRange(List(0)) shouldBe (0 to 0 by -1)
    reverseRange(List()) shouldBe (-1 to 0 by -1)

    //example use case
    val a: Array[Char] = "boom".toArray
    val x = for {
      i <- reverseRange(a) //iterates over index
    } yield a(i)

    x.mkString shouldBe "moob"

  }

}
