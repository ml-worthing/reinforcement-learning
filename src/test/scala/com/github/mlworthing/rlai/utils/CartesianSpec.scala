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

class CartesianSpec extends UnitSpec {


  "cartesian" in {

    val result = Cartesian.cartesian(List(
      List(1,2),
      List(3,4),
      List(5,6)
    ))

    result shouldBe List(
      List(5, 3, 1),
      List(5, 3, 2),
      List(5, 4, 1),
      List(5, 4, 2),
      List(6, 3, 1),
      List(6, 3, 2),
      List(6, 4, 1),
      List(6, 4, 2)
    )

    Cartesian.cartesian(List(List())) shouldBe Nil
    Cartesian.cartesian(List(Nil)) shouldBe Nil

    //TODO: maybe later make if lazy and efficient
//    val bigList: List[List[Int]] = List.fill(1000)(List.fill(1000)(0))
//    val x: Iterable[List[Int]] = Cartesian.cartesian(bigList) //shouldNot fail
//    x.take(10)
  }

}
