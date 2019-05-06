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

package com.github.mlworthing.rl
package utils

import mdp.{FrozenLake, SuperMario}
import org.scalatest.{Matchers, WordSpec}

class BoardEnvironmentSpec extends WordSpec with Matchers {

  "BoardEnvironment" should {

    "parse FrozenLake" in {
      val frozenLake = FrozenLake()
      frozenLake.initialStates should contain.only(0)
      frozenLake.terminalStates should contain.allOf(5, 7, 11, 12, 15)
      println(frozenLake.board)
    }

    "parse SuperMario" in {
      val superMario = SuperMario()
      superMario.initialStates should contain.only(8)
      superMario.terminalStates should contain.allOf(3, 7)
      println(superMario.board)
    }
  }

  /*val frozenLakeBoard = Map(
    0 -> Map(
      "W" -> Seq((0, 0.33, 0), (0, 0.33, 0), (4, 0.33, 0)),
      "E" -> Seq((1, 0.33, 0), (0, 0.33, 0), (4, 0.33, 0)),
      "N" -> Seq((0, 0.33, 0), (0, 0.33, 0), (1, 0.33, 0)),
      "S" -> Seq((1, 0.33, 0), (0, 0.33, 0), (4, 0.33, 0))
    ),
    1 -> Map(
      "W" -> Seq((0, 0.33, 0), (1, 0.33, 0), (5, 0.33, -1)),
      "E" -> Seq((2, 0.33, 0), (1, 0.33, 0), (5, 0.33, -1)),
      "N" -> Seq((1, 0.33, 0), (0, 0.33, 0), (2, 0.33, 0)),
      "S" -> Seq((5, 0.33, -1), (0, 0.33, 0), (2, 0.33, 0))
    ),
    2 -> Map(
      "W" -> Seq((1, 0.33, 0), (2, 0.33, 0), (6, 0.33, 0)),
      "E" -> Seq((3, 0.33, 0), (2, 0.33, 0), (6, 0.33, 0)),
      "N" -> Seq((2, 0.33, 0), (1, 0.33, 0), (3, 0.33, 0)),
      "S" -> Seq((6, 0.33, 0), (1, 0.33, 0), (3, 0.33, 0))
    ),
    3 -> Map(
      "W" -> Seq((2, 0.33, 0), (3, 0.33, 0), (7, 0.33, -1)),
      "E" -> Seq((3, 0.33, 0), (3, 0.33, 0), (7, 0.33, -1)),
      "N" -> Seq((3, 0.33, 0), (2, 0.33, 0), (3, 0.33, 0)),
      "S" -> Seq((7, 0.33, -1), (2, 0.33, 0), (3, 0.33, 0))
    ),
    4 -> Map(
      "W" -> Seq((4, 0.33, 0), (0, 0.33, 0), (8, 0.33, 0)),
      "E" -> Seq((5, 0.33, -1), (0, 0.33, 0), (8, 0.33, 0)),
      "N" -> Seq((0, 0.33, 0), (4, 0.33, 0), (5, 0.33, -1)),
      "S" -> Seq((8, 0.33, 0), (4, 0.33, 0), (5, 0.33, -1))
    ),
    5 -> Map(
      "W" -> Seq((4, 0.33, 0), (1, 0.33, 0), (9, 0.33, 0)),
      "E" -> Seq((6, 0.33, 0), (1, 0.33, 0), (9, 0.33, 0)),
      "N" -> Seq((1, 0.33, 0), (4, 0.33, 0), (6, 0.33, 0)),
      "S" -> Seq((9, 0.33, 0), (4, 0.33, 0), (6, 0.33, 0))
    ),
    6 -> Map(
      "W" -> Seq((5, 0.33, -1), (2, 0.33, 0), (10, 0.33, 0)),
      "E" -> Seq((7, 0.33, -1), (2, 0.33, 0), (10, 0.33, 0)),
      "N" -> Seq((2, 0.33, 0), (5, 0.33, -1), (7, 0.33, -1)),
      "S" -> Seq((10, 0.33, 0), (5, 0.33, -1), (7, 0.33, -1))
    ),
    7 -> Map(
      "W" -> Seq((6, 0.33, 0), (3, 0.33, 0), (11, 0.33, -1)),
      "E" -> Seq((7, 0.33, -1), (3, 0.33, 0), (11, 0.33, -1)),
      "N" -> Seq((3, 0.33, 0), (6, 0.33, 0), (7, 0.33, -1)),
      "S" -> Seq((11, 0.33, -1), (6, 0.33, 0), (7, 0.33, -1))
    ),
    8 -> Map(
      "W" -> Seq((8, 0.33, 0), (4, 0.33, 0), (12, 0.33, 0)),
      "E" -> Seq((9, 0.33, 0), (4, 0.33, 0), (12, 0.33, 0)),
      "N" -> Seq((4, 0.33, 0), (8, 0.33, 0), (9, 0.33, 0)),
      "S" -> Seq((12, 0.33, -1), (8, 0.33, 0), (9, 0.33, 0))
    ),
    9 -> Map(
      "W" -> Seq((8, 0.33, 0), (5, 0.33, -1), (13, 0.33, 0)),
      "E" -> Seq((10, 0.33, 0), (5, 0.33, -1), (13, 0.33, 0)),
      "N" -> Seq((5, 0.33, -1), (8, 0.33, 0), (10, 0.33, 0)),
      "S" -> Seq((13, 0.33, 0), (8, 0.33, 0), (10, 0.33, 0))
    ),
    10 -> Map(
      "W" -> Seq((9, 0.33, 0), (6, 0.33, 0), (14, 0.33, 0)),
      "E" -> Seq((11, 0.33, -1), (6, 0.33, 0), (15, 0.33, 1)),
      "N" -> Seq((6, 0.33, 0), (9, 0.33, 0), (11, 0.33, -1)),
      "S" -> Seq((14, 0.33, 0), (9, 0.33, 0), (11, 0.33, -1))
    ),
    11 -> Map(
      "W" -> Seq((10, 0.33, 0), (7, 0.33, -1), (15, 0.33, 1)),
      "E" -> Seq((11, 0.33, -1), (7, 0.33, -1), (15, 0.33, 1)),
      "N" -> Seq((7, 0.33, -1), (10, 0.33, 0), (11, 0.33, -1)),
      "S" -> Seq((15, 0.33, 1), (10, 0.33, 0), (11, 0.33, -1))
    ),
    12 -> Map(
      "W" -> Seq((12, 0.33, -1), (8, 0.33, 0), (12, 0.33, 0)),
      "E" -> Seq((13, 0.33, 0), (8, 0.33, 0), (12, 0.33, 0)),
      "N" -> Seq((8, 0.33, 0), (12, 0.33, 0), (13, 0.33, 0)),
      "S" -> Seq((12, 0.33, -1), (12, 0.33, 0), (13, 0.33, 0))
    ),
    13 -> Map(
      "W" -> Seq((12, 0.33, -1), (9, 0.33, 0), (13, 0.33, 0)),
      "E" -> Seq((14, 0.33, 0), (9, 0.33, 0), (13, 0.33, 0)),
      "N" -> Seq((9, 0.33, 0), (12, 0.33, 0), (14, 0.33, 0)),
      "S" -> Seq((13, 0.33, 0), (12, 0.33, 0), (14, 0.33, 0))
    ),
    14 -> Map(
      "W" -> Seq((13, 0.33, 0), (10, 0.33, 0), (14, 0.33, 0)),
      "E" -> Seq((15, 0.33, 1), (10, 0.33, 0), (14, 0.33, 0)),
      "N" -> Seq((10, 0.33, 0), (13, 0.33, 0), (15, 0.33, 1)),
      "S" -> Seq((14, 0.33, 0), (13, 0.33, 0), (15, 0.33, 1))
    ),
    15 -> Map(
      "W" -> Seq((14, 0.33, 0), (11, 0.33, -1), (15, 0.33, 1)),
      "E" -> Seq((15, 0.33, 1), (11, 0.33, -1), (15, 0.33, 1)),
      "N" -> Seq((11, 0.33, -1), (14, 0.33, 0), (15, 0.33, 1)),
      "S" -> Seq((15, 0.33, 1), (14, 0.33, 0), (15, 0.33, 1))
    )
  )*/

}
