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

import com.github.mlworthing.rl.environments.BoardEnvironment
import com.github.mlworthing.rl.utils.{S0FGXFormat, UpRightDownLeft}

object SuperMario extends BoardEnvironment[Int, String] with UpRightDownLeft with S0FGXFormat {

  override lazy val layout: String =
    s"""
       |0 0 0 G
       |0 X 0 F
       |S 0 0 0
     """.stripMargin

  override lazy val actionMoves: Map[String, ActionMoves] = Map(
    "↑" -> (Up, 0.8, Map(Right   -> 0.1, Left -> 0.1)),
    "→" -> (Right, 0.8, Map(Up   -> 0.1, Down -> 0.1)),
    "↓" -> (Down, 0.8, Map(Right -> 0.1, Left -> 0.1)),
    "←" -> (Left, 0.8, Map(Up    -> 0.1, Down -> 0.1))
  )

  override def stateAt(row: Int, col: Int): Int = row * 4 + col
  override def positionOf(state: Int): (Int, Int) = (state / 4, state % 4)
}
