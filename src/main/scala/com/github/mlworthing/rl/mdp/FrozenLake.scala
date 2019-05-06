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
package mdp

import utils.{BoardEnvironment, S0FGXFormat, UpRightDownLeft}

/**
  * The Frozen Lake environment as described in the book
  * <https://www.manning.com/books/grokking-deep-reinforcement-learning>
  *
  * The 4x4 board consists of 16 tiles having 4 ice holes rewarding -1
  * and one terminal state rewarding 1. The probability of moving on ice in the selected
  * direction should be less than 1.
  */
case class FrozenLake(gamma: Double = 0.9) extends BoardEnvironment[Int, String] with UpRightDownLeft with S0FGXFormat {

  override lazy val layout: String =
    s"""
       |S 0 0 0
       |0 F 0 F
       |0 0 0 F
       |F 0 0 G
     """.stripMargin

  override lazy val actionMoves: Map[String, ActionMoves] = Map(
    "N" -> (Up, 0.34, Map(Right   -> 0.33, Left -> 0.33)),
    "E" -> (Right, 0.34, Map(Up   -> 0.33, Down -> 0.33)),
    "S" -> (Down, 0.34, Map(Right -> 0.33, Left -> 0.33)),
    "W" -> (Left, 0.34, Map(Up    -> 0.33, Down -> 0.33))
  )

  override def stateAt(row: Int, col: Int): Int = row * 4 + col
}
