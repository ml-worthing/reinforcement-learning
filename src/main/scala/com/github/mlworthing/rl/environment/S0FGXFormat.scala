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

package com.github.mlworthing.rl.environment

trait S0FGXFormat {

  def rewardFor(tile: String): Double = tile match {
    case "F" => -1
    case "G" => 1
    case _   => 0
  }

  def isAccessible(tile: String): Boolean = tile != "X"

  def isStart(tile: String): Boolean = tile == "S"

  def isTerminal(tile: String): Boolean = tile == "F" || tile == "G"
}
