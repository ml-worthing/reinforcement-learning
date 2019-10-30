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

import com.github.mlworthing.rl.Environment

trait Printer {

  def printDeterministicPolicy[State, Action](
    headline: String,
    policy: scala.collection.Map[State, Action],
    environment: Environment[State, Action]): Unit = {
    println(headline)
    println()
    println(
      environment
        .show(policy.get, (_: State, action: Action) => action.toString, cellLength = 1, showForTerminalTiles = false))

    println()
  }

  def printStochasticPolicy[State, Action](
    headline: String,
    policy: scala.collection.Map[State, scala.collection.Map[Action, Double]],
    environment: Environment[State, Action]): Unit = {
    val actionsCount = policy.map(_._2.size).max
    println(headline)
    println()
    println(
      environment
        .show[scala.collection.Map[Action, Double]](
          policy.get,
          (_: State, actions: scala.collection.Map[Action, Double]) => actions.filter(_._2 > 0).keys.mkString(""),
          cellLength = actionsCount,
          showForTerminalTiles = false
        ))

    println()
  }

  def printStateValue[State, Action](
    headline: String,
    stateValue: scala.collection.Map[State, Double],
    environment: Environment[State, Action]): Unit = {
    println(headline)
    println()
    println(
      environment
        .show(stateValue.get, (_: State, d: Double) => f"$d%+2.4f", cellLength = 10, showForTerminalTiles = true))
    println()
  }

}
