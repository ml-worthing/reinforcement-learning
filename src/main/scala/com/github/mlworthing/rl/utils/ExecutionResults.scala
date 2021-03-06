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

/** Result of an Agent evaluation for a number of different configurations. */
case class ExecutionResults[Config](
  description: String,
  configName: String,
  rates: Seq[(Config, Double, Long)],
  numberOfRuns: Int) {

  lazy val maxRate: Double = rates.maxBy(_._2)._2
  lazy val maxConfig: Config = rates.maxBy(_._2)._1
  lazy val maxTime: Double = rates.maxBy(_._3)._3

  def print: Unit = {
    println()
    println(description)
    println()
    println(s"Agent success rates after $numberOfRuns runs of each config:")
    val separator = "-" * 27
    println(separator)
    println(f"| $configName%-10s| rate | time |")
    println(separator)
    println(
      rates
        .map {
          case (config, rate, time) =>
            f"|${if (rate == maxRate) "*" else " "}$config%-10s|$rate%4.0f%% |${(time / maxTime) * 100d}%4.0f%% |"
        }
        .mkString("\n"))
    println(separator)
  }

}
