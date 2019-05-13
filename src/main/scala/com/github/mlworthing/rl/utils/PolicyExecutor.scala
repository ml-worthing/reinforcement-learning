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

import com.github.mlworthing.rl.{Environment, Policy}

/**
  * Calculates stats of rewards gained by executing the policy in a stochastic environment.
  */
object PolicyExecutor {

  def execute[State, Action](
    policy: Policy[State, Action],
    environment: Environment[State, Action],
    maxIterations: Int,
    numberOfSamples: Int): Map[Double, Int] = {

    val stats = (0 until numberOfSamples)
      .map { _ =>
        policy.execute(environment, maxIterations)
      }
      .groupBy(identity)
      .mapValues(_.size)

    println(s"Rewards gained by executing policy $numberOfSamples times: ${stats.toSeq
      .map { case (k, v) => s"$k -> ${(v * 100) / numberOfSamples}%" }
      .mkString(", ")}")

    stats
  }

}
