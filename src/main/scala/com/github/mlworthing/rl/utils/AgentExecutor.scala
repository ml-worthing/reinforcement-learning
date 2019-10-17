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

/**
  * Computes success rate as a function of agent's configuration parameter.
  *
  * @param expected - the best solution
  * @param agent - agent creator
  * @param configurations - configurations to execute
  */
case class AgentExecutor[State, Action, Config, E <: Environment[State, Action]](
  expected: Policy[State, Action],
  agent: Config => Agent[State, Action, E],
  configurations: Iterable[Config],
  description: String,
  configName: String) {

  def execute(environment: => E, numberOfRuns: Int): ExecutionResults[Config] = {
    val rates = configurations.map { config =>
      val results = for (_ <- 0 until numberOfRuns) yield {
        val t0 = System.nanoTime()
        val result = agent(config).solve(environment)
        val t1 = System.nanoTime()
        (result, t1 - t0)
      }

      val successful = results.count(_._1 == expected)
      val successRate = (successful * 100d) / numberOfRuns
      val totalTime = results.map(_._2).sum
      (config, successRate, totalTime)
    }
    ExecutionResults(description + "\n\n" + environment.description, configName, rates.toSeq, numberOfRuns)
  }
}
