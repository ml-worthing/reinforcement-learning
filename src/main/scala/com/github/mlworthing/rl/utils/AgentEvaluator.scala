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
  * Evaluates success rate as a function of agent's configuration parameter.
  *
  * @param expected - the best solution
  * @param agent - agent creator
  * @param configurations - configurations to evaluate
  */
case class AgentEvaluator[State, Action, Config, E <: Environment[State, Action]](
  expected: Policy[State, Action],
  agent: Config => Agent[State, Action, E],
  configurations: Iterable[Config],
  description: String) {

  def evaluate(environment: => E, numberOfSamples: Int): EvaluationResults[Config] = {
    val rates = configurations.map { config =>
      val successful = (0 until numberOfSamples)
        .count(_ => agent(config).solve(environment) == expected)
      val successRate = (successful * 100d) / numberOfSamples
      (config, successRate)
    }
    EvaluationResults(description + "\n" + environment.description, rates.toSeq, numberOfSamples)
  }
}
