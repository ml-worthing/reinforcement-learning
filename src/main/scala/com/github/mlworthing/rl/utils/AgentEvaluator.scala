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
  configurations: Iterable[Config]) {

  def evaluate(environment: => E, numberOfSamples: Int): EvaluationResults[Config] = {
    val rates = configurations.map { config =>
      val successful = (0 until numberOfSamples)
        .count(_ => agent(config).solve(environment) == expected)
      val successRate = (successful * 100d) / numberOfSamples
      (config, successRate)
    }
    EvaluationResults(environment.description, rates.toSeq, numberOfSamples)
  }
}
