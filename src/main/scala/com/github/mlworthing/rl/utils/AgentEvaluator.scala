package com.github.mlworthing.rl
package utils

/**
  * Evaluates success rate as a function of agent's configuration parameter.
  *
  * @param expected - the best solution
  * @param agent - agent creator
  * @param configurations - configurations to evaluate
  */
case class AgentEvaluator[State, Action, Config](
  expected: Seq[Action],
  agent: Config => Agent[State, Action],
  configurations: Iterable[Config]) {

  def evaluate(
    environment: => Environment[State, Action],
    numberOfSamples: Int,
    report: Boolean = true): Map[Config, Double] = {
    val stats = configurations.map { config =>
      val successful = (0 until numberOfSamples)
        .count(_ => agent(config).solve(environment) == expected)
      val successRate = (successful * 100d) / numberOfSamples
      (config, successRate)
    }
    if (report) {
      println()
      println(s"Agent success rates after")
      println(s"$numberOfSamples evaluations of each config value:")
      println()
      println("+------------------+")
      println("| config    | rate |")
      println("+-----------+------+")
      println(stats.map { case (config, rate) => f"| $config%-10s|$rate%4.0f%% |" }.mkString("\n"))
      println("+-----------+------+")
    }
    stats.toMap
  }
}
