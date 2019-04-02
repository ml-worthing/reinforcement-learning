package com.github.mlworthing.rl.utils

/** Result of an Agent evaluation for number of different configurations. */
case class EvaluationResults[Config](rates: Seq[(Config, Double)], numberOfSamples: Int) {

  def maxRate: Double = rates.maxBy(_._2)._2

  def print: Unit = {
    println()
    println(s"Agent success rates after")
    println(s"$numberOfSamples evaluations of each config value:")
    println()
    println("+------------------+")
    println("| config    | rate |")
    println("+-----------+------+")
    println(rates.map { case (config, rate) => f"| $config%-10s|$rate%4.0f%% |" }.mkString("\n"))
    println("+-----------+------+")
  }

}
