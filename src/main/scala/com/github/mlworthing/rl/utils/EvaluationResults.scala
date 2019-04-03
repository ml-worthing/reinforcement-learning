package com.github.mlworthing.rl.utils

/** Result of an Agent evaluation for number of different configurations. */
case class EvaluationResults[Config](description: String, rates: Seq[(Config, Double)], numberOfSamples: Int) {

  lazy val maxRate: Double = rates.maxBy(_._2)._2
  lazy val maxConfig: Config = rates.maxBy(_._2)._1

  def print: Unit = {
    println()
    println(description)
    println()
    println(s"Agent success rates after")
    println(s"$numberOfSamples samples of each config:")
    println()
    println("+------------------+")
    println("| config    | rate |")
    println("+-----------+------+")
    println(
      rates
        .map { case (config, rate) => f"|${if (rate == maxRate) "*" else " "}$config%-10s|$rate%4.0f%% |" }
        .mkString("\n"))
    println("+-----------+------+")
  }

}
