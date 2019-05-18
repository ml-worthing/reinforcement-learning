package com.github.mlworthing.rl.utils

import com.github.mlworthing.rl.Environment

trait Printer {

  def printPolicy[State, Action](
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
