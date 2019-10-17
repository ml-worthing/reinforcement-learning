package com.github.mlworthing.rl.utils

trait S0FGXFormat {

  def rewardFor(tile: String): Double = tile match {
    case "F" => -1
    case "G" => 1
    case _   => 0
  }

  def isAccessible(tile: String): Boolean = tile != "X"

  def isStart(tile: String): Boolean = tile == "S"

  def isTerminal(tile: String): Boolean = tile == "F" || tile == "G"
}
