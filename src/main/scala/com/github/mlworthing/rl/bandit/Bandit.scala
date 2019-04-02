package com.github.mlworthing.rl
package bandit

import scala.util.Random

class Bandit(arms: Map[Int, (Int, Int)]) extends Environment[Unit, Int] {

  val actions = arms.keySet.toSeq

  override def send(action: Option[Int]): (Unit, Double, Seq[Int]) = {
    ((), reward(action.getOrElse(actions.head)), actions)
  }

  def reward(action: Int): Double = arms.get(action).map {
    case (average, spread) => Random.nextDouble() * spread + average
  }.getOrElse(throw new Exception(s"Invalid action $action"))

  override def isTerminal(state: Unit): Boolean = false
}


