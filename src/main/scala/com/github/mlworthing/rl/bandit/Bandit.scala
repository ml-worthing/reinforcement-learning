package com.github.mlworthing.rl
package bandit

import scala.util.Random

/**
  * K-armed bandit problem, so named by analogy to a slot machine,
  * or “one-armed bandit,” except that it has k levers instead of one.
  * Each action selection is like a play of one of the slot machine’s levers,
  * and the rewards are the payoffs for hitting the jackpot.
  * Through repeated action selections you are to maximize your winnings by
  * concentrating your actions on the best levers.
  * @see <https://mitpress.mit.edu/books/reinforcement-learning-second-edition>
  *
  * @param arms - mapping of an arm number to the reward distribution parameters (average, spread)
  */
class Bandit(arms: Map[Int, (Int, Int)]) extends Environment[Unit, Int] {

  val actions = arms.keySet

  /** Bandit returns always the same unit state and static set of actions */
  override def send(action: Option[Int]): (Unit, Double, Set[Int]) =
    ((), reward(action.getOrElse(actions.head)), actions)

  /**
    * Bandit reward is a random number generated for a given arm
    * in the range (average-spread/2, average+spread/2)
    **/
  private def reward(action: Int): Double =
    arms
      .get(action)
      .map {
        case (average, spread) => (average - spread / 2) + Random.nextDouble() * spread
      }
      .getOrElse(throw new Exception(s"Invalid action $action"))

  override def isTerminal(state: Unit): Boolean = false
}
