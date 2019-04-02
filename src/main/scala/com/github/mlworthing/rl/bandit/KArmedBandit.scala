package com.github.mlworthing.rl
package bandit

import utils.GaussianRandom

/**
  * K-armed bandit problem, so named by analogy to a slot machine,
  * or "one-armed bandit" except that it has k levers instead of one.
  * Each action selection is like a play of one of the slot machine’s levers,
  * and the rewards are the payoffs for hitting the jackpot.
  * Through repeated action selections you are to maximize your winnings by
  * concentrating your actions on the best levers.
  * @see <https://mitpress.mit.edu/books/reinforcement-learning-second-edition>
  *
  * @param arms - mapping of an arm number to the reward's gaussian distribution parameters (mean, range)
  */
class KArmedBandit(arms: Map[Int, (Double, Double)]) extends Environment[Unit, Int] {

  val actions = arms.keySet

  /** Bandit returns always the same unit state and static set of actions */
  override def send(action: Option[Int]): (Unit, Double, Set[Int]) =
    ((), reward(action.getOrElse(actions.head)), actions)

  /**
    * Bandit reward is a normal random number generated for a given arm
    * with the parameters (average, deviation)
    **/
  private def reward(action: Int): Double =
    arms
      .get(action)
      .map {
        case (mean, range) =>
          GaussianRandom.next(mean, range / 7d, mean - range / 2, mean + range / 2)
      }
      .getOrElse(throw new Exception(s"Invalid action $action"))

  override def isTerminal(state: Unit): Boolean = false
}