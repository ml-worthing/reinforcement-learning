package com.github.mlworthing.rl
package bandit

import scala.collection.mutable
import scala.util.Random

/**
  * Exploitation-exploration balanced Agent suitable for stationary problems.
  */
case class EpsilonGreedyStationaryProblemAgent[A](epsilon: Double = 0.2, stepsToLearn: Int = 100)
    extends Agent[Unit, A, StatelessEnvironment[A]] {

  override def solve(environment: StatelessEnvironment[A]): Winner[A] = {

    val actions: Seq[A] = environment.actions.toIndexedSeq

    val q: mutable.Map[A, Double] = mutable.Map(actions.map(x => (x, 0d)): _*)
    val steps: mutable.Map[A, Int] = mutable.Map(actions.map(x => (x, 0)): _*)

    (0 to stepsToLearn).foreach { _ =>
      val action = {
        val explore = Random.nextDouble() < epsilon
        if (explore) {
          val randomActionIndex = Random.nextInt(actions.length)
          actions(randomActionIndex)
        } else {
          q.maxBy(_._2)._1
        }
      }

      val reward = environment.reward(action)

      /** NewEstimate = OldEstimate + 1/StepSize * (Target  - OldEstimate) */
      steps(action) = steps(action) + 1
      q(action) = q(action) + ((reward - q(action)) / steps(action))
    }

    Winner(q.maxBy(_._2)._1)
  }
}
