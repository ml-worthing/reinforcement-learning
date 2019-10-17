package com.github.mlworthing.rl.agents

import com.github.mlworthing.rl.environments.SingleStateEnvironment
import com.github.mlworthing.rl.{Agent, Winner}

import scala.collection.mutable
import scala.util.Random

/**
  * Exploitation-exploration balanced Agent suitable for stationary problems.
  */
case class EpsilonGreedyStationaryProblemAgent[A](epsilon: Double = 0.2, stepsToLearn: Int = 100)
    extends Agent[Unit, A, SingleStateEnvironment[A]] {

  override def solve(environment: SingleStateEnvironment[A]): Winner[A] = {

    val actions: Seq[A] = environment.actions.toIndexedSeq

    // q - estimates of the action values
    val q: mutable.Map[A, Double] = mutable.Map(actions.map(x => (x, 0d)): _*)
    val steps: mutable.Map[A, Int] = mutable.Map(actions.map(x => (x, 0)): _*)

    (0 to stepsToLearn).foreach { _ =>
      val action = {
        val explore = Random.nextDouble() >= epsilon
        if (explore) {
          val randomActionIndex = Random.nextInt(actions.length)
          actions(randomActionIndex)
        } else {
          q.maxBy(_._2)._1
        }
      }

      val reward = environment.reward(action)

      steps(action) = steps(action) + 1
      // NewEstimate = OldEstimate + 1/StepSize * (Target  - OldEstimate)
      q(action) = q(action) + ((reward - q(action)) / steps(action))
    }

    Winner(q.maxBy(_._2)._1)
  }
}
