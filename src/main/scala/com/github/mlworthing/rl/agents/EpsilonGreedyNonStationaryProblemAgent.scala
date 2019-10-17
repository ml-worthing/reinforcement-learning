package com.github.mlworthing.rl.agents

import com.github.mlworthing.rl.environments.SingleStateEnvironment
import com.github.mlworthing.rl.{Agent, Winner}

import scala.collection.mutable
import scala.util.Random

/**
  * Exploitation-exploration balanced Agent suitable for non-stationary problems.
  */
case class EpsilonGreedyNonStationaryProblemAgent[A](
  epsilon: Double = 0.2,
  constantStepSize: Double = 0.1,
  stepsToLearn: Int = 100)
    extends Agent[Unit, A, SingleStateEnvironment[A]] {

  override def solve(environment: SingleStateEnvironment[A]): Winner[A] = {

    val actions: Seq[A] = environment.actions.toIndexedSeq

    // q - estimates of the action values
    val q: mutable.Map[A, Double] = mutable.Map(actions.map(x => (x, 0d)): _*)

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

      // NewEstimate = OldEstimate + ConstantStepSize * (Target  - OldEstimate)
      q(action) = q(action) + constantStepSize * (reward - q(action))
    }

    Winner(q.maxBy(_._2)._1)
  }
}
