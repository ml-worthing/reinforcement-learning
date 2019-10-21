package com.github.mlworthing.rl.agents.singlestate

import com.github.mlworthing.rl.environments.SingleStateEnvironment
import com.github.mlworthing.rl.{Agent, Winner}

import scala.collection.mutable
import scala.util.Random

/**
  * Exploitation-exploration balanced Agent suitable for non-stationary problems.
  * Uses exponential recency-weighted average without initial bias method to estimate the reward value.
  */
case class EpsilonGreedyUnbiasedConstantStepSizeAgent[A](
  epsilon: Double = 0.2,
  constantStepSize: Double = 0.1,
  stepsToLearn: Int = 100,
  initialValue: Double = 0d)
    extends Agent[Unit, A, SingleStateEnvironment[A]] {

  override def solve(environment: SingleStateEnvironment[A]): Winner[A] = {

    val actions: Seq[A] = environment.actions.toIndexedSeq

    // q - estimates of the action values
    val q: mutable.Map[A, Double] = mutable.Map(actions.map(x => (x, initialValue)): _*)
    val traces: mutable.Map[A, Double] = mutable.Map(actions.map(x => (x, 0d)): _*)

    (1 to stepsToLearn).foreach { _ =>
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

      traces(action) = traces(action) + constantStepSize * (1 - traces(action))
      // NewEstimate = OldEstimate + ConstantStepSize/Trace * (Target  - OldEstimate)
      q(action) = q(action) + (constantStepSize / traces(action)) * (reward - q(action))
    }

    Winner(q.maxBy(_._2)._1)
  }
}
