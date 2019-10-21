package com.github.mlworthing.rl.agents.singlestate

import com.github.mlworthing.rl.environments.SingleStateEnvironment
import com.github.mlworthing.rl.{Agent, Winner}

import scala.collection.mutable

/**
  * Exploration-only Agent suitable for stationary problems.
  * Uses greedy action selection method.
  */
case class GreedySampleAverageAgent[A](stepsToLearn: Int = 100, initialValue: Double = 0d)
    extends Agent[Unit, A, SingleStateEnvironment[A]] {

  override def solve(environment: SingleStateEnvironment[A]): Winner[A] = {

    val actions: Seq[A] = environment.actions.toIndexedSeq

    // q - estimates of the action values
    val q: mutable.Map[A, Double] = mutable.Map(actions.map(x => (x, initialValue)): _*)
    val steps: mutable.Map[A, Int] = mutable.Map(actions.map(x => (x, 0)): _*)

    (1 to stepsToLearn).foreach { _ =>
      val action: A = q
        .maxBy(_._2)
        ._1

      val reward = environment.reward(action)

      steps(action) = steps(action) + 1
      // NewEstimate = OldEstimate + 1/Step * (Target  - OldEstimate)
      q(action) = q(action) + ((reward - q(action)) / steps(action))
    }

    Winner(q.maxBy(_._2)._1)
  }
}
