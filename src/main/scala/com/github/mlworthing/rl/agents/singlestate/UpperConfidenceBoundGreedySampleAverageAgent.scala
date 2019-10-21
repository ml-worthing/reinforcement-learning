package com.github.mlworthing.rl.agents.singlestate

import com.github.mlworthing.rl.environments.SingleStateEnvironment
import com.github.mlworthing.rl.{Agent, Winner}

import scala.collection.mutable

/**
  * Exploitation-exploration balanced Agent suitable for stationary problems.
  * Uses upper-confidence-bound action selection method.
  */
case class UpperConfidenceBoundGreedySampleAverageAgent[A](
  explorationFactor: Double = 0.2,
  stepsToLearn: Int = 100,
  initialValue: Double = 0d)
    extends Agent[Unit, A, SingleStateEnvironment[A]] {

  override def solve(environment: SingleStateEnvironment[A]): Winner[A] = {

    val actions: Seq[A] = environment.actions.toIndexedSeq

    // q - estimates of the action values
    val q: mutable.Map[A, Double] = mutable.Map(actions.map(x => (x, initialValue)): _*)
    val steps: mutable.Map[A, Int] = mutable.Map(actions.map(x => (x, 0)): _*)

    (1 to stepsToLearn).foreach { t =>
      val action: A = q
        .map {
          case (a, qa) =>
            if (steps(a) == 0) (a, Double.MaxValue)
            else (a, qa + explorationFactor * Math.sqrt(Math.log(t) / steps(a)))
        }
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
