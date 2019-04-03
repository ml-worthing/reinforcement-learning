package com.github.mlworthing.rl.bandit

import com.github.mlworthing.rl.{Agent, StationaryEnvironment}

import scala.collection.mutable
import scala.util.Random

case class EGreedyAgent[A](epsilon: Double = 0.01, rate: Double = 0.1, stepsToLearn: Int = 100)
    extends Agent[Unit, A, StationaryEnvironment[A]] {

  override def solve(environment: StationaryEnvironment[A]): Seq[A] = {

    val actions: Seq[A] = environment.actions.toIndexedSeq

    val q: mutable.Map[A, Double] = mutable.Map(actions.map(x => (x, 0d)): _*)

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

      /** NewEstimate = OldEstimate + StepSize * (Target  - OldEstimate) */
      q(action) = q(action) + rate * (reward - q(action))
    }

    Seq(q.maxBy(_._2)._1)
  }
}
