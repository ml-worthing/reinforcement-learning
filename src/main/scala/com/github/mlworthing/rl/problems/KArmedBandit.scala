/*
 * Copyright 2019 ml-worthing
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.mlworthing.rl.problems

import com.github.mlworthing.rl.environments.SingleStateEnvironment
import com.github.mlworthing.rl.utils.GaussianRandom

import scala.util.Random

/**
  * K-armed bandit problem, so named by analogy to a slot machine,
  * or "one-armed bandit" except that it has k levers instead of one.
  * Each action selection is like a play of one of the slot machine’s levers,
  * and the rewards are the payoffs for hitting the jackpot.
  * Through repeated action selections you are to maximize your winnings by
  * concentrating your actions on the best levers.
  * @see <https://mitpress.mit.edu/books/reinforcement-learning-second-edition>
  *
  * @param arms - mapping of an arm number to the reward's gaussian distribution parameters (mean, range, deviation)
  */
class KArmedBandit[A](arms: Map[A, BanditArm]) extends SingleStateEnvironment[A] {

  override val actions: Set[A] = arms.keySet

  /**
    * Bandit reward is a gaussian random number generated
    * for a given arm parameters (mean, range)
    **/
  override def reward(action: A): Double =
    arms
      .get(action)
      .map(_.reward)
      .getOrElse(throw new Exception(s"Invalid action $action"))

  override def description: String =
    s"""The ${arms.size}-armed bandit environment.
       |${arms
         .map { case (k, arm) => f"arm #$k ${arm.description}" }
         .mkString("\n")}""".stripMargin
}

trait BanditArm {
  def reward: Double
  def description: String
}

object BanditArm {

  object stationary {

    def gaussian(mean: Double, deviation: Double, range: Double): GaussianRandomBanditArm =
      GaussianRandomBanditArm(mean, deviation, range)

    def uniform(mean: Double, range: Double): UniformRandomBanditArm =
      UniformRandomBanditArm(mean, range)

    case class GaussianRandomBanditArm(mean: Double, deviation: Double, range: Double) extends BanditArm {
      override def reward: Double =
        GaussianRandom.next(mean, deviation, mean - range / 2, mean + range / 2)

      override val description: String =
        f"stationary gaussian: mean=$mean%4.1f, deviation=$deviation%4.1f, range=$range%4.1f"
    }

    case class UniformRandomBanditArm(mean: Double, range: Double) extends BanditArm {
      override def reward: Double =
        mean + (Random.nextDouble() - 0.5) * range

      override val description: String = f"stationary uniform:  mean=$mean%4.1f, range=$range%4.1f"
    }
  }

  object drifting {

    def gaussian(mean: Double, deviation: Double, range: Double, drift: Double): DriftingGaussianRandomBanditArm =
      DriftingGaussianRandomBanditArm(mean, deviation, range, drift)

    def uniform(mean: Double, range: Double, drift: Double): DriftingUniformRandomBanditArm =
      DriftingUniformRandomBanditArm(mean, range, drift)

    case class DriftingGaussianRandomBanditArm(mean: Double, deviation: Double, range: Double, drift: Double)
        extends BanditArm {

      private var offset = 0d

      override def reward: Double = {
        offset = offset + Random.nextDouble() * drift
        offset + GaussianRandom.next(mean, deviation, mean - range / 2, mean + range / 2)
      }

      override val description: String =
        f"drifting gaussian: mean=$mean%4.1f, deviation=$deviation%4.1f, range=$range%4.1f, drift=$drift%4.1f"
    }

    case class DriftingUniformRandomBanditArm(mean: Double, range: Double, drift: Double) extends BanditArm {

      private var offset = 0d

      override def reward: Double = {
        offset = offset + Random.nextDouble() * drift
        offset + mean + (Random.nextDouble() - 0.5) * range
      }

      override val description: String =
        f"drifting uniform:  mean=$mean%4.1f, range=$range%4.1f, drift=$drift%4.3f"
    }
  }
}
