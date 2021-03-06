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

package com.github.mlworthing.rl.agents.singlestate

import com.github.mlworthing.rl.environments.SingleStateEnvironment
import com.github.mlworthing.rl.utils.SeqWithMutableWeights
import com.github.mlworthing.rl.{Agent, Winner}

import scala.collection.mutable

/**
  * Exploitation-exploration balanced Agent suitable for any problems.
  * Uses a numerical preference for each action which are determined
  * according to a soft-max distribution.
  * @param alpha step-size parameter
  */
case class StochasticGradientAscentAgent[A](
  alpha: Double = 0.2,
  stepsToLearn: Int = 100,
  initialValue: Double = 0d,
  constantStepSize: Option[Double] = None)
    extends Agent[Unit, A, SingleStateEnvironment[A]] {

  override def solve(environment: SingleStateEnvironment[A]): Winner[A] = {

    val actions: Seq[A] = environment.actions.toIndexedSeq

    // q - estimates of the action values
    val q: mutable.Map[A, Double] = mutable.Map(actions.map(x => (x, initialValue)): _*)
    val steps: mutable.Map[A, Int] = mutable.Map(actions.map(x => (x, 0)): _*)
    val preferences: SeqWithMutableWeights[A] =
      new SeqWithMutableWeights(actions, boost = Math.exp, initialWeight = 0d)

    var baseline: Double = 0d

    (1 to stepsToLearn).foreach { t =>
      val action: A = preferences.selectRandom

      val reward = environment.reward(action)

      val probabilityOf = preferences.probabilities

      actions.foreach { a =>
        val na = if (a == action) {
          preferences(a) + alpha * (reward - baseline) * (1 - probabilityOf(a))
        } else {
          preferences(a) + alpha * (reward - baseline) * probabilityOf(a)
        }
        preferences(a) = na
      }

      baseline = baseline + (reward - baseline) / t

      steps(action) = steps(action) + 1
      val stepSize = constantStepSize.getOrElse(1d / steps(action))
      // NewEstimate = OldEstimate + StepSize * (Target  - OldEstimate)
      q(action) = q(action) + (stepSize * (reward - q(action)))
    }

    Winner(q.maxBy(_._2)._1)
  }
}
