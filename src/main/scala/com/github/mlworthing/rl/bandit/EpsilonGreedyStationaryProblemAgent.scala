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
