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

import com.github.mlworthing.rl.utils.AgentEvaluator
import org.scalatest.{FreeSpec, Matchers}
import sun.management.resources.agent

class KArmedBanditTest extends FreeSpec with Matchers {

  val arms = Map(
    1 -> (0d, 10d),
    2 -> (1d, 2d),
    5 -> (2d, 4d),
    7 -> (3d, 3d),
    9 -> (-1d, 3d)
  )

  "find a solution for K-armed bandit problem using epsilon greedy stationary agent" in {

    val kArmedBandit = new KArmedBandit(arms)

    val evaluator = AgentEvaluator(
      expected = Winner(7),
      agent = EpsilonGreedyStationaryProblemAgent[Int](0.2, _),
      configurations = Seq(10, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of epsilon greedy stationary agent\nwith regard to the number of steps"
    )

    val stats = evaluator.evaluate(kArmedBandit, 100)

    stats.print
    stats.maxRate should be > 99.0d
  }

  "find a solution for K-armed bandit problem using epsilon greedy non-stationary agent" in {

    val kArmedBandit = new KArmedBandit(arms)

    val evaluator = AgentEvaluator(
      expected = Winner(7),
      agent = EpsilonGreedyNonStationaryProblemAgent[Int](0.2, 0.1, _),
      configurations = Seq(10, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of epsilon greedy non-stationary agent\nwith regard to the number of steps"
    )

    val stats = evaluator.evaluate(kArmedBandit, 100)

    stats.print
    stats.maxRate should be > 99.0d
  }
}
