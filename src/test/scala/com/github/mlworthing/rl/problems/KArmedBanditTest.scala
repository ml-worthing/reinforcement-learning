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

import com.github.mlworthing.rl.Winner
import com.github.mlworthing.rl.agents.{EpsilonGreedyNonStationaryProblemAgent, EpsilonGreedyStationaryProblemAgent}
import com.github.mlworthing.rl.utils.AgentExecutor
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

  "find a solution for K-armed bandit problem using epsilon greedy stationary agent with variable number of steps" in {

    val kArmedBandit = new KArmedBandit(arms)

    val executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyStationaryProblemAgent[Int](0.02, _),
      configurations = Seq(10, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of epsilon greedy stationary agent (epsilon=0.02)\nwith regard to the number of steps",
      "steps"
    )

    val stats = executor.execute(kArmedBandit, 100)

    stats.print
    stats.maxRate should be > 99.0d
  }

  "find a solution for K-armed bandit problem using epsilon greedy stationary agent with variable exploitation factor (epsilon)" in {

    val kArmedBandit = new KArmedBandit(arms)

    val executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyStationaryProblemAgent[Int](_, 1000),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of epsilon greedy stationary agent (stepsToLearn=1000)\nwith regard to the exploitation factor (epsilon)",
      "epsilon"
    )

    val stats = executor.execute(kArmedBandit, 100)

    stats.print
    stats.maxRate should be > 99.0d
  }

  "find a solution for K-armed bandit problem using epsilon greedy non-stationary agent with variable number of steps" in {

    val kArmedBandit = new KArmedBandit(arms)

    val executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyNonStationaryProblemAgent[Int](0.02, 0.2, _),
      configurations = Seq(10, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of epsilon greedy non-stationary agent (epsilon=0.02)\nwith regard to the number of steps",
      "steps"
    )

    val stats = executor.execute(kArmedBandit, 100)

    stats.print
    stats.maxRate should be > 99.0d
  }

  "find a solution for K-armed bandit problem using epsilon greedy non-stationary agent with variable exploitation factor (epsilon)" in {

    val kArmedBandit = new KArmedBandit(arms)

    val executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyNonStationaryProblemAgent[Int](_, 0.2, 1000),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of epsilon greedy non-stationary agent (stepsToLearn=1000)\nwith regard to the exploitation factor (epsilon)",
      "epsilon"
    )

    val stats = executor.execute(kArmedBandit, 100)

    stats.print
    stats.maxRate should be > 99.0d
  }

  "find a solution for K-armed bandit problem using epsilon greedy non-stationary agent with variable step size" in {

    val kArmedBandit = new KArmedBandit(arms)

    val executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyNonStationaryProblemAgent[Int](0.02, _, 1000),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of epsilon greedy non-stationary agent (stepsToLearn=1000,epsilon=0.02)\nwith regard to the step size",
      "step size"
    )

    val stats = executor.execute(kArmedBandit, 100)

    stats.print
    stats.maxRate should be > 99.0d
  }
}
