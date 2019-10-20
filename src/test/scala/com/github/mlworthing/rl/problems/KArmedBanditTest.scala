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
import com.github.mlworthing.rl.agents.{EpsilonGreedyConstantStepSizeAgent, EpsilonGreedySampleAverageAgent, EpsilonGreedyUnbiasedConstantStepSizeAgent}
import com.github.mlworthing.rl.environments.SingleStateEnvironment
import com.github.mlworthing.rl.utils.AgentExecutor
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}
import sun.management.resources.agent

class KArmedBanditTest extends FreeSpec with Matchers with BeforeAndAfterAll {

  val arms = Map(
    1 -> BanditArm.stationary.gaussian(mean = 0d, range = 10d, deviation = 7d),
    2 -> BanditArm.stationary.uniform(mean = 1d, range = 2d),
    5 -> BanditArm.drifting.gaussian(mean = 2d, range = 4d, deviation = 1.5d, drift = 0.001d),
    7 -> BanditArm.drifting.gaussian(mean = 1d, range = 3d, deviation = 2d, drift = 0.002d),
    9 -> BanditArm.stationary.uniform(mean = -1d, range = 2d)
  )

  class KArmedBanditExecution[C](
    underTest: KArmedBandit[Int],
    executor: AgentExecutor[Unit, Int, C, SingleStateEnvironment[Int]]) {
    val stats = executor.execute(underTest, 100)
    stats.print
    stats.maxRate should be > 99.0d

  }

  "find a solution for K-armed bandit problem using epsilon greedy sample-average agent with variable number of steps" in new KArmedBanditExecution(
    underTest = new KArmedBandit(arms),
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedySampleAverageAgent[Int](0.02, _, initialValue = 0d),
      configurations = Seq(10, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of epsilon greedy sample-average agent (epsilon=0.02)\nwith regard to the number of steps",
      "steps"
    )
  )

  "find a solution for K-armed bandit problem using epsilon greedy sample-average agent with variable exploitation factor (epsilon)" in new KArmedBanditExecution(
    underTest = new KArmedBandit(arms),
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedySampleAverageAgent[Int](_, 1000, initialValue = 0d),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of epsilon greedy sample-average agent (stepsToLearn=1000)\nwith regard to the exploitation factor (epsilon)",
      "epsilon"
    )
  )

  "find a solution for K-armed bandit problem using epsilon greedy constant-step agent with variable number of steps" in new KArmedBanditExecution(
    underTest = new KArmedBandit(arms),
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyConstantStepSizeAgent[Int](0.02, 0.2, _, initialValue = 0d),
      configurations = Seq(10, 50, 100, 200, 350, 500, 1000, 5000),
      "Evaluation of epsilon greedy constant-step agent (epsilon=0.02)\nwith regard to the number of steps",
      "steps"
    )
  )

  "find a solution for K-armed bandit problem using epsilon greedy constant-step agent with variable exploitation factor (epsilon)" in new KArmedBanditExecution(
    underTest = new KArmedBandit(arms),
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyConstantStepSizeAgent[Int](_, 0.2, 1000, initialValue = 0d),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of epsilon greedy constant-step agent (stepsToLearn=1000)\nwith regard to the exploitation factor (epsilon)",
      "epsilon"
    )
  )

  "find a solution for K-armed bandit problem using epsilon greedy constant-step agent with variable step size" in new KArmedBanditExecution(
    underTest = new KArmedBandit(arms),
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyConstantStepSizeAgent[Int](0.02, _, 1000, initialValue = 0d),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of epsilon greedy constant-step agent (stepsToLearn=1000,epsilon=0.02)\nwith regard to the step size",
      "step size"
    )
  )

  "find a solution for K-armed bandit problem using epsilon greedy unbiased constant-step agent with variable step size" in new KArmedBanditExecution(
    underTest = new KArmedBandit(arms),
    executor = AgentExecutor(
      expected = Winner(7),
      agent = EpsilonGreedyUnbiasedConstantStepSizeAgent[Int](0.02, _, 1000, initialValue = 0d),
      configurations = Seq(0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.001),
      "Evaluation of epsilon greedy unbiased constant-step agent (stepsToLearn=1000,epsilon=0.02)\nwith regard to the step size",
      "step size"
    )
  )
}
